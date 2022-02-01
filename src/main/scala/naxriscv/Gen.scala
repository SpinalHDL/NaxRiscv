package naxriscv

import naxriscv.compatibility.{EnforceSyncRamPhase, MemReadDuringWriteHazardPhase, MultiPortReadSymplifier, MultiPortWritesSymplifier}
import spinal.core._
import naxriscv.frontend._
import naxriscv.fetch._
import naxriscv.misc.{CommitDebugFilterPlugin, CommitPlugin, CsrRamPlugin, MmuPlugin, MmuPortParameter, MmuSpec, MmuStorageLevel, MmuStorageParameter, PerformanceCounterPlugin, PrivilegedConfig, PrivilegedPlugin, RegFilePlugin, RobPlugin, StaticAddressTranslationParameter, StaticAddressTranslationPlugin}
import naxriscv.execute._
import naxriscv.fetch.FetchCachePlugin
import naxriscv.lsu.{DataCache, DataCachePlugin, LsuPlugin}
import naxriscv.prediction.{BranchContextPlugin, BtbPlugin, DecoderPredictionPlugin, GSharePlugin, HistoryPlugin}
import naxriscv.utilities._
import spinal.lib.LatencyAnalysis
import spinal.lib.eda.bench.Rtl

import scala.collection.mutable.ArrayBuffer

object Config{
  def properties() = {
    NaxDataBase.create()

    Fetch.RVC.set(false)
    Fetch.FETCH_DATA_WIDTH.set(64)
    Fetch.INSTRUCTION_WIDTH.set(32)
    Frontend.DECODE_COUNT.set(2)
    Global.COMMIT_COUNT.set(2)
    Global.XLEN.set(32)
    ROB.SIZE.set(64)

//    Fetch.RVC.set(false)
//    Fetch.FETCH_DATA_WIDTH.set(32)
//    Fetch.INSTRUCTION_WIDTH.set(32)
//    Frontend.DECODE_COUNT.set(1)
//    Global.COMMIT_COUNT.set(1)
//    Global.XLEN.set(32)
//    ROB.SIZE.set(64)
  }

  def plugins(): Seq[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
//    plugins += new StaticAddressTranslationPlugin(
//      ioRange = _(31 downto 28) === 0x1
//    )
    plugins += new MmuPlugin(
      spec    = MmuSpec.sv32,
      ioRange = _(31 downto 28) === 0x1
    )

    //FETCH
    plugins += new FetchPlugin()
    plugins += new PcPlugin()
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      memDataWidth = Fetch.FETCH_DATA_WIDTH,
      reducedBankWidth = false,
      hitsWithTranslationWays = true,
      translationStorageParameter = MmuStorageParameter(
        levels   = List(
          MmuStorageLevel(
            id    = 0,
            ways  = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id    = 1,
            ways  = 2,
            depth = 32
          )
        ),
        priority = 0
      ),
      translationPortParameter  = MmuPortParameter(
        readAt = 0,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
      //      translationStorageParameter = null,
      //      translationPortParameter  = StaticAddressTranslationParameter(rspAt = 1),
    )
    plugins += new AlignerPlugin(inputAt = 2)

    //FRONTEND
    plugins += new FrontendPlugin()
    plugins += new DecompressorPlugin()
    plugins += new DecoderPlugin()
    plugins += new RfTranslationPlugin()
    plugins += new RfDependencyPlugin()
    plugins += new RfAllocationPlugin(riscv.IntRegFile)
    plugins += new DispatchPlugin(
      slotCount = 32
    )

    //BRANCH PREDICTION
    plugins += new BranchContextPlugin(
      branchCount = 16
    )
    plugins += new HistoryPlugin(
      historyFetchBypass = true
    )
    plugins += new DecoderPredictionPlugin(
//      applyAt = _.pipeline.decoded,
      flushOnBranch = false //TODO remove me (DEBUG)
    )
    plugins += new BtbPlugin(
//      entries = 8192*8,
      entries = 512,
      readAt = 0,
      hitAt = 1,
      jumpAt = 1
    )
    plugins += new GSharePlugin(
//      entries = 1 << 24,
      memBytes = 4 KiB,
      historyWidth = 24,
      readAt = 0
    )

    //LOAD / STORE
    plugins += new LsuPlugin(
      lqSize = 16,
      sqSize = 16,
      loadToCacheBypass = true,
      lqToCachePipelined = true,
      hazardPedictionEntries = 512,
      hazardPredictionTagWidth = 16,
      hitPedictionEntries = 1024,
      translationStorageParameter = MmuStorageParameter(
        levels   = List(
          MmuStorageLevel(
            id    = 0,
            ways  = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id    = 1,
            ways  = 2,
            depth = 32
          )
        ),
        priority = 1
      ),
      loadTranslationParameter  = MmuPortParameter(
        readAt = 0,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      ),
      storeTranslationParameter = MmuPortParameter(
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
//      translationStorageParameter = null,
//      loadTranslationParameter  = StaticAddressTranslationParameter(rspAt = 1),
//      storeTranslationParameter = StaticAddressTranslationParameter(rspAt = 1)
    )
    plugins += new DataCachePlugin(
      memDataWidth = 64,
      cacheSize    = 4096*4,
      wayCount     = 4,
      refillCount = 2,
      writebackCount = 2,
      tagsReadAsync = true,
      reducedBankWidth = false,
//      loadHitAt      = 2
//      loadRspAt      = 3,
      loadRefillCheckEarly = false
    )

    //MISC
    plugins += new RobPlugin(
      completionWithReg = false
    )
    plugins += new CommitPlugin(
      ptrCommitRetimed = true
    )
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1
    )
    plugins += new CommitDebugFilterPlugin(List(4, 8, 12))
    plugins += new CsrRamPlugin()
    plugins += new PrivilegedPlugin(PrivilegedConfig.full)
    plugins += new PerformanceCounterPlugin(
      additionalCounterCount = 4,
      bufferWidth            = 6
    )

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("EU0")
    plugins += new SrcPlugin("EU0", earlySrc = true)
    plugins += new IntAluPlugin("EU0", aluStage = 0)
    plugins += new ShiftPlugin("EU0" , aluStage = 0)
//    plugins += new BranchPlugin("EU0")
//    plugins += new LoadPlugin("EU0")
//    plugins += new StorePlugin("EU0")

    plugins += new ExecutionUnitBase("EU1", writebackCountMax = 1)
    plugins += new SrcPlugin("EU1", earlySrc = true)
    plugins += new MulPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU1", writebackAt = 2)
//    plugins += new IntAluPlugin("EU1")
//    plugins += new ShiftPlugin("EU1")
    plugins += new BranchPlugin("EU1", writebackAt = 2, staticLatency = false)
//    plugins += new LoadPlugin("EU1")
    plugins += new StorePlugin("EU1")
    plugins += new CsrAccessPlugin("EU1")(
      decodeAt = 0,
      readAt = 1,
      writeAt = 2,
      writebackAt = 2,
      staticLatency = false
    )
    plugins += new EnvCallPlugin("EU1")(rescheduleAt = 2)

    plugins += new ExecutionUnitBase("EU2", writebackCountMax = 0)
    plugins += new SrcPlugin("EU2")
    plugins += new LoadPlugin("EU2")

//    plugins += new ExecutionUnitBase("EU2")
//    plugins += new MulPlugin("EU2", staticLatency = false)
//    plugins += new DivPlugin("EU2", staticLatency = false)
//    plugins += new SrcPlugin("EU2")
//    plugins += new IntAluPlugin("EU2")
//    plugins += new ShiftPlugin("EU2")
//    plugins += new BranchPlugin("EU2")
//    plugins += new LoadPlugin("EU2")
//    plugins += new StorePlugin("EU2")

//    plugins += new ExecutionUnitBase("EU3")
//    plugins += new SrcPlugin("EU3")
//    plugins += new IntAluPlugin("EU3")
//    plugins += new ShiftPlugin("EU3")
//    plugins += new BranchPlugin("EU3")


//
//    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 0)
//    plugins += new SrcPlugin("EU5")
//    plugins += new StorePlugin("EU5")

    plugins
  }
}

// ramstyle = "MLAB, no_rw_check"
object Gen extends App{
  LutInputs.set(6)

  {
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    //  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

    val report = spinalConfig.generateVerilog(new Component {
      setDefinitionName("NaxRiscv")
      Config.properties()
      val framework = new Framework(Config.plugins())
    })
    val doc = report.toplevel.framework.getService[DocPlugin]
    doc.genC()

    val nax = report.toplevel
    val dcache = nax.framework.getService[DataCachePlugin].logic.cache
//    println("Miaou : " + LatencyAnalysis(dcache.reflectBaseType("load_pipeline_stages_2_REFILL_HITS_EARLY"), nax.reflectBaseType("CommitPlugin_logic_reschedule_cause")))
  }

  {
    def wrapper[T <: Component](c: T) = {
      c.afterElaboration(Rtl.ffIo(c)); c
    }
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
    spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

    spinalConfig.generateVerilog(wrapper(new Component {
      setDefinitionName("NaxRiscvSynt")
      Config.properties()
      val framework = new Framework(Config.plugins())
    }))
  }
}


//CMD
/*
make clean compile  test_clean output/nax/dhrystone/PASS ARGS="--stats_print_all --stats_toggle_symbol sim_time"

 */
//object GenSim extends App{
//  import spinal.core.sim._
//  SimConfig.withFstWave.compile(new Component {
//    Config.properties()
//    val frontend = new Framework(Config.plugins())
//  }).doSim(seed = 42){}
//}


//ROADMAP
/*
- https://github.com/riscv-non-isa/riscv-arch-test
 */

//TODO Optimisations
/*
- bypass for write to read on btb
- commit.reschedule storage could be used to trigger frontend flush, reducing branch miss penality and combinatorial delay
- Decoder ILLEGAL datapath can be optimized to use INSTRUCTION_ALIGNED instead, with some additional hit from the decompressor for RVC stuff
- DataCache pipeline load refill hits to cut path
- Check that sw -> lw do not trigger a checkLq reschedule
- store to load hazard prediction (unlearning bad entry over time, ...)
- less pessimistic store to load detection (only trigger if the load got the data from the cache, instead of just having its address), else there is also the case where both load/store on a line miss will trigger lqCheck
- RAS implement healing
- gshare should update the fetch branchHistory himself, instead of the decode stage update workaround
- optimize aligner for non-rvc config
- PredoctorPlugin to Branchplugin context storage optimisation
- LSU getting PC for reschedule
- When a reschedule is pending, stop feeding the pipeline with things which will get trashed anyway
- LSU aliasing prediction, and in general reducing the pessiming nature of checkSq/checkLq
- RobPlugin completion vector could be stored as a distributed ram with single bit flip. This shouild save 200 lut
- DataCache banks write sharing per way, instead of allocating them all between refill and store
- Lsu loadFeedAt is currently set to 1 to relax timings on the d$ banks, maybe those banks can use falling edge clock of stage loadFeedAt+1 stage instead ?
 */

//TODO fix bellow list
/*
- IMPORTANT add datacache option to create poison on read during write ram blocks
- refill -> store delay may not be right (refill stay active a extra time after completion to avoid hazard)
- Should i$ d$ check permitions before triggering a refill ? Seems already ok for data cache via the abord signal
- lsu peripheral do not handle rsp.error yet
- lsu load to load with same address ordering in a far future
- Manage the verilator seeds
- having genTests.py generating different seed for each test
- Data cache handle store which had tag hit but the line is currently being written back to the main memory
- Data cache / LSU  need cares about read during writes on tags and data, also, care about refill happening from previous cycle hazarding pipeline
- data cache reduce ram blocks usage clashes by using banks sel
- aligner flush connector from fetches.last stage (workarounded via a extra stage)
- Likely pc management in the aligner need rework, specialy what's about btb impact when buffer pc + 4 != input pc ?
- load to load ordering
- Check lsu memory depedency cross check (store and load with aliasing checking others at the same time)
- data cache flush (init, and others) do not halt incoming requests (should at least schedule a redo)
 */

//ASSUMPTIONS
/*
X0 init =>
- RfTranslationPlugin entries are initialized to all point to physical 0
- RfAllocationPlugin will reject physical 0 freedoom
- RfAllocationPlugin will not init physical 0
- RegFilePlugin will write physical 0 with 0

 */


//stats

/*
checkLq => 6.8slack 550 LUT (16lq/16sq)
 */

/*
obj_dir/VNaxRiscv --name dhrystone --output-dir output/nax/dhrystone --load-elf ../../../../ext/NaxSoftware/baremetal/dhrystone/build/dhrystone.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --stats-print --stats-toggle-symbol sim_time
obj_dir/VNaxRiscv --name coremark --output-dir output/nax/coremark --load-elf /media/data/open/riscv/coremark/build/coremark_rv32im.elf --start-symbol _start --pass-symbol pass  --stats-print-all --stats-toggle-symbol sim_time
obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/rv32im/play.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --trace --trace-ref --stats-print-all
obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/freertosDemo/integer/rv32im/freertosDemo.elf --start-symbol _start --pass-symbol c_pass --fail-symbol c_fail --stats-print-all


LAST PC COMMIT=c002be98

ARGS_COMMON="--load-elf /media/data/open/riscv/VexRiscvOoo/ext/NaxSoftware/baremetal/play/build/play.elf"

LINUX_IMAGES=/media/data/open/riscv/buildroot/output/images
ARGS_COMMON="--load-bin $LINUX_IMAGES/fw_jump.bin,0x80000000 \
             --load-bin $LINUX_IMAGES/linux.dtb,0x80F80000 \
             --load-bin $LINUX_IMAGES/Image,0x80400000  \
             --load-bin $LINUX_IMAGES/rootfs.cpio,0x81000000"
ARGS_MASTER="--sim-master"
ARGS_SLAVE="--sim-slave --trace"

make compile
./obj_dir/VNaxRiscv $ARGS_COMMON $ARGS_MASTER
./obj_dir/VNaxRiscv $ARGS_COMMON $ARGS_SLAVE


./obj_dir/VNaxRiscv \
    --load-bin $LINUX_IMAGES/fw_jump.bin,0x80000000 \
    --load-bin $LINUX_IMAGES/linux.dtb,0x80F80000 \
    --load-bin $LINUX_IMAGES/Image,0x80400000 \
    --load-bin $LINUX_IMAGES/rootfs.cpio,0x81000000 \
    --getc "buildroot login" \
    --putc "root" \
    --getc "root@buildroot:~#" \
    --putc "cat /proc/cpuinfo" \
    --getc "root@buildroot:~#" \
    --putc "echo 1+2+3*4 | bc" \
    --getc "root@buildroot:~#" \
    --putc "micropython" \
    --getc ">>> " \
    --putc "import math" \
    --getc ">>> " \
    --putc "math.sin(math.pi/4)" \
    --getc ">>> " \
    --putc "from sys import exit" \
    --getc ">>> " \
    --putc "exit()" \
    --getc "root@buildroot:~#" \
    --putc "ls /" \
    --getc "root@buildroot:~#" \
    --success
 */