package naxriscv

import spinal.core._
import naxriscv.compatibility._
import naxriscv.frontend._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv.execute._
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.prediction._
import naxriscv.utilities._
import spinal.lib.LatencyAnalysis
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.eda.bench.Rtl
import spinal.lib.misc.WishboneClint
import spinal.lib.misc.plic.WishbonePlic

import scala.collection.mutable.ArrayBuffer
import scala.sys.exit


class NaxRiscv(xlen : Int,
               plugins : Seq[Plugin]) extends Component{
  NaxScope.create(xlen = xlen)
  val framework = new Framework(plugins)
}

object Config{
  def plugins(resetVector : BigInt = 0x80000000l,
              withRdTime : Boolean = true,
              ioRange    : UInt => Bool = _(31 downto 28) === 0x1,
              fetchRange : UInt => Bool = _(31 downto 28) =/= 0x1): ArrayBuffer[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
//    plugins += new StaticAddressTranslationPlugin(
//      ioRange = _(31 downto 28) === 0x1
//    )
    plugins += new MmuPlugin(
      spec    = MmuSpec.sv32,
      ioRange = ioRange,
      fetchRange = fetchRange,
      physicalWidth = 32
    )

    //FETCH
    plugins += new FetchPlugin()
    plugins += new PcPlugin(resetVector)
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      fetchDataWidth = 64,
      memDataWidth = 64,
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
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
      //      translationStorageParameter = null,
      //      translationPortParameter  = StaticAddressTranslationParameter(rspAt = 1),
    )
    plugins += new AlignerPlugin(
      decodeCount = 2,
      inputAt = 2
    )

    //FRONTEND
    plugins += new FrontendPlugin()
    plugins += new DecompressorPlugin(enabled = false)
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
        hitsAt = 0,
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
      robSize = 64,
      completionWithReg = false
    )
    plugins += new CommitPlugin(
      commitCount = 2,
      ptrCommitRetimed = true
    )
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1
    )
    plugins += new CommitDebugFilterPlugin(List(4, 8, 12))
    plugins += new CsrRamPlugin()
    plugins += new PrivilegedPlugin(PrivilegedConfig.full.copy(withRdTime = withRdTime))
    plugins += new PerformanceCounterPlugin(
      additionalCounterCount = 4,
      bufferWidth            = 6
    )

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("EU0")
    plugins += new IntFormatPlugin("EU0")
    plugins += new SrcPlugin("EU0", earlySrc = true)
    plugins += new IntAluPlugin("EU0", aluStage = 0)
    plugins += new ShiftPlugin("EU0" , aluStage = 0)
    plugins += new BranchPlugin("EU0")
//    plugins += new LoadPlugin("EU0")
//    plugins += new StorePlugin("EU0")

    plugins += new ExecutionUnitBase("EU1", writebackCountMax = 1, readPhysRsFromQueue = true)
    plugins += new IntFormatPlugin("EU1")
    plugins += new SrcPlugin("EU1", earlySrc = true)
    plugins += new MulPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU1", writebackAt = 2)
//    plugins += new IntAluPlugin("EU1")
//    plugins += new ShiftPlugin("EU1")
//    plugins += new BranchPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new LoadPlugin("EU1")
    plugins += new StorePlugin("EU1")
    plugins += new EnvCallPlugin("EU1")(rescheduleAt = 2)
    plugins += new CsrAccessPlugin("EU1")(
      writebackAt = 2
    )

//    plugins += new ExecutionUnitBase("EU2", writebackCountMax = 0)
//    plugins += new SrcPlugin("EU2")
//    plugins += new LoadPlugin("EU2")
//
//
//    plugins += new ExecutionUnitBase("EU3", writebackCountMax = 0)
//    plugins += new SrcPlugin("EU3")
//    plugins += new StorePlugin("EU3")

//    plugins += new ExecutionUnitBase("EU2")
//    plugins += new MulPlugin("EU2", staticLatency = false)
//    plugins += new DivPlugin("EU2", staticLatency = false)
//    plugins += new SrcPlugin("EU2")
//    plugins += new IntAluPlugin("EU2")
//    plugins += new ShiftPlugin("EU2")
//    plugins += new BranchPlugin("EU2")
//    plugins += new LoadPlugin("EU2")
//    plugins += new StorePlugin("EU2")

    plugins += new ExecutionUnitBase("EU4")
    plugins += new IntFormatPlugin("EU4")
    plugins += new SrcPlugin("EU4")
    plugins += new IntAluPlugin("EU4")
    plugins += new ShiftPlugin("EU4")
    plugins += new BranchPlugin("EU4")


//
//    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 0)
//    plugins += new SrcPlugin("EU5")
//    plugins += new StorePlugin("EU5")

//    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 1)
//    plugins += new MulPlugin("EU5", writebackAt = 2, staticLatency = false)
//    plugins += new DivPlugin("EU5", writebackAt = 2)
//    plugins += new EnvCallPlugin("EU5")(rescheduleAt = 2)
//    plugins += new CsrAccessPlugin("EU5")(
//      decodeAt = 0,
//      readAt = 1,
//      writeAt = 2,
//      writebackAt = 2,
//      staticLatency = false
//    )

    plugins
  }
}

// ramstyle = "MLAB, no_rw_check"
object Gen extends App{
  LutInputs.set(6)
  def plugins = {
    Config.plugins(withRdTime = false)
  }

  {
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    //  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

    val report = spinalConfig.generateVerilog(new NaxRiscv(xlen = 32, plugins))
    val doc = report.toplevel.framework.getService[DocPlugin]
    doc.genC()

    val nax = report.toplevel
    val dcache = nax.framework.getService[DataCachePlugin].logic.cache
  }

  {
    def wrapper[T <: Component](c: T) = {
      c.afterElaboration(Rtl.ffIo(c)); c
    }
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
    spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

    spinalConfig.generateVerilog(wrapper(new NaxRiscv(xlen = 32, plugins).setDefinitionName("NaxRiscvSynt")))
  }
}



object Config64{
  def plugins(resetVector : BigInt = 0x80000000l,
              withRdTime : Boolean = true,
              ioRange    : UInt => Bool = _(31 downto 28) === 0x1,
              fetchRange : UInt => Bool = _(31 downto 28) =/= 0x1): ArrayBuffer[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
//    plugins += new StaticAddressTranslationPlugin(
//      ioRange = _(31 downto 28) === 0x1,
//      fetchRange = fetchRange
//    )
    plugins += new MmuPlugin(
      spec    = MmuSpec.sv39,
      ioRange = ioRange,
      fetchRange = fetchRange,
      physicalWidth = 32
    )

    //FETCH
    plugins += new FetchPlugin()
    plugins += new PcPlugin(resetVector)
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      fetchDataWidth = 64,
      memDataWidth = 64,
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
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
//      translationPortParameter  = StaticAddressTranslationParameter(rspAt = 1)
    )
    plugins += new AlignerPlugin(
      decodeCount = 2,
      inputAt = 2
    )

    //FRONTEND
    plugins += new FrontendPlugin()
    plugins += new DecompressorPlugin(enabled = false)
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
        hitsAt = 0,
        ctrlAt = 1,
        rspAt  = 1
      ),
      storeTranslationParameter = MmuPortParameter(
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
//        loadTranslationParameter  = StaticAddressTranslationParameter(rspAt = 1),
//        storeTranslationParameter = StaticAddressTranslationParameter(rspAt = 1)
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
      robSize = 64,
      completionWithReg = false
    )
    plugins += new CommitPlugin(
      commitCount = 2,
      ptrCommitRetimed = true
    )
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1
    )
    plugins += new CommitDebugFilterPlugin(List(4, 8, 12))
    plugins += new CsrRamPlugin()
    plugins += new PrivilegedPlugin(PrivilegedConfig.full.copy(withRdTime = withRdTime))
    plugins += new PerformanceCounterPlugin(
      additionalCounterCount = 4,
      bufferWidth            = 6
    )

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("EU0")
    plugins += new IntFormatPlugin("EU0")
    plugins += new SrcPlugin("EU0", earlySrc = true)
    plugins += new IntAluPlugin("EU0", aluStage = 0)
    plugins += new ShiftPlugin("EU0" , aluStage = 0)
    plugins += new BranchPlugin("EU0")
    //    plugins += new LoadPlugin("EU0")
    //    plugins += new StorePlugin("EU0")

    plugins += new ExecutionUnitBase("EU1", writebackCountMax = 1, readPhysRsFromQueue = true)
    plugins += new IntFormatPlugin("EU1")
    plugins += new SrcPlugin("EU1", earlySrc = true)
    plugins += new MulPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU1", writebackAt = 2)
    //    plugins += new IntAluPlugin("EU1")
    //    plugins += new ShiftPlugin("EU1")
    //    plugins += new BranchPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new LoadPlugin("EU1")
    plugins += new StorePlugin("EU1")
    plugins += new EnvCallPlugin("EU1")(rescheduleAt = 2)
    plugins += new CsrAccessPlugin("EU1")(
      writebackAt = 2
    )

    //    plugins += new ExecutionUnitBase("EU2", writebackCountMax = 0)
    //    plugins += new SrcPlugin("EU2")
    //    plugins += new LoadPlugin("EU2")
    //
    //
    //    plugins += new ExecutionUnitBase("EU3", writebackCountMax = 0)
    //    plugins += new SrcPlugin("EU3")
    //    plugins += new StorePlugin("EU3")

    //    plugins += new ExecutionUnitBase("EU2")
    //    plugins += new MulPlugin("EU2", staticLatency = false)
    //    plugins += new DivPlugin("EU2", staticLatency = false)
    //    plugins += new SrcPlugin("EU2")
    //    plugins += new IntAluPlugin("EU2")
    //    plugins += new ShiftPlugin("EU2")
    //    plugins += new BranchPlugin("EU2")
    //    plugins += new LoadPlugin("EU2")
    //    plugins += new StorePlugin("EU2")

    plugins += new ExecutionUnitBase("EU4")
    plugins += new IntFormatPlugin("EU4")
    plugins += new SrcPlugin("EU4")
    plugins += new IntAluPlugin("EU4")
    plugins += new ShiftPlugin("EU4")
    plugins += new BranchPlugin("EU4")


    //
    //    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 0)
    //    plugins += new SrcPlugin("EU5")
    //    plugins += new StorePlugin("EU5")

    //    plugins += new ExecutionUnitBase("EU5", writebackCountMax = 1)
    //    plugins += new MulPlugin("EU5", writebackAt = 2, staticLatency = false)
    //    plugins += new DivPlugin("EU5", writebackAt = 2)
    //    plugins += new EnvCallPlugin("EU5")(rescheduleAt = 2)
    //    plugins += new CsrAccessPlugin("EU5")(
    //      decodeAt = 0,
    //      readAt = 1,
    //      writeAt = 2,
    //      writebackAt = 2,
    //      staticLatency = false
    //    )

    plugins
  }
}

// ramstyle = "MLAB, no_rw_check"
object Gen64 extends App{
  LutInputs.set(6)
  def plugins = {
    val l = Config64.plugins(withRdTime = false)
    l.foreach{
//      case p : ExecutionUnitBase if p.euId == "EU1" => p.readPhysRsFromQueue = true
      case _ =>
    }
//    Tweek.euWritebackAt(l, "EU0", 1)
//    Tweek.euWritebackAt(l, "EU4", 1)
    l
  }

  {
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    //  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

    val report = spinalConfig.generateVerilog(new NaxRiscv(xlen = 64, plugins))
    val doc = report.toplevel.framework.getService[DocPlugin]
    doc.genC()

    val nax = report.toplevel
    val dcache = nax.framework.getService[DataCachePlugin].logic.cache
  }

  {
    def wrapper[T <: Component](c: T) = {
      c.afterElaboration(Rtl.xorOutputs(Rtl.ffIo(c))); c
    }
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
    spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

    spinalConfig.generateVerilog(wrapper(new NaxRiscv(xlen = 64, plugins).setDefinitionName("NaxRiscvSynt")))
  }
}


//CMD
/*
make test-clean output/nax/dhrystone/PASS output/nax/coremark/PASS ARGS="--stats-print --stats-toggle-symbol sim_time"

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
- Each Eu could have its own regfile space
- Optimise SrcPlugin by unifying SRC / ADD muxes
- May use distributed shift register for the issue queue context
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
/*
./configure --prefix=/opt/riscv --with-cmodel=medany --with-multilib-generator="rv32i-ilp32--;rv32im-ilp32--;rv32imac-ilp32--;rv32imafd-ilp32d--;rv32imacfd-ilp32d--;rv64i-lp64--;rv64im-lp64--;rv64imac-lp64--;rv64imafd-lp64d--;rv64imacfd-lp64d--"
*/
//stats

/*
checkLq => 6.8slack 550 LUT (16lq/16sq)
 */

/*
obj_dir/VNaxRiscv --name dhrystone --output-dir output/nax/dhrystone --load-elf ../../../../ext/NaxSoftware/baremetal/dhrystone/build/rv32im/dhrystone.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --stats-print --stats-toggle-symbol sim_time
obj_dir/VNaxRiscv --name coremark --output-dir output/nax/coremark --load-elf ../../../../ext/NaxSoftware/baremetal/coremark/build/rv32im/coremark.elf --start-symbol _start --pass-symbol pass  --stats-print-all --stats-toggle-symbol sim_time
obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/rv32im/play.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --trace --trace-ref --stats-print-all
obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/freertosDemo/integer/rv32im/freertosDemo.elf --start-symbol _start --pass-symbol c_pass --fail-symbol c_fail --stats-print-all


obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/rv64im/play.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --trace --trace-ref --stats-print-all

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


buildroot/run0 =>
STATS :
  IPC               0.564001
  cycles            227596500
  commits           128364701
  reschedules       2737629
  trap              21189
  branch miss       1252921
  jump miss         1228113
  storeToLoadHazard 6208
  loadHitMiss       154496


STATS :
  IPC               0.399107
  cycles            324598890
  commits           129549688
  reschedules       2831840
  trap              25216
  branch miss       1286238
  jump miss         1282142
  storeToLoadHazard 7109
  loadHitMiss       151680

STATS :
  IPC               0.461625
  cycles            281659239
  commits           130020836
  reschedules       2787057
  trap              23349
  branch miss       1270644
  jump miss         1252713
  storeToLoadHazard 7247
  loadHitMiss       155787


Against cortex M4
Benchmark           Speed
---------           -----
aha-mont64           0.89
crc32                0.92
cubic                0.49
edn                  1.80
huffbench            1.62
matmult-int          1.65
md5sum               1.87
minver               0.85
nbody                0.96
nettle-aes           1.10
nettle-sha256        1.10
nsichneu             1.29
picojpeg             1.24
primecount           1.87
qrduino              1.51
sglib-combined       1.10
slre                 1.81
st                   1.21
statemate            2.14
tarfind              1.07
ud                   1.18
wikisort             1.65
---------           -----
Geometric mean       1.26
Geometric SD         1.40
Geometric range      0.87

Against ri5cy
Benchmark           Speed
---------           -----
aha-mont64           1.19
crc32                1.04
cubic                1.04
edn                  1.93
huffbench            1.45
matmult-int          1.63
md5sum               1.87
minver               1.46
nbody                1.29
nettle-aes           1.29
nettle-sha256        1.34
nsichneu             1.17
picojpeg             1.34
primecount           1.87
qrduino              1.27
sglib-combined       0.98
slre                 1.51
st                   1.47
statemate            1.12
tarfind              1.07
ud                   1.48
wikisort             1.25
---------           -----
Geometric mean       1.34
Geometric SD         1.21
Geometric range      0.51


1.63065 + 1.24106 + 1.67444 + 1.28898 + 1.31207 + 1.89032 + 1.27391 + 1.11954 + 1.8445 + 1.5426 + 1.36446 + 1.45095 + 1.35236 + 1.30745 + 1.11382 + 0.723316 + 0.907167 + 1.55149 + 1.1772 + 1.04995 + 1.3341 + 1.07459

 */