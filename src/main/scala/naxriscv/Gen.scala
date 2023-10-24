// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv

import spinal.core._
import naxriscv.compatibility._
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.frontend._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv.execute._
import naxriscv.execute.fpu._
import naxriscv.fetch._
import naxriscv.interfaces.CommitService
import naxriscv.lsu._
import naxriscv.lsu2.Lsu2Plugin
import naxriscv.prediction._
import naxriscv.riscv.IntRegFile
import naxriscv.utilities._
import spinal.lib.{LatencyAnalysis, Timeout}
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.cpu.riscv.debug.DebugTransportModuleParameter
import spinal.lib.eda.bench.Rtl
import spinal.lib.misc.WishboneClint
import spinal.lib.misc.plic.WishbonePlic

import scala.collection.mutable.ArrayBuffer
import scala.sys.exit


class NaxRiscv(val plugins : Seq[Plugin]) extends Component{
  val database = new DataBase
  val framework = NaxScope(database) on new Framework(plugins) //Will run the generation asynchronously
}

object Config{
  def plugins(resetVector : BigInt = 0x80000000l,
              withRdTime : Boolean = true,
              ioRange    : UInt => Bool = _(31 downto 28) === 0x1,
              fetchRange : UInt => Bool = _(31 downto 28) =/= 0x1,
              aluCount : Int = 2,
              decodeCount : Int = 2,
              withRvc : Boolean = false,
              withMmu : Boolean = true,
              withPerfCounters : Boolean = true,
              withSupervisor : Boolean = true,
              withDistributedRam : Boolean = true,
              xlen : Int = 32,
              withLoadStore : Boolean = true,
              withDedicatedLoadAgu : Boolean = false,
              withDebug : Boolean = false,
              withEmbeddedJtagTap : Boolean = false,
              withEmbeddedJtagInstruction : Boolean = false,
              jtagTunneled : Boolean = false,
              debugTriggers : Int = 0,
              branchCount : Int = 16,
              withFloat  : Boolean = false,
              withDouble : Boolean = false,
              withLsu2 : Boolean = true,
              lqSize : Int = 16,
              sqSize : Int = 16,
              simulation : Boolean = GenerationFlags.simulation,
              sideChannels : Boolean = false,
              dispatchSlots : Int = 32,
              robSize : Int = 64,
              withCoherency : Boolean = false,
              hartId : Int = 0): ArrayBuffer[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()

    val fpu = withFloat || withDouble

    plugins += new DocPlugin()
    plugins += (withMmu match {
      case false => new StaticAddressTranslationPlugin(
        ioRange = ioRange,
        fetchRange = fetchRange,
        physicalWidth = 32
      )
      case true => new MmuPlugin(
        spec    = if(xlen == 32) MmuSpec.sv32 else MmuSpec.sv39,
        ioRange = ioRange,
        fetchRange = fetchRange,
        physicalWidth = 32
      )
    })
    if(withEmbeddedJtagTap || withEmbeddedJtagInstruction) plugins += new EmbeddedJtagPlugin(
      p = DebugTransportModuleParameter(
        addressWidth = 7,
        version      = 1,
        idle         = 7
      ),
      withTunneling = jtagTunneled,
      withTap = withEmbeddedJtagTap
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
      tagsReadAsync = withDistributedRam,
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
      translationPortParameter = withMmu match {
        case false => StaticAddressTranslationParameter(rspAt = 1)
        case true => MmuPortParameter(
          readAt = 1,
          hitsAt = 1,
          ctrlAt = 1,
          rspAt = 1
        )
      }
    )
    plugins += new AlignerPlugin(
      decodeCount = decodeCount,
      inputAt = 2
    )

    //FRONTEND
    plugins += new FrontendPlugin()
    plugins += new DecompressorPlugin(
      enabled = withRvc,
      pipelined = withRvc
    )
    plugins += new DecoderPlugin(xlen)
    plugins += new RfTranslationPlugin(riscv.IntRegFile)
    plugins += new RfDependencyPlugin()
    plugins += new RfAllocationPlugin(riscv.IntRegFile)
    plugins += new DispatchPlugin(
      slotCount = dispatchSlots,
      robIdAt = withDistributedRam.toInt //Not having it enabled allows ram block inferation on execution unit context reads
    )

    //BRANCH PREDICTION
    plugins += new BranchContextPlugin(
      branchCount = branchCount
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
    if(withLoadStore){
      withLsu2 match {
        case false => plugins += new LsuPlugin(
          lqSize = lqSize,
          sqSize = sqSize,
          loadToCacheBypass = true,
          lqToCachePipelined = true,
          hitPedictionEntries = 1024,
          loadWriteRfOnPrivilegeFail = sideChannels,
          translationStorageParameter = MmuStorageParameter(
            levels = List(
              MmuStorageLevel(
                id = 0,
                ways = 4,
                depth = 32
              ),
              MmuStorageLevel(
                id = 1,
                ways = 2,
                depth = 32
              )
            ),
            priority = 1
          ),

          loadTranslationParameter = withMmu match {
            case false => StaticAddressTranslationParameter(rspAt = 1)
            case true => MmuPortParameter(
              readAt = 0,
              hitsAt = 0,
              ctrlAt = 1,
              rspAt = 1
            )
          },
          storeTranslationParameter = withMmu match {
            case false => StaticAddressTranslationParameter(rspAt = 1)
            case true => MmuPortParameter(
              readAt = 1,
              hitsAt = 1,
              ctrlAt = 1,
              rspAt = 1
            )
          }
        )
        case true => plugins += new Lsu2Plugin(
          lqSize = lqSize,
          sqSize = sqSize,
//          loadToCacheBypass = true,
          lqToCachePipelined = true,
//          hitPedictionEntries = 1024,
          loadWriteRfOnPrivilegeFail = sideChannels,
          translationStorageParameter = MmuStorageParameter(
            levels = List(
              MmuStorageLevel(
                id = 0,
                ways = 4,
                depth = 32
              ),
              MmuStorageLevel(
                id = 1,
                ways = 2,
                depth = 32
              )
            ),
            priority = 1
          ),

          sharedTranslationParameter = withMmu match {
            case false => StaticAddressTranslationParameter(rspAt = 1)
            case true => MmuPortParameter(
              readAt = 0,
              hitsAt = 0,
              ctrlAt = 1,
              rspAt  = 1
            )
          }
        )
      }

      plugins += new DataCachePlugin(
        memDataWidth = 64,
        cacheSize = 4096 * 4,
        wayCount = 4,
        refillCount = 2,
        writebackCount = 2,
        tagsReadAsync = withDistributedRam,
        loadReadTagsAt = if (withDistributedRam) 1 else 0,
        storeReadTagsAt = if (withDistributedRam) 1 else 0,
        reducedBankWidth = false,
        //      loadHitAt      = 2
        //      loadRspAt      = 3,
        loadRefillCheckEarly = false,
        withCoherency = withCoherency,
        probeIdWidth = if (withCoherency) 4 else 0,
        ackIdWidth = if (withCoherency) 4 else 0
      )
    }





    //MISC
    plugins += new RobPlugin(
      robSize = robSize,
      completionWithReg = !withDistributedRam
    )
    plugins += new CommitPlugin(
      commitCount = decodeCount,
      ptrCommitRetimed = true
    )
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1,
      preferedWritePortForInit = "ALU0"
    )
    plugins += new CommitDebugFilterPlugin(List(4, 8, 12))
    plugins += new CsrRamPlugin()
    plugins += new PrivilegedPlugin(PrivilegedConfig.full.copy(
      withRdTime = withRdTime,
      withSupervisor = withSupervisor,
      withDebug = withDebug,
      debugTriggers = debugTriggers,
      hartId = hartId
    ))
    if(withPerfCounters) plugins += new PerformanceCounterPlugin(
      additionalCounterCount = 4,
      bufferWidth            = 6
    )

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("ALU0")
    plugins += new IntFormatPlugin("ALU0")
    plugins += new SrcPlugin("ALU0")
    plugins += new IntAluPlugin("ALU0", aluStage = 0)
    plugins += new ShiftPlugin("ALU0" , aluStage = 0)
    if(aluCount > 1) plugins += new BranchPlugin("ALU0")


    plugins += new ExecutionUnitBase("EU0", writebackCountMax = 1, readPhysRsFromQueue = true)
    plugins += new IntFormatPlugin("EU0")
    plugins += new SrcPlugin("EU0")
    plugins += new MulPlugin("EU0", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU0", writebackAt = 2)
    //    plugins += new IntAluPlugin("EU0")
    //    plugins += new ShiftPlugin("EU0")
    if(aluCount == 1) plugins += new BranchPlugin("EU0", writebackAt = 2, staticLatency = false)
    if(withLoadStore) {
      withLsu2 match {
        case false => {
          withDedicatedLoadAgu match{
            case false => plugins += new LoadPlugin("EU0")
            case true => {
              plugins += new ExecutionUnitBase("LOAD", writebackCountMax = 0, readPhysRsFromQueue = true)
              plugins += new SrcPlugin("LOAD")
              plugins += new LoadPlugin("LOAD")
            }
          }
          plugins += new StorePlugin("EU0")
        }
        case true => plugins += new AguPlugin("EU0")
      }
    }
    plugins += new EnvCallPlugin("EU0")(rescheduleAt = 2)
    plugins += new CsrAccessPlugin("EU0")(
      writebackAt = 2
    )

    if(fpu){
      plugins += new FpuSettingPlugin(withFloat, withDouble)
      plugins += new ExecutionUnitBase("FPU0", writebackCountMax = 0, readPhysRsFromQueue = true)
      plugins += new FpuFloatExecute("FPU0")
      plugins += new RegFilePlugin(
        spec = riscv.FloatRegFile,
        physicalDepth = 64,
        bankCount = 1,
        allOne = simulation,
        preferedWritePortForInit = "Fpu"
      )

      plugins += new FpuIntegerExecute("EU0")

      plugins += new RfAllocationPlugin(riscv.FloatRegFile)
      plugins += new RfTranslationPlugin(riscv.FloatRegFile)

      plugins += new FpuWriteback()
      plugins += new FpuEmbedded()
    }

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

    if(aluCount >= 2) {
      plugins += new ExecutionUnitBase("ALU1")
      plugins += new IntFormatPlugin("ALU1")
      plugins += new SrcPlugin("ALU1")
      plugins += new IntAluPlugin("ALU1")
      plugins += new ShiftPlugin("ALU1")
      plugins += new BranchPlugin("ALU1")
      assert(aluCount < 3)
    }


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


    // Integer write port sharing
    val intRfWrite = new{}
    plugins.collect{
      case lsu : LsuPlugin =>
        lsu.addRfWriteSharing(IntRegFile, intRfWrite, withReady = false, priority = 2)
      case eu0 : ExecutionUnitBase if eu0.euId == "EU0" =>
        eu0.addRfWriteSharing(IntRegFile, intRfWrite, withReady = true, priority = 1)
      case fpu : FpuWriteback =>
        fpu.addRfWriteSharing(IntRegFile, intRfWrite, withReady = true, priority = 0)
    }

    plugins
  }
}
// ramstyle = "MLAB, no_rw_check"
object Gen extends App{
  LutInputs.set(6)
  def plugins = {
    val l = Config.plugins(
      withRdTime = false,
      aluCount    = 2,
      decodeCount = 2,
      debugTriggers = 4,
      withDedicatedLoadAgu = false,
      withRvc = false,
      withLoadStore = true,
      withMmu = true,
      withDebug = false,
      withEmbeddedJtagTap = false,
      jtagTunneled = false,
      withFloat = false,
      withDouble = false,
      withLsu2 = true,
      lqSize = 16,
      sqSize = 16,
//      withCoherency = true,
      ioRange = a => a(31 downto 28) === 0x1// || !a(12)//(a(5, 6 bits) ^ a(12, 6 bits)) === 51
    )
    l.foreach{
      case p : EmbeddedJtagPlugin => p.debugCd.load(ClockDomain.current.copy(reset = Bool().setName("debug_reset")))
      case _ =>
    }
    l
  }

  {
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
    spinalConfig.addTransformationPhase(new MemReadDuringWritePatcherPhase)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    //  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

    spinalConfig.includeSimulation

    val report = spinalConfig.generateVerilog(new NaxRiscv(plugins))
    val doc = report.toplevel.framework.getService[DocPlugin]
    doc.genC()

    val nax = report.toplevel
//    val dcache = nax.framework.getService[DataCachePlugin].logic.cache
  }

  {
    def wrapper[T <: Component](c: T) = {
      c.afterElaboration(c.getAllIo.foreach(_.addTag(crossClockDomain)))
      c.afterElaboration(Rtl.xorOutputs(Rtl.ffIo(c))); c
    }
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
    spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

    spinalConfig.generateVerilog(wrapper(new NaxRiscv(plugins).setDefinitionName("NaxRiscvSynt")))
  }
}

/*
RV32IMASU => 14472
no mmu   => 13427
no mmu, no load/store => 7873
 */

// ramstyle = "MLAB, no_rw_check"
object Gen64 extends App{
  LutInputs.set(6)
  def plugins = {
    val l = Config.plugins(
      sideChannels = false, //WARNING, FOR TEST PURPOSES ONLY, turn to false for real stuff <3
      xlen = 64,
      withRdTime = false,
      aluCount    = 2,
      decodeCount = 2,
      withRvc = false,
      withDebug = false,
      withEmbeddedJtagTap = false,
      debugTriggers = 4,
      withFloat = false,
      withDouble = false,
      lqSize = 16,
      sqSize = 16
    )
    l.foreach{
      case p : EmbeddedJtagPlugin => p.debugCd.load(ClockDomain.current.copy(reset = Bool().setName("debug_reset")))
      case _ =>
    }
//    Tweek.euWritebackAt(l, "ALU0", 1)
//    Tweek.euWritebackAt(l, "ALU1", 1)
    l
  }

  {
    val spinalConfig = SpinalConfig(inlineRom = true, anonymSignalPrefix = "_zz")
    spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
    spinalConfig.addTransformationPhase(new MemReadDuringWritePatcherPhase)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    //  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

    spinalConfig.includeSimulation

    val report = spinalConfig.generateVerilog(new NaxRiscv(plugins))
    val doc = report.toplevel.framework.getService[DocPlugin]
    doc.genC()

    val nax = report.toplevel
    val dcache = nax.framework.getService[DataCachePlugin].logic.cache
  }

  {
    def wrapper[T <: Component](c: T) = {
      c.afterElaboration(c.getAllIo.foreach(_.addTag(crossClockDomain)))
      c.afterElaboration(Rtl.xorOutputs(Rtl.ffIo(c))); c
    }
    val spinalConfig = SpinalConfig(inlineRom = true)
    spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
    spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
    spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

    spinalConfig.generateVerilog(wrapper(new NaxRiscv(plugins).setDefinitionName("NaxRiscvSynt")))
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
./configure --prefix=/opt/riscv --with-cmodel=medany --with-multilib-generator="rv32i-ilp32--;rv64i-lp64--;rv32im-ilp32--;rv32ima-ilp32--;rv32imc-ilp32--;rv32imac-ilp32--;rv32imf-ilp32f--;rv32imaf-ilp32f--;rv32imfc-ilp32f--;rv32imafc-ilp32f--;rv32imfd-ilp32d--;rv32imafd-ilp32d--;rv32imfdc-ilp32d--;rv32imafdc-ilp32d--;rv64im-lp64--;rv64ima-lp64--;rv64imc-lp64--;rv64imac-lp64--;rv64imf-lp64f--;rv64imaf-lp64f--;rv64imfc-lp64f--;rv64imafc-lp64f--;rv64imfd-lp64d--;rv64imafd-lp64d--;rv64imfdc-lp64d--;rv64imafdc-lp64d--"
*/
//stats

/*
checkLq => 6.8slack 550 LUT (16lq/16sq)
 */

/*
obj_dir/VNaxRiscv --name dhrystone --output-dir output/nax/dhrystone --load-elf ../../../../ext/NaxSoftware/baremetal/dhrystone/build/rv32im/dhrystone.elf --start-symbol _start  --stats-print --stats-toggle-symbol sim_time
obj_dir/VNaxRiscv --name coremark --output-dir output/nax/coremark --load-elf ../../../../ext/NaxSoftware/baremetal/coremark/build/rv32im/coremark.elf --start-symbol _start --pass-symbol pass  --stats-print-all --stats-toggle-symbol sim_time
obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/rv32im/play.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --trace --trace-ref --stats-print-all
obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/freertosDemo/integer/rv32im/freertosDemo.elf --start-symbol _start --pass-symbol c_pass --fail-symbol c_fail --stats-print-all


obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/rv64ima/play.elf --start-symbol _start --pass-symbol pass --fail-symbol fail --trace --trace-ref --stats-print-all

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

    val report = spinalConfig.generateVerilog(new NaxRiscv(plugins :+ new Plugin{
      val logic = create late new Area{
        val dcacheCmd = getService[DataCachePlugin].logic.cache.io.store.cmd
        val icacheInv = getService[FetchCachePlugin].logic.invalidate
        val flushingStuff = dcacheCmd.valid && dcacheCmd.flush || !icacheInv.done
        val counter = Reg(UInt(64 bits)) init(0)
        counter := counter + 2
        def addTimeout(cycles : Int): Unit ={
          val noCommit = Timeout(cycles)
          when(getService[CommitService].onCommit().mask.orR || flushingStuff) {noCommit.clear()}
          when((noCommit.state.setName("noCommitTrigger" + cycles)).rise()){
            spinal.core.report(L"lol ${counter}")
          }
        }
        addTimeout(200)
      }
    }))

// no MMU
LUT	12957	133800	9.683856
LUTRAM	2805	46200	6.071429
FF	9278	267600	3.467115
BRAM	11.5	365	3.1506848
DSP	4	740	0.5405406
IO	245	500	49.0
BUFG	1	32	3.125

// no memory
LUT	7875	133800	5.88565
LUTRAM	1964	46200	4.2510824
FF	5641	267600	2.107997
BRAM	6	365	1.6438355
DSP	4	740	0.5405406
IO	138	500	27.599998
BUFG	1	32	3.125



make output/nax/buildroot/run0/PASS ARGS="--stats-print --memory-latency 25 --memory-bandwidth 0.8"
with 4*2KB
root@buildroot:~#SUCCESS buildroot_run0
STATS :
  IPC               0.383806
  cycles            366078779
  commits           140503396
  reschedules       2868806
  trap              10513
  storeToLoadHazard 13478
  loadHitMiss       145029

with 4*4KB
root@buildroot:~#SUCCESS buildroot_run0
STATS :
  IPC               0.436099
  cycles            319994552
  commits           139549454
  reschedules       2839479
  trap              9863
  storeToLoadHazard 12096
  loadHitMiss       146718

with 4*4KB  --memory-latency 8 --memory-bandwidth 1.0
  IPC               0.590046
  cycles            240993947
  commits           142197543
  reschedules       2851075
  trap              8858
  branch miss       0
  jump miss         0
  storeToLoadHazard 11832
  loadHitMiss       150247

 */