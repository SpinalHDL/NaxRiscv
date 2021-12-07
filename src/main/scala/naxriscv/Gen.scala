package naxriscv

import naxriscv.backend.{CommitPlugin, RegFilePlugin, RobPlugin}
import naxriscv.compatibility.{MultiPortReadSymplifier, MultiPortWritesSymplifier}
import spinal.core._
import naxriscv.frontend.{FetchAddressTranslationPlugin, _}
import naxriscv.misc.StaticAddressTranslationPlugin
import naxriscv.units._
import naxriscv.units.lsu.{DataCachePlugin, LoadPlugin, LsuPlugin, StorePlugin}
import naxriscv.utilities._

import scala.collection.mutable.ArrayBuffer

object Config{
  def properties() = {
//    Frontend.RVC.set(true)
//    Frontend.FETCH_DATA_WIDTH.set(64)
//    Frontend.INSTRUCTION_WIDTH.set(32)
//    Frontend.DECODE_COUNT.set(2)
//    Global.COMMIT_COUNT.set(2)
//    ROB.SIZE.set(64)
//    Global.XLEN.set(32)

    Frontend.RVC.set(true)
    Frontend.FETCH_DATA_WIDTH.set(32)
    Frontend.INSTRUCTION_WIDTH.set(32)
    Frontend.DECODE_COUNT.set(1)
    Global.COMMIT_COUNT.set(1)
    ROB.SIZE.set(64)
    Global.XLEN.set(32)
  }

  def plugins(): Seq[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
    plugins += new StaticAddressTranslationPlugin(
      peripheralRange = _(31 downto 28) === 0x1
    )
    plugins += new FrontendPlugin()
    plugins += new FetchAddressTranslationPlugin()
    plugins += new PcPlugin()
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      memDataWidth = Frontend.FETCH_DATA_WIDTH,
      reducedBankWidth = false
    )
    plugins += new AlignerPlugin()
    plugins += new DecompressorPlugin()
    plugins += new DecoderPlugin()
    plugins += new RfTranslationPlugin()
    plugins += new RfDependencyPlugin()
    plugins += new RfAllocationPlugin(riscv.IntRegFile)
    plugins += new DispatchPlugin(
      slotCount = 32
    )

    plugins += new LsuPlugin(
      lqSize = 16,
      sqSize = 4
    )
    plugins += new DataCachePlugin(
      memDataWidth = Global.XLEN,
      cacheSize    = 4096*8,
      wayCount     = 1,
      refillCount = 1,
      reducedBankWidth = false
    )

    plugins += new ExecutionUnitBase("ALU0")
    plugins += new SrcPlugin("ALU0")
    plugins += new IntAluPlugin("ALU0")
    plugins += new BranchPlugin("ALU0")
    plugins += new ShiftPlugin("ALU0")
    plugins += new LoadPlugin("ALU0")
    plugins += new StorePlugin("ALU0")

//    plugins += new ExecutionUnitBase("ALU1")
//    plugins += new SrcPlugin("ALU1")
//    plugins += new IntAluPlugin("ALU1")
//    plugins += new BranchPlugin("ALU1")
//    plugins += new ShiftPlugin("ALU1")
//    plugins += new LoadPlugin("ALU1")
//    plugins += new StorePlugin("ALU1")

//    plugins += new ExecuteUnit("ALU0")
//    plugins += new IntAluPlugin("ALU0")
//    plugins += new ExecuteUnit("ALU1")
//    plugins += new IntAluPlugin("ALU1", withAdd = false)
//    plugins += new ExecuteUnitDemo("ALU2", false)
//    plugins += new ExecuteUnit("ALU3")
//    plugins += new MulPlugin("ALU3")
    plugins += new RobPlugin()
    plugins += new CommitPlugin()
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1
    )
    plugins += new PlayPlugin()
    plugins
  }
}

// ramstyle = "MLAB, no_rw_check"
object Gen extends App{
  LutInputs.set(6)
  val spinalConfig = SpinalConfig(inlineRom = true)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
//  spinalConfig.addTransformationPhase(new MultiPortReadSymplifier)

  val report = spinalConfig.generateVerilog(new Component {
    setDefinitionName("NaxRiscv")
    Config.properties()
    val framework = new Framework(Config.plugins())
  })
  val doc = report.toplevel.framework.getService[DocPlugin]
  doc.genC()
}

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
- LSU getting PC for reschedule
 */

//TODO fix bellow list
/*
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