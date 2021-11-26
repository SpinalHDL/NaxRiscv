package naxriscv

import naxriscv.backend.{CommitPlugin, RegFilePlugin, RobPlugin}
import naxriscv.compatibility.{MultiPortReadSymplifier, MultiPortWritesSymplifier}
import spinal.core._
import naxriscv.frontend.{FetchAddressTranslationPlugin, _}
import naxriscv.misc.StaticAddressTranslationPlugin
import naxriscv.units._
import naxriscv.utilities._

import scala.collection.mutable.ArrayBuffer

object Config{
  def properties() = {
//    Global.PHYSICAL_WIDTH.set(32)
//    Frontend.RVC.set(true)
//    Frontend.FETCH_DATA_WIDTH.set(64)
//    Frontend.INSTRUCTION_WIDTH.set(32)
//    Frontend.DECODE_COUNT.set(2)
//    Global.COMMIT_COUNT.set(2)
//    ROB.SIZE.set(64)
//    Global.XLEN.set(32)

//    Global.PHYSICAL_WIDTH.set(32)
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
      cacheSize = 4096,
      wayCount = 1,
      injectionAt = 2,
      memDataWidth = Frontend.FETCH_DATA_WIDTH
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
    plugins += new ExecutionUnitBase("ALU0")
    plugins += new IntAluPlugin("ALU0")
    plugins += new BranchPlugin("ALU0")
    plugins += new SrcPlugin("ALU0")
    plugins += new ShiftPlugin("ALU0")

//    plugins += new ExecutionUnitBase("ALU1")
//    plugins += new IntAluPlugin("ALU1")
//    plugins += new BranchPlugin("ALU1")
//    plugins += new SrcPlugin("ALU1")
//    plugins += new ShiftPlugin("ALU1")

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
- ALU wakeup issue queue ahead of rf write
- Regfile bypass
- Having a proper microcode instead of direct RISC-V ?
- Execution unit API
  -
 */

//TODO fix bellow list
/*
- aligner flush connector from fetches.last stage (workarounded via a extra stage)
- Likely pc management in the aligner need rework, specialy what's about btb impact when buffer pc + 4 != input pc ?
 */

//ASSUMPTIONS
/*
X0 init =>
- RfTranslationPlugin entries are initialized to all point to physical 0
- RfAllocationPlugin will reject physical 0 freedoom
- RfAllocationPlugin will not init physical 0
- RegFilePlugin will write physical 0 with 0

 */