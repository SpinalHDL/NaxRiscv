package naxriscv

import naxriscv.backend.{CommitPlugin, RegFilePlugin, RobPlugin}
import naxriscv.compatibility.MultiPortWritesSymplifier
import spinal.core._
import naxriscv.frontend._
import naxriscv.interfaces.{ExecutionUnitPush, Riscv}
import naxriscv.units.{ExecuteUnit, IntAluPlugin, MulPlugin}
import naxriscv.utilities._

import scala.collection.mutable.ArrayBuffer

object Config{
  def properties() = {
    Global.PHYSICAL_WIDTH.set(32)
    Frontend.RVC.set(true)
    Frontend.FETCH_DATA_WIDTH.set(64)
    Frontend.INSTRUCTION_WIDTH.set(32)
    Frontend.DECODE_COUNT.set(2)
    Global.COMMIT_COUNT.set(2)
    ROB.SIZE.set(64)
    Global.XLEN.set(32)

//    Global.PHYSICAL_WIDTH.set(32)
//    Frontend.RVC.set(true)
//    Frontend.FETCH_DATA_WIDTH.set(32)
//    Frontend.INSTRUCTION_WIDTH.set(32)
//    Frontend.DECODE_COUNT.set(1)
//    Global.COMMIT_COUNT.set(1)
//    ROB.SIZE.set(64)
//    Global.XLEN.set(32)
  }

  def plugins(): Seq[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
    plugins += new FrontendPlugin()
    plugins += new DirectAddressTranslationPlugin()
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
    plugins += new RfAllocationPlugin(Riscv.integer.regfile)
    plugins += new DispatchPlugin(
      slotCount = 32
    )
    plugins += new ExecuteUnit("ALU0")
    plugins += new IntAluPlugin("ALU0")
    plugins += new ExecuteUnit("ALU1")
    plugins += new IntAluPlugin("ALU1")
//    plugins += new ExecuteUnit("ALU2")
//    plugins += new IntAluPlugin("ALU2", false)
//    plugins += new ExecuteUnit("ALU3")
//    plugins += new MulPlugin("ALU3")
    plugins += new RobPlugin()
    plugins += new CommitPlugin()
    plugins += new RegFilePlugin(
      spec = Riscv.integer.regfile,
      physicalDepth = 64,
      bankCount = 1
    )
    plugins += new PlayPlugin()
    plugins
  }
}
object Gen extends App{
  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

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