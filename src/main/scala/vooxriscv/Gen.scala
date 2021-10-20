package vooxriscv

import spinal.core._
import vooxriscv.frontend._
import vooxriscv.utilities._

import scala.collection.mutable.ArrayBuffer

object Config{
  def properties() = {
    Global.PHYSICAL_WIDTH.set(32)
    Frontend.RVC.set(true)
    Frontend.FETCH_DATA_WIDTH.set(64)
    Frontend.INSTRUCTION_WIDTH.set(32)
    Frontend.DECODE_COUNT.set(2)
  }
  def plugins(): Seq[Plugin] ={
    val plugins = ArrayBuffer[Plugin]()
    plugins += new FrontendPlugin()
    plugins += new DirectAddressTranslationPlugin()
    plugins += new PcPlugin()
    plugins += new FetchL1Plugin(
      cacheSize = 4096,
      wayCount = 1
    )
    plugins += new AlignerPlugin()
    plugins += new DecoderPlugin()
    plugins += new SandboxPlugin()
    plugins
  }
}
object Gen extends App{
  SpinalVerilog(new Component {
    Config.properties()
    val frontend = new Framework(Config.plugins())
  })
}

//object GenSim extends App{
//  import spinal.core.sim._
//  SimConfig.withFstWave.compile(new Component {
//    Config.properties()
//    val frontend = new Framework(Config.plugins())
//  }).doSim(seed = 42){}
//}


