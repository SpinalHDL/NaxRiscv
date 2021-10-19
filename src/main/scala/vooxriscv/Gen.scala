package vooxriscv

import spinal.core._
import vooxriscv.frontend._
import vooxriscv.utilities.{Framework, FrameworkConfig, JunctionPlugin, Plugin}

import scala.collection.mutable.ArrayBuffer

object Config{
  def default() = {
    Global.PHYSICAL_WIDTH.set(32)
    Frontend.RVC.set(true)
    Frontend.FETCH_DATA_WIDTH.set(64)
    Frontend.INSTRUCTION_WIDTH.set(32)
    Frontend.DECODE_COUNT.set(2)
  }
}
object Gen extends App{
  SpinalVerilog(new Component {
    Config.default()
    val plugins = ArrayBuffer[Plugin]()
    plugins += new JunctionPlugin()
    plugins += new FrontendPlugin()
    plugins += new PcPlugin()
    plugins += new FetchL1Plugin(
      cacheSize = 4096,
      wayCount = 1
    )
    plugins += new AlignerPlugin()
    plugins += new DecoderPlugin()
    val frontend = new Framework(plugins)
  })
}
