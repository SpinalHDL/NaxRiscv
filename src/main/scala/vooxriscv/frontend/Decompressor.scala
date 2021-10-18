package vooxriscv.frontend

import spinal.core._
import spinal.lib._
import vooxriscv.pipeline.Connection.DIRECT
import vooxriscv.utilities._



class DecompressorPlugin() extends Plugin{
  val setup = create early new Area{
    val frontend = framework.getService(classOf[FrontendPlugin])
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.aligned, frontend.pipeline.decompressed)(new DIRECT)
  }

  val logic = create late new Area{

    setup.frontend.release()
  }
}