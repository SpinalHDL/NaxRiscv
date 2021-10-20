package vooxriscv.frontend

import vooxriscv.utilities.Plugin
import spinal.core._

class DirectAddressTranslationPlugin extends Plugin{
  val setup = create early new Area{
    val frontend = framework.getService(classOf[FrontendPlugin])
    frontend.retain()
  }

  val logic = create late new Area{
    val stage = setup.frontend.getStage(0)
    stage(Frontend.FETCH_PC_PHYSICAL) := stage(Frontend.FETCH_PC_VIRTUAL)
      setup.frontend.release()
  }
}
