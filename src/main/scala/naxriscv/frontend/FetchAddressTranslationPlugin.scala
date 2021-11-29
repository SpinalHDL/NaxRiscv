package naxriscv.frontend

import naxriscv._
import naxriscv.utilities.Plugin
import spinal.core._

class FetchAddressTranslationPlugin extends Plugin{
  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
  }

  val logic = create late new Area{
    val stage = setup.frontend.getStage(0)
    val frontend = getService[FrontendPlugin]
    stage(frontend.keys.FETCH_PC_POST_TRANSLATION) := stage(frontend.keys.FETCH_PC_PRE_TRANSLATION)
      setup.frontend.release()
  }
}
