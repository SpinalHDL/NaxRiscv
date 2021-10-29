package naxriscv.frontend

import naxriscv.utilities.Plugin
import spinal.core._
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._

class DirectAddressTranslationPlugin extends Plugin{
  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
  }

  val logic = create late new Area{
    val stage = setup.frontend.getStage(0)
    stage(Frontend.FETCH_PC_PHYSICAL) := stage(Frontend.FETCH_PC_VIRTUAL)
      setup.frontend.release()
  }
}
