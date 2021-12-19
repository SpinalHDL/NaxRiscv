package naxriscv.fetch

import naxriscv._
import naxriscv.utilities.Plugin
import spinal.core._

class FetchAddressTranslationPlugin extends Plugin{
  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    fetch.retain()
  }

  val logic = create late new Area{
    val stage = setup.fetch.getStage(0)
    val fetch = getService[FetchPlugin]
    stage(fetch.keys.FETCH_PC_TRANSLATED) := stage(fetch.keys.FETCH_PC)
    setup.fetch.release()
  }
}
