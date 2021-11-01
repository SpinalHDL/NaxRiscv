package naxriscv.frontend

import spinal.core._
import naxriscv.utilities.Plugin
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._

class PlayPlugin extends Plugin{
  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val stage = frontend.pipeline.fetches.last
    import stage._
    out(stage.isValid)
    out(stage(Frontend.WORD))
    out(frontend.pipeline.allocated(INSTRUCTION_DECOMPRESSED, 0))
    frontend.release()
  }
}
