package naxriscv.frontend

import spinal.core._
import naxriscv.utilities.Plugin
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.DecoderService
import spinal.lib.Delay
import spinal.lib.pipeline.StageableOffset

class PlayPlugin extends Plugin{
  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val stage = frontend.pipeline.fetches.last
    val decode = getService[DecoderService]
    import stage._
//    out(stage.isValid)
//    out(stage(Frontend.WORD))
//    out(frontend.pipeline.allocated(INSTRUCTION_DECOMPRESSED, 0))
//    for(i <- 0 until DISPATCH_COUNT) {
//      val stage = frontend.pipeline.allocated
//      import stage._
//      out(Delay(stage(decode.PHYS_RS(0), i), 3))
//      out(Delay(stage(decode.PHYS_RS(1), i), 3))
//    }
    frontend.release()
  }
}
