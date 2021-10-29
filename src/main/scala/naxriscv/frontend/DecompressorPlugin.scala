package naxriscv.frontend

import spinal.core._
import spinal.lib._
import naxriscv.pipeline.Connection.DIRECT
import naxriscv.utilities._
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.pipeline.{Stage, Stageable}



class DecompressorPlugin() extends Plugin{
  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.aligned, frontend.pipeline.decompressed)(new DIRECT)
  }

  val logic = create late new Area{
    val stage = setup.frontend.pipeline.decompressed
    import stage._
    for(i <- 0 until DECODE_COUNT) {
      stage(INSTRUCTION_DECOMPRESSED, i) := stage(INSTRUCTION_ALIGNED, i)
    }
    setup.frontend.release()
  }
}