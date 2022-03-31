package naxriscv.debug

import naxriscv.utilities._
import spinal.core._
import spinal.lib._

class DebugPlugin extends Plugin{


  val logic = create late new Area{
    val bus = master(DebugHartBus())

    // Program buffer
  }
}


/*
make compile && ./obj_dir/VNaxRiscv --timeout-disable --spike-disable

src/openocd -f ../VexRiscvOoo/src/main/tcl/openocd/naxriscv_sim.tcl
 */