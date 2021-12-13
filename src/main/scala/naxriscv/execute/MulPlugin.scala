package naxriscv.execute

import naxriscv.riscv.Rvi
import naxriscv.utilities.Plugin
import spinal.core._

class MulPlugin(fuId : Any) extends Plugin{
  val setup = create early new Area{
    val fu = getService[ExecuteUnitDemo](fuId)
    fu.addMicroOp(Rvi.MUL)
  }

  val logic = create late new Area{

  }
}
