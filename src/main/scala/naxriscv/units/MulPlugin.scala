package naxriscv.units

import naxriscv.interfaces.Riscv
import naxriscv.utilities.Plugin
import spinal.core._

class MulPlugin(fuId : Any) extends Plugin{
  val setup = create early new Area{
    val fu = getService[ExecuteUnit](fuId)
    fu.addFunction(Riscv.integer.MUL)
  }

  val logic = create late new Area{

  }
}
