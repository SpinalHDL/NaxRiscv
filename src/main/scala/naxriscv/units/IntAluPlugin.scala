package naxriscv.units

import naxriscv.interfaces.Riscv
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

class IntAluPlugin(fuId : Any) extends Plugin{
  val setup = create early new Area{
    val fu = getService[FunctionalUnit](fuId)
    fu.addFunction(Riscv.integer.ADD)
    fu.addFunction(Riscv.integer.ADDI)
  }

  val logic = create late new Area{

  }
}
