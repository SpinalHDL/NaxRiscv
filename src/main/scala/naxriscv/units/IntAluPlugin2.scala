package naxriscv.units

import naxriscv.riscv.Rvi
import naxriscv.utilities.Plugin
import spinal.core._

class IntAluPlugin2(fuId : Any, withAddi : Boolean = true) extends Plugin{
  val setup = create early new Area{
    val fu = getService[ExecuteUnit](fuId)
    fu.addMicroOp(Rvi.ADD)
    if(withAddi)fu.addMicroOp(Rvi.ADDI)
    fu.addMicroOp(Rvi.BEQ)
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](fuId)


  }
}
