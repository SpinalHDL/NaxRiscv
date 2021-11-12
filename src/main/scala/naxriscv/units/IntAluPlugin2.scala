package naxriscv.units

import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._

class IntAluPlugin2(euId : Any, withAddi : Boolean = true) extends Plugin{
  val setup = create early new Area{
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()
    eu.addMicroOp(Rvi.ADD)
    if(withAddi)eu.addMicroOp(Rvi.ADDI)
    eu.addMicroOp(Rvi.BEQ)
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val stage = eu.getExecute(0)
    import stage._

    val rs1 = U(eu(IntRegFile, RS1))
    val rs2 = U(eu(IntRegFile, RS2))
    val result = rs1 + rs2
    eu.release()
  }
}
