package naxriscv.units

import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._

class IntAluPlugin2(euId : Any) extends Plugin{
  val aluStage = 0
  val branchStage = 1
  val setup = create early new Area{
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()
    eu.addMicroOp(Rvi.ADD, aluStage)
    eu.addMicroOp(Rvi.ADDI, aluStage)
    eu.addMicroOp(Rvi.BEQ, branchStage)
    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(0)

      import stage._

      val rs1 = U(eu(IntRegFile, RS1))
      val rs2 = U(eu(IntRegFile, RS2))
      val result = rs1 + rs2

      val wb = eu.newWriteback(IntRegFile, RD, stage)
      wb.valid := False
      wb.payload := B(result)
    }

    val branch = new Area{
      val stage = eu.getExecute(branchStage)
      import stage._
      setup.reschedule.valid := False
      setup.reschedule.trap := False
      setup.reschedule.robId := ExecutionUnitKeys.ROB_ID
      setup.reschedule.cause := 0
      setup.reschedule.tval := 0
      setup.reschedule.pcTarget := 0
      setup.reschedule.skipCommit := False
    }
    eu.release()
  }
}
