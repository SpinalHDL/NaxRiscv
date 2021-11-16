package naxriscv.units

import naxriscv.{Frontend, Global}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

object BranchPlugin extends AreaObject {
  val SEL = Stageable(Bool())
  val NEED_BRANCH = Stageable(Bool())
  val COND = Stageable(Bool())
  val PC_TRUE = Stageable(Global.PC)
  val PC_FALSE = Stageable(Global.PC)
  val PC_BRANCH = Stageable(Global.PC)
}

class BranchPlugin(euId : String) extends Plugin {
  withPrefix(euId)

  import BranchPlugin._
  val aluStage = 0
  val branchStage = 1



  val setup = create early new Area{
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()
    eu.addMicroOp(Rvi.BEQ, branchStage)

    val baseline = eu.DecodeList(SEL -> True)

    eu.addDecodingDefault(SEL, False)
    eu.addDecoding(Rvi.BEQ,  baseline)

    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)
  }

  val logic = create late new Area{
    val sliceShift = if(Frontend.RVC) 1 else 2

    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(0)

      import stage._

      val src1 = S(eu(IntRegFile, RS1))
      val src2 = S(eu(IntRegFile, RS2))

      COND := src1 === src2
      PC_TRUE := U(S(Global.PC) + IMM(Frontend.MICRO_OP).b_sext)
      val slices = Frontend.INSTRUCTION_SLICE_COUNT+1
      PC_FALSE := Global.PC + (slices << sliceShift)
      PC_BRANCH := NEED_BRANCH ? stage(PC_TRUE) | stage(PC_FALSE)
      NEED_BRANCH := COND //For now, until prediction are iplemented
    }

    val branch = new Area{
      val stage = eu.getExecute(branchStage)
      import stage._
      setup.reschedule.valid := isFireing && SEL && NEED_BRANCH
      setup.reschedule.robId := ExecutionUnitKeys.ROB_ID
      setup.reschedule.cause := 0
      setup.reschedule.tval := 0
      setup.reschedule.pcTarget := PC_BRANCH

      setup.reschedule.trap := PC_BRANCH(0, sliceShift bits) =/= 0
      setup.reschedule.skipCommit := True
    }
    eu.release()
  }
}
