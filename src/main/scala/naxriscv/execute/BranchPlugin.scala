package naxriscv.execute

import naxriscv.Frontend.MICRO_OP
import naxriscv.{Frontend, Global}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

object BranchPlugin extends AreaObject {
  val NEED_BRANCH = Stageable(Bool())
  val COND = Stageable(Bool())
  val EQ = Stageable(Bool())
  val BranchCtrlEnum = new SpinalEnum(binarySequential){
    val B,JAL,JALR = newElement()
  }
  val BRANCH_CTRL = new Stageable(BranchCtrlEnum())
}

class BranchPlugin(euId : String, staticLatency : Boolean = true, linkAt : Int = 0, branchAt : Int = 1) extends ExecutionUnitElementSimple(euId, staticLatency)  {
  import BranchPlugin._


  override def writeBackAt = linkAt
  override def completionAt = branchAt

  override val setup = create early new Setup{
    val sk = SrcKeys

    add(Rvi.JAL , List(                                    ), List(BRANCH_CTRL -> BranchCtrlEnum.JAL))
    add(Rvi.JALR, List(              sk.SRC1.RF            ), List(BRANCH_CTRL -> BranchCtrlEnum.JALR))
    add(Rvi.BEQ , List(              sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BNE , List(              sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BLT , List(sk.Op.LESS  , sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BGE , List(sk.Op.LESS  , sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BLTU, List(sk.Op.LESS_U, sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BGEU, List(sk.Op.LESS_U, sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))

    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)
  }

  override val logic = create late new Logic{
    val PC = getService[AddressTranslationService].PC
    val sliceShift = if(Frontend.RVC) 1 else 2

    val process = new Area {
      val stage = eu.getExecute(0)
      val ss = SrcStageables

      import stage._

      EQ := ss.SRC1 === ss.SRC2

      COND := BRANCH_CTRL.mux(
        BranchCtrlEnum.JAL  -> True,
        BranchCtrlEnum.JALR -> True,
        BranchCtrlEnum.B    -> MICRO_OP(14 downto 12).mux(
          B"000"  -> EQ,
          B"001"  -> !EQ,
          M"1-1"  -> !ss.LESS,
          default ->  ss.LESS
        )
      )

      val imm = IMM(Frontend.MICRO_OP)
      val target_a = BRANCH_CTRL.mux(
        BranchCtrlEnum.B    -> S(stage(PC)),
        BranchCtrlEnum.JAL  -> S(stage(PC)),
        BranchCtrlEnum.JALR -> stage(ss.SRC1)
      )

      val target_b = BRANCH_CTRL.mux(
        BranchCtrlEnum.B    -> imm.b_sext,
        BranchCtrlEnum.JAL  -> imm.j_sext,
        BranchCtrlEnum.JALR -> imm.i_sext
      )

      (PC, "TRUE") := U(target_a + target_b)
      val slices = Frontend.INSTRUCTION_SLICE_COUNT+^1
      (PC, "FALSE") := PC + (slices << sliceShift)
      (PC, "BRANCH") := NEED_BRANCH ? stage(PC, "TRUE") | stage(PC, "FALSE")
      NEED_BRANCH := COND //For now, until prediction are implemented

      wb.payload := B(stage(PC, "FALSE"))
    }

    val branch = new Area{
      val stage = eu.getExecute(branchAt)
      import stage._

      setup.reschedule.valid := isFireing && SEL && NEED_BRANCH
      setup.reschedule.robId := ExecutionUnitKeys.ROB_ID
      setup.reschedule.cause := 0
      setup.reschedule.tval := 0
      setup.reschedule.pcTarget := stage(PC, "BRANCH")
      setup.reschedule.reason  := ScheduleReason.BRANCH

      setup.reschedule.trap := stage(PC, "BRANCH")(0, sliceShift bits) =/= 0
      setup.reschedule.skipCommit := setup.reschedule.trap
    }
  }
}
