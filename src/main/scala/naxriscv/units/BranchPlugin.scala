package naxriscv.units

import naxriscv.Frontend.MICRO_OP
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
  val EQ = Stageable(Bool())
  val BranchCtrlEnum = new SpinalEnum(binarySequential){
    val B,JAL,JALR = newElement()
  }
  val BRANCH_CTRL = new Stageable(BranchCtrlEnum())
}

class BranchPlugin(euId : String, staticLatency : Boolean = true) extends Plugin with WakeRobService with WakeRegFileService {
  withPrefix(euId)

  override def wakeRobs = if(!staticLatency) List(logic.process.wake.rob) else Nil
  override def wakeRegFile = if(!staticLatency) List(logic.process.wake.rf) else Nil

  import BranchPlugin._
  val aluStage = 0
  val branchStage = 1



  val setup = create early new Area{
    val sk = SrcKeys
    val src = getService[SrcPlugin](euId)
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    def add(microOp: MicroOp, srcKeys : List[SrcKeys], decoding : eu.DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.setStaticCompletion(microOp, branchStage)
      eu.addDecoding(microOp, decoding)
      if(srcKeys.nonEmpty) src.specify(microOp, srcKeys)
    }

    val baseline = eu.DecodeList(SEL -> True)

    eu.setDecodingDefault(SEL, False)
    add(Rvi.JAL , List(                                    ), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.JAL))
    add(Rvi.JALR, List(              sk.SRC1.RF            ), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.JALR))
    add(Rvi.BEQ , List(              sk.SRC1.RF, sk.SRC2.RF), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BNE , List(              sk.SRC1.RF, sk.SRC2.RF), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BLT , List(sk.Op.LESS  , sk.SRC1.RF, sk.SRC2.RF), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BGE , List(sk.Op.LESS  , sk.SRC1.RF, sk.SRC2.RF), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BLTU, List(sk.Op.LESS_U, sk.SRC1.RF, sk.SRC2.RF), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BGEU, List(sk.Op.LESS_U, sk.SRC1.RF, sk.SRC2.RF), baseline ++ List(BRANCH_CTRL -> BranchCtrlEnum.B))

    if(staticLatency) {
      eu.setStaticWake(Rvi.JAL, aluStage)
      eu.setStaticWake(Rvi.JALR, aluStage)
    }
    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)
  }

  val logic = create late new Area{
    val sliceShift = if(Frontend.RVC) 1 else 2

    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(0)
      val ss = SrcStageables
      val decode = getService[DecoderService]

      import stage._

//      val src1 = S(eu(IntRegFile, RS1))
//      val src2 = S(eu(IntRegFile, RS2))

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
        BranchCtrlEnum.B    -> S(Global.PC),
        BranchCtrlEnum.JAL  -> S(Global.PC),
        BranchCtrlEnum.JALR -> stage(ss.SRC1)
      )

      val target_b = BRANCH_CTRL.mux(
        BranchCtrlEnum.B    -> imm.b_sext,
        BranchCtrlEnum.JAL  -> imm.j_sext,
        BranchCtrlEnum.JALR -> imm.i_sext
      )

      PC_TRUE := U(target_a + target_b)
      val slices = Frontend.INSTRUCTION_SLICE_COUNT+^1
      PC_FALSE := Global.PC + (slices << sliceShift)
      PC_BRANCH := NEED_BRANCH ? stage(PC_TRUE) | stage(PC_FALSE)
      NEED_BRANCH := COND //For now, until prediction are implemented

      val wb = eu.newWriteback(IntRegFile, RD, stage, if(staticLatency) 0 else 1)
      wb.valid := SEL
      wb.payload := B(PC_FALSE)

      val wake = !staticLatency generate new Area{
        val fire = isFireing && SEL
        val rob = Flow(WakeRob())
        val rf = Flow(WakeRegFile(decode.PHYS_RD, needBypass = false))

        rob.valid := fire
        rob.robId := ExecutionUnitKeys.ROB_ID

        rf.valid := fire
        rf.physical := decode.PHYS_RD
      }
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
