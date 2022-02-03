package naxriscv.execute

import naxriscv.Frontend.MICRO_OP
import naxriscv.{Fetch, Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.{Flow, KeepAttribute}
import spinal.lib.pipeline.Stageable
import naxriscv.Global._
import naxriscv.fetch.{FetchCachePlugin, FetchPlugin}
import naxriscv.frontend.DispatchPlugin
import naxriscv.lsu.LsuPlugin
import naxriscv.misc.MmuPlugin
import naxriscv.prediction.{BranchContextPlugin, DecoderPredictionPlugin}
import spinal.lib.fsm._

object BranchPlugin extends AreaObject {
  val NEED_BRANCH = Stageable(Bool())
  val COND = Stageable(Bool())
  val EQ = Stageable(Bool())
  val BranchCtrlEnum = new SpinalEnum(binarySequential){
    val B, JAL, JALR = newElement()
  }
  val BRANCH_CTRL = new Stageable(BranchCtrlEnum())
  val VMA_INVALIDATE = Stageable(Bool())
  val FETCH_INVALIDATE = Stageable(Bool())
  val MISSPREDICTED = Stageable(Bool())
  val MISSALIGNED = Stageable(Bool())
  val BAD_EARLY_TARGET = Stageable(Bool())
}

class BranchPlugin(euId : String,
                   staticLatency : Boolean = true,
                   writebackAt : Int = 0,
                   branchAt : Int = 1) extends ExecutionUnitElementSimple(euId, staticLatency)  {
  import BranchPlugin._


  override def euWritebackAt = writebackAt
  override def euCompletionAt = branchAt

  override val setup = create early new Setup{
    getService[RobService].retain()
    val sk = SrcKeys

    add(Rvi.JAL , List(                                    ), List(BRANCH_CTRL -> BranchCtrlEnum.JAL))
    add(Rvi.JALR, List(              sk.SRC1.RF            ), List(BRANCH_CTRL -> BranchCtrlEnum.JALR))
    add(Rvi.BEQ , List(              sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BNE , List(              sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BLT , List(sk.Op.LESS  , sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BGE , List(sk.Op.LESS  , sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BLTU, List(sk.Op.LESS_U, sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))
    add(Rvi.BGEU, List(sk.Op.LESS_U, sk.SRC1.RF, sk.SRC2.RF), List(BRANCH_CTRL -> BranchCtrlEnum.B))

    val withBranchContext = isServiceAvailable[BranchContextPlugin]
    if(withBranchContext){
      eu.addRobStageable(getService[BranchContextPlugin].keys.BRANCH_ID)
    }
    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)
  }

  override val logic = create late new Logic{
    val fetch = getService[FetchPlugin]
    val commit = getService[CommitService]
    val rob = getService[RobService]
    val sliceShift = if(Fetch.RVC) 1 else 2
    val branchContext = setup.withBranchContext generate getService[BranchContextPlugin]
    val bck = setup.withBranchContext generate branchContext.keys.get
    import bck._

    val process = new ExecuteArea(0) {
      import stage._
      val ss = SrcStageables

      EQ := ss.SRC1 === ss.SRC2

      COND := BRANCH_CTRL.mux(
        BranchCtrlEnum.JALR -> True,
        BranchCtrlEnum.JAL -> True,
        BranchCtrlEnum.B    -> MICRO_OP(14 downto 12).mux(
          B"000"  -> EQ,
          B"001"  -> !EQ,
          M"1-1"  -> !ss.LESS,
          default ->  ss.LESS
        )
      )

      val imm = IMM(Frontend.MICRO_OP)
      val target_a = BRANCH_CTRL.mux(
        default             -> S(stage(PC)),
        BranchCtrlEnum.JALR -> stage(ss.SRC1)
      )

      val target_b = BRANCH_CTRL.mux(
        default              -> imm.b_sext,
        BranchCtrlEnum.JAL   -> imm.j_sext,
        BranchCtrlEnum.JALR  -> imm.i_sext
      )

      (PC, "TRUE") := U(target_a + target_b)
      (PC, "TRUE")(0) := False

      val slices = Fetch.INSTRUCTION_SLICE_COUNT+^1
      (PC, "FALSE") := PC + (slices << sliceShift)
      (PC, "TARGET") := COND ? stage(PC, "TRUE") | stage(PC, "FALSE")

      // Without those keepattribute, Vivado will transform the logic in a way which will serialize the 32 bits of the COND comparator,
      // with the 32 bits of the TRUE/FALSE adders, ending up in a quite long combinatorial path (21 lut XD)
      KeepAttribute(stage(PC, "TRUE"))
      KeepAttribute(stage(PC, "FALSE"))

      if(setup.withBranchContext) stage(BRANCH_EARLY) := branchContext.readEarly(BRANCH_ID)

      BAD_EARLY_TARGET := (if(setup.withBranchContext) BRANCH_EARLY.pc    =/= stage(PC, "TRUE") else False)
    }

    val writeback = new ExecuteArea(writebackAt){
      import stage._

      wb.payload := B(stage(PC, "FALSE")) //JAL/JALR
    }

    val branch = new ExecuteArea(branchAt){
      import stage._

      val badEarlyTaken  = if(setup.withBranchContext) BRANCH_EARLY.taken =/= COND              else CombInit(stage(COND))
      MISSPREDICTED := badEarlyTaken || COND && BAD_EARLY_TARGET

      def target = if(setup.withBranchContext)  stage(PC, "TARGET") else stage(PC, "TRUE")

      MISSALIGNED := (if(Fetch.RVC) False else target(0, sliceShift bits) =/= 0 && COND)

      setup.reschedule.valid := isFireing && SEL && (MISSPREDICTED || MISSALIGNED)
      setup.reschedule.robId := ROB.ID
      setup.reschedule.cause := 0
      setup.reschedule.tval := 0
      setup.reschedule.pcTarget := target
      setup.reschedule.reason  := ((BRANCH_CTRL === BranchCtrlEnum.B) ? U(ScheduleReason.BRANCH) otherwise U(ScheduleReason.JUMP)).resized

      setup.reschedule.trap := MISSALIGNED
      setup.reschedule.skipCommit := MISSALIGNED

      val finalBranch = setup.withBranchContext generate branchContext.writeFinal()
      if(setup.withBranchContext) {
        finalBranch.valid := isValid && SEL
        finalBranch.address := BRANCH_ID
        finalBranch.data.pcOnLastSlice := PC + (Fetch.INSTRUCTION_SLICE_COUNT << sliceShift)
        finalBranch.data.pcTarget := stage(PC, "TRUE")
        finalBranch.data.taken := COND

        rob.write(branchContext.keys.BRANCH_TAKEN, 1, List(stage(COND)), ROB.ID, isFireing)
      }
    }
    rob.release()
  }
}

