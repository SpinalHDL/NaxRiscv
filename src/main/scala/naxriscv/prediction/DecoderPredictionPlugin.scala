package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.prediction.Prediction._
import naxriscv.fetch.{AlignerPlugin, FetchConditionalPrediction, FetchPlugin, PcPlugin}
import naxriscv.frontend.{DecoderPlugin, FrontendPlugin}
import naxriscv.interfaces.{CommitService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import naxriscv.utilities.Plugin
import naxriscv.{Frontend, Global, ROB}
import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.pipeline.{Stage, Stageable, StageableOffset}

class DecoderPredictionPlugin( decodeAt: FrontendPlugin => Stage = _.pipeline.decoded,
                               pcAddAt: FrontendPlugin => Stage = _.pipeline.decoded,
                               pcPredictionAt: FrontendPlugin => Stage = _.pipeline.decoded,
                               applyAt : FrontendPlugin => Stage = _.pipeline.allocated) extends Plugin{
  val keys = create early new AreaRoot{
    val BRANCH_CONDITIONAL = Stageable(Bool())
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val rob = getService[RobService]
    val aligner = getService[AlignerPlugin]
    val branchContext = getService[BranchContextPlugin]
    val decodeJump = jump.createJumpInterface(JumpService.Priorities.DECODE_PREDICTION)
    val historyPush = getService[HistoryPlugin].createPushPort(0, DISPATCH_COUNT)

    frontend.retain()
    fetch.retain()
    rob.retain()
    branchContext.retain()

    aligner.addWordContext(
      CONDITIONAL_TAKE_IT
    )
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val decoder = getService[DecoderPlugin]
    val rob = getService[RobService]
    val commit = getService[CommitService]
    val branchContext = getService[BranchContextPlugin]

    val k = new AreaRoot{
      val PC_INC = Stageable(SInt(PC_WIDTH bits))
      val PC_TARGET_PRE_RAS = Stageable(SInt(PC_WIDTH bits))
      val PC_TARGET = Stageable(SInt(PC_WIDTH bits))
      val PC_PREDICTION = Stageable(SInt(PC_WIDTH bits))
      val PC_NEXT = Stageable(UInt(PC_WIDTH bits))
      val MISSMATCH = Stageable(Bool())
      val IS_JAL    = Stageable(Bool())
      val IS_JALR   = Stageable(Bool())
      val IS_BRANCH = Stageable(Bool())
      val IS_ANY    = Stageable(Bool())
      val RAS_PUSH  = Stageable(Bool())
      val RAS_POP   = Stageable(Bool())
      val CAN_IMPROVE = Stageable(Bool())
      val BRANCHED_PREDICTION = Stageable(Bool())
      val NEED_CORRECTION = Stageable(Bool())
      val OFFSET = Stageable(SInt(32 bits))
      val CONDITIONAL_PREDICTION = Stageable(Bool())
    }
    import k._

    val decodeStage = decodeAt(frontend)
    val pcAddStage  = pcAddAt(frontend)
    val pcPredictionStage  = pcPredictionAt(frontend)
    val applyStage  = applyAt(frontend)

    if(!isServiceAvailable[FetchConditionalPrediction]){
      fetch.getLastStage(CONDITIONAL_TAKE_IT) := 0
    }

    val ras = new Area{
      val rasDepth = 32
      val mem = new Area{
        val stack = Mem.fill(rasDepth)(PC)
      }
      val ptr = new Area{
        val push = Reg(UInt(log2Up(rasDepth) bits)) init(0)
        val pop = Reg(UInt(log2Up(rasDepth) bits)) init(rasDepth-1)
        val pushIt, popIt = False

        push := push + U(pushIt) - U(popIt)
        pop  := pop + U(pushIt) - U(popIt)
      }
      val read = mem.stack.readAsync(ptr.pop)
      val write = mem.stack.writePort
      write.valid := ptr.pushIt
      write.address := ptr.push
      write.data.assignDontCare()
    }


    val decodePatch = new Area {
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
      branchDecoder.addNeeds(branchKeys, Masked.one)
      jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
      jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
      anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)



      var rasPushUsed = False
      val slots = for (slotId <- 0 until Frontend.DECODE_COUNT) yield new Area {
        implicit val _ = StageableOffset(slotId)

        def inst = Frontend.INSTRUCTION_DECOMPRESSED

        val decode = new Area{
          import decodeStage._

          IS_JAL    := jalDecoder.build(inst, decoder.covers())
          IS_JALR   := jalrDecoder.build(inst, decoder.covers())
          IS_BRANCH := branchDecoder.build(inst, decoder.covers())
          IS_ANY    := anyDecoder.build(inst, decoder.covers())

          val imm = IMM(inst)
          OFFSET := inst(2).mux(
            False -> imm.b_sext,
            True -> imm.j_sext
          )

          val rdLink  = List(1,5).map(decoder.ARCH_RD === _).orR
          val rs1Link = List(1,5).map(decoder.ARCH_RS(0) === _).orR
          val rdEquRs1 = decoder.ARCH_RD === decoder.ARCH_RS(0)
          RAS_PUSH  := (IS_JAL || IS_JALR) && rdLink
          RAS_POP   := IS_JALR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)

          val lastSlice = PC(SLICE_RANGE) + INSTRUCTION_SLICE_COUNT
          CONDITIONAL_PREDICTION :=  CONDITIONAL_TAKE_IT(lastSlice)
        }

        val pcAdd = new Area{
          import pcAddStage._

          val slices = INSTRUCTION_SLICE_COUNT +^ 1
          PC_INC := S(PC + (slices << SLICE_RANGE_LOW))
          PC_TARGET_PRE_RAS := S(PC) + OFFSET
        }

        val pcPrediction = new Area {
          import pcPredictionStage._
          val stage = pcPredictionStage


          PC_TARGET := PC_TARGET_PRE_RAS
          when(IS_JALR){ PC_TARGET := S(ras.read) }
          CAN_IMPROVE := !IS_JALR || RAS_POP
          BRANCHED_PREDICTION := IS_BRANCH && CONDITIONAL_PREDICTION || IS_JAL || IS_JALR
          PC_PREDICTION := BRANCHED_PREDICTION ? stage(PC_TARGET, slotId) otherwise PC_INC
        }

        val applyIt = new Area{
          val stage = applyStage
          import applyStage._

          PC_NEXT := CAN_IMPROVE ?  U(PC_PREDICTION) otherwise ALIGNED_BRANCH_PC_NEXT
          MISSMATCH := !ALIGNED_BRANCH_VALID && BRANCHED_PREDICTION || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(PC_PREDICTION)
          NEED_CORRECTION := DECODED_MASK && CAN_IMPROVE && MISSMATCH

          branchContext.keys.BRANCH_SEL := IS_ANY
          branchContext.keys.BRANCH_EARLY.pcNext := PC_NEXT
          keys.BRANCH_CONDITIONAL := IS_BRANCH

          when(DISPATCH_MASK && RAS_PUSH) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
            when(!rasPushUsed){
              ras.write.data := U(PC_INC)
            }
            rasPushUsed \= True
            ras.ptr.pushIt := True
          }
          when(DISPATCH_MASK && RAS_POP) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
            ras.ptr.popIt := True
          }
        }
      }



      val applyIt = new Area{
        val stage = applyStage
        import applyStage._

        ras.ptr.pushIt    clearWhen(!isFireing)
        ras.ptr.popIt     clearWhen(!isFireing)

        val slotIds = (0 until DECODE_COUNT)
        val hit = slotIds.map(stage(NEED_CORRECTION, _)).orR
        val selOh = OHMasking.first(slotIds.map(stage(NEED_CORRECTION, _)))
        setup.decodeJump.valid := isFireing && hit
        setup.decodeJump.pc := U(MuxOH(selOh, (0 until DECODE_COUNT).map(stage(PC_PREDICTION, _))))

        flushIt(setup.decodeJump.valid, root = false)

        //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
        for (slotId <- 0 until Frontend.DECODE_COUNT) {
          stage(Frontend.DISPATCH_MASK, slotId) := stage(Frontend.DECODED_MASK, slotId) && !stage(0 until slotId)(NEED_CORRECTION).orR
        }

        for (slotId <- 0 until Frontend.DECODE_COUNT) {
          val slot = slots(slotId)
          implicit val _ = StageableOffset(slotId)
          setup.historyPush.mask(slotId) := isFireing && IS_BRANCH && Frontend.DECODED_MASK && !stage(0 until slotId)(NEED_CORRECTION).orR
          setup.historyPush.taken(slotId) := BRANCHED_PREDICTION
        }
      }
    }

    val onDispatch = new Area{
      val stage = frontend.pipeline.dispatch
      import stage._
      rob.write(keys.BRANCH_CONDITIONAL, DISPATCH_COUNT, stage(0 until DISPATCH_COUNT)(keys.BRANCH_CONDITIONAL),  ROB.ID, isFireing)

      branchContext.dispatchWrite(
        keys.BRANCH_CONDITIONAL,
        CONDITIONAL_TAKE_IT
      )
    }

    rob.release()
    frontend.release()
    fetch.release()
    branchContext.release()
  }
}