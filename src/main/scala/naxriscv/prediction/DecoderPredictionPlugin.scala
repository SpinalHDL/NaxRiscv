package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.prediction.Prediction._
import naxriscv.fetch.{AlignerPlugin, FetchPlugin, PcPlugin}
import naxriscv.frontend.{DecoderPlugin, FrontendPlugin}
import naxriscv.interfaces.{CommitService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import naxriscv.utilities.Plugin
import naxriscv.{Frontend, Global, ROB}
import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.pipeline.{Stage, Stageable, StageableOffset}


//flushOnBranch can be used to ensure correctness of the branch history used by predictors (usefull for debug) at the cost of flushes
class DecoderPredictionPlugin( var decodeAt: FrontendPlugin => Stage = _.pipeline.decoded,
                               var pcAddAt: FrontendPlugin => Stage = _.pipeline.decoded,
                               var pcPredictionAt: FrontendPlugin => Stage = _.pipeline.decoded,
                               var applyAt : FrontendPlugin => Stage = _.pipeline.serialized,
                               var flushOnBranch : Boolean = false,
                               var rasDepth : Int = 16) extends Plugin with DecoderPrediction{

  val RAS_PUSH_PTR = Stageable(UInt(log2Up(rasDepth) bits))
  val RAS_POP_PTR  = Stageable(UInt(log2Up(rasDepth) bits))

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val rob = getService[RobService]
    val aligner = getService[AlignerPlugin]
    val branchContext = getService[BranchContextPlugin]
    val priority = JumpService.Priorities.DECODE_PREDICTION
    val decodeJump = jump.createJumpInterface(priority)
    val historyPush = getService[HistoryPlugin].createPushPort(priority, DISPATCH_COUNT)
    val decoder = getService[DecoderPlugin]

    frontend.retain()
    fetch.retain()
    rob.retain()
    branchContext.retain()

    decoder.addDecodingToRob(RAS_PUSH_PTR)
    decoder.addDecodingToRob(RAS_POP_PTR)
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
      val MISSMATCH_PC = Stageable(Bool())
      val MISSMATCH_HISTORY = Stageable(Bool())
      val MISSMATCH = Stageable(Bool())
      val IS_JAL    = Stageable(Bool())
      val IS_JALR   = Stageable(Bool())
      def IS_BRANCH = Prediction.IS_BRANCH
      val IS_ANY    = Stageable(Bool())
      val RAS_PUSH  = Stageable(Bool())
      val RAS_POP   = Stageable(Bool())
      val CAN_IMPROVE = Stageable(Bool())
      val BRANCHED_PREDICTION = Stageable(Bool())
      val NEED_CORRECTION = Stageable(Bool())
      val OFFSET = Stageable(SInt(PC_WIDTH bits))
      val CONDITIONAL_PREDICTION = Stageable(Bool())
      val LAST_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits))
      val BAD_RET_PC = Stageable(Bool())
    }
    import k._

    val decodeStage = decodeAt(frontend)
    val pcAddStage  = pcAddAt(frontend)
    val pcPredictionStage  = pcPredictionAt(frontend)
    val applyStage  = applyAt(frontend)

    if(!isServiceAvailable[FetchConditionalPrediction]){
      for(slotId <- 0 until Frontend.DECODE_COUNT) {
        frontend.pipeline.decompressed(CONDITIONAL_TAKE_IT, slotId) := (1 << SLICE_COUNT) - 1
      }
    }

    val ras = new Area{
      val mem = new Area{
        val stack = Mem.fill(rasDepth)(PC)
        if(GenerationFlags.simulation){
          stack.initBigInt(List.fill(stack.wordCount)(BigInt(0)))
        }
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

      //Restore the RAS ptr on reschedules
      val reschedule = commit.reschedulingPort(onCommit = false)
      val healPush = rob.readAsyncSingle(RAS_PUSH_PTR, reschedule.robId)
      val healPop  = rob.readAsyncSingle(RAS_POP_PTR , reschedule.robId)
      when(reschedule.valid){
        ptr.push := healPush
        ptr.pop  := healPop
      }
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
            True  -> imm.j_sext
          ).resized

          val rdLink  = List(1,5).map(decoder.ARCH_RD === _).orR
          val rs1Link = List(1,5).map(decoder.ARCH_RS(0) === _).orR
          val rdEquRs1 = decoder.ARCH_RD === decoder.ARCH_RS(0)
          RAS_PUSH  := (IS_JAL || IS_JALR) && rdLink
          RAS_POP   := IS_JALR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)

          LAST_SLICE := PC(SLICE_RANGE) + INSTRUCTION_SLICE_COUNT
          CONDITIONAL_PREDICTION := decodeStage(CONDITIONAL_TAKE_IT, slotId)(LAST_SLICE)
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

          BAD_RET_PC := RAS_POP && ras.read =/= ALIGNED_BRANCH_PC_NEXT
          CAN_IMPROVE := !IS_JALR || RAS_POP
          BRANCHED_PREDICTION := IS_BRANCH && CONDITIONAL_PREDICTION || IS_JAL || IS_JALR
        }

        val applyIt = new Area{
          val stage = applyStage
          import applyStage._

          PC_TARGET := PC_TARGET_PRE_RAS
          when(IS_JALR){ PC_TARGET := S(ras.read) }
          PC_PREDICTION := BRANCHED_PREDICTION ? stage(PC_TARGET, slotId) otherwise PC_INC

          val badTaken = IS_ANY ? (BRANCHED_PREDICTION =/= ALIGNED_BRANCH_VALID) otherwise ALIGNED_BRANCH_VALID
          MISSMATCH_PC := badTaken || BAD_RET_PC   // ALIGNED_BRANCH_VALID =/= BRANCHED_PREDICTION // || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(PC_PREDICTION)
          //val historyPushed = BRANCH_HISTORY_PUSH_VALID && BRANCH_HISTORY_PUSH_SLICE === LAST_SLICE
          MISSMATCH_HISTORY := False //historyPushed =/= IS_BRANCH || IS_BRANCH && BRANCH_HISTORY_PUSH_VALUE =/= CONDITIONAL_PREDICTION
          //MISSMATCH_HISTORY Will improve the branch hit rate, but will also reduce the fetch bandwidth in cases it wasn't realy necessary

          MISSMATCH := MISSMATCH_PC || MISSMATCH_HISTORY
          NEED_CORRECTION := DECODED_MASK && CAN_IMPROVE && MISSMATCH
          if(flushOnBranch) MISSMATCH setWhen(IS_BRANCH)

          branchContext.keys.BRANCH_SEL         := IS_ANY
          when(NEED_CORRECTION){
            branchContext.keys.BRANCH_EARLY.taken := BRANCHED_PREDICTION
            branchContext.keys.BRANCH_EARLY.pc    := U(PC_TARGET)
          } otherwise {
            branchContext.keys.BRANCH_EARLY.taken := ALIGNED_BRANCH_VALID
            branchContext.keys.BRANCH_EARLY.pc    := ALIGNED_BRANCH_PC_NEXT
          }

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
          RAS_PUSH_PTR := ras.ptr.push
          RAS_POP_PTR  := ras.ptr.pop
        }
      }



      val applyIt = new Area{
        val stage = applyStage
        import applyStage._

        val slotIds = (0 until DECODE_COUNT)
        val hit = slotIds.map(stage(NEED_CORRECTION, _)).orR
        val selOh = OHMasking.first(slotIds.map(stage(NEED_CORRECTION, _)))
        val applySideEffects = isFireing
        val firstCycle = RegInit(True) clearWhen(isValid) setWhen(isReady || isFlushed)

        setup.decodeJump.valid := isValid && hit  //IsValid instead of applySideEffects to avoid propagating the ready path
        setup.decodeJump.pc := U(MuxOH(selOh, (0 until DECODE_COUNT).map(stage(PC_PREDICTION, _))))

        setup.historyPush.flush := setup.decodeJump.valid

        ras.ptr.pushIt    clearWhen(!applySideEffects)
        ras.ptr.popIt     clearWhen(!applySideEffects)


        flushIt(setup.decodeJump.valid && isReady, root = false)

        //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
        for (slotId <- 0 until Frontend.DECODE_COUNT) {
          stage(Frontend.DISPATCH_MASK, slotId) := stage(Frontend.DECODED_MASK, slotId) && !stage(0 until slotId)(NEED_CORRECTION).orR
        }

        for (slotId <- 0 until Frontend.DECODE_COUNT) {
          val slot = slots(slotId)
          implicit val _ = StageableOffset(slotId)
          setup.historyPush.mask(slotId) := isValid && firstCycle && IS_BRANCH && Frontend.DECODED_MASK && !stage(0 until slotId)(NEED_CORRECTION).orR
          setup.historyPush.taken(slotId) := BRANCHED_PREDICTION
        }
      }
    }

    val onDispatch = new Area{
      val stage = frontend.pipeline.dispatch
      import stage._
      rob.write(IS_BRANCH, DISPATCH_COUNT, stage(0 until DISPATCH_COUNT)(IS_BRANCH),  ROB.ID, isFireing)

      branchContext.dispatchWrite(
        IS_BRANCH,
        CONDITIONAL_TAKE_IT
      )
    }

    rob.release()
    frontend.release()
    fetch.release()
    branchContext.release()
  }
}