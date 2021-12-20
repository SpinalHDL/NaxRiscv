package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK}
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
import spinal.lib.pipeline.{Stageable, StageableOffset}

class DecodePredictionPlugin() extends Plugin{
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
      val stage = frontend.pipeline.decoded
      val stagePrevious = frontend.pipeline.aligned
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
      branchDecoder.addNeeds(branchKeys, Masked.one)
      jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
      jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
      anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)

      import stage._

      var rasPushUsed = False
      var rasPopUsed = False
      val slots = for (slotId <- 0 until Frontend.DECODE_COUNT) yield new Area {
        implicit val _ = StageableOffset(slotId)

        def inst = Frontend.INSTRUCTION_DECOMPRESSED

        val isJal = jalDecoder.build(inst, decoder.covers())
        val isJalR = jalrDecoder.build(inst, decoder.covers())
        val isBranch = branchDecoder.build(inst, decoder.covers())
        val isAny = anyDecoder.build(inst, decoder.covers())
        val rdLink  = List(1,5).map(decoder.ARCH_RD === _).orR
        val rs1Link = List(1,5).map(decoder.ARCH_RS(0) === _).orR
        val rdEquRs1 = decoder.ARCH_RD === decoder.ARCH_RS(0)
        val rasPush = (isJal || isJalR) && rdLink
        val rasPop  = isJalR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)

        val imm = IMM(inst)
        val offset = Frontend.INSTRUCTION_DECOMPRESSED(2).mux(
          False -> imm.b_sext,
          True -> imm.j_sext
        )

        val lastSlice = PC(SLICE_RANGE) + INSTRUCTION_SLICE_COUNT
        val conditionalPrediction =  CONDITIONAL_TAKE_IT(lastSlice)//TODO pipeline it from earlier stage

        val slices = INSTRUCTION_SLICE_COUNT +^ 1
        val pcInc = S(PC + (slices << SLICE_RANGE_LOW))
        val pcTarget = S(PC) + offset
        when(isJalR){ pcTarget := S(ras.read) }
        val canImprove = !isJalR || rasPop
        val branchedPrediction = isBranch && conditionalPrediction || isJal || isJalR
        val pcPrediction = branchedPrediction ? pcTarget otherwise pcInc
        val pcNext = canImprove ?  U(pcPrediction) otherwise ALIGNED_BRANCH_PC_NEXT
        val missmatch = !ALIGNED_BRANCH_VALID && branchedPrediction || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(pcPrediction)
        val needCorrection = Frontend.DISPATCH_MASK && canImprove && missmatch

        branchContext.keys.BRANCH_SEL := isAny
        branchContext.keys.BRANCH_EARLY.pcNext := pcNext
        keys.BRANCH_CONDITIONAL := isBranch

        when(stage.resulting(DISPATCH_MASK, slotId) && rasPush) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
          when(!rasPushUsed){
            ras.write.data := U(pcInc)
          }
          rasPushUsed \= True
        }
        when(stage.resulting(DISPATCH_MASK, slotId) && rasPop) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
          rasPopUsed \= True
        }
      }
      when(isFireing){
        ras.ptr.pushIt    setWhen(rasPushUsed)
        ras.ptr.popIt     setWhen(rasPopUsed)
      }

      val hit = slots.map(_.needCorrection).orR
      val selOh = OHMasking.first(slots.map(_.needCorrection))
      setup.decodeJump.valid := isFireing && hit
      setup.decodeJump.pc := U(MuxOH(selOh, slots.map(_.pcPrediction)))

      flushIt(setup.decodeJump.valid, root = false)

      //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
      for (slotId <- 1 until Frontend.DECODE_COUNT) {
        stage.overloaded(Frontend.DISPATCH_MASK, slotId) := stage(Frontend.DISPATCH_MASK, slotId)&& !(0 until slotId).map(i => slots(i).needCorrection).orR
      }

      for (slotId <- 0 until Frontend.DECODE_COUNT) {
        val slot = slots(slotId)
        setup.historyPush.mask(slotId) := isFireing && slot.isBranch && (Frontend.DISPATCH_MASK, slotId) && B(slots.take(slotId).map(s => s.needCorrection)) === 0
        setup.historyPush.taken(slotId) := slot.branchedPrediction
      }
    }

    val update = new Area{
      val stage = frontend.pipeline.dispatch
      import stage._
      rob.write(keys.BRANCH_CONDITIONAL, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(keys.BRANCH_CONDITIONAL, _)),  ROB.ID, isFireing)

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