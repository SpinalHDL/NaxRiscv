package naxriscv.frontend

import naxriscv.backend.BranchContextPlugin
import naxriscv.{Frontend, ROB}
import naxriscv.interfaces.{AddressTranslationService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.logic.{DecodingSpec, DecodingSpecExample, Masked}
import spinal.lib.pipeline.{Stageable, StageableOffset}

class PredictorPlugin() extends Plugin{


  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val jump = getService[JumpService]
    val rob = getService[RobService]
    val PC = getService[AddressTranslationService].PC
    val jumpPort = jump.createJumpInterface(JumpService.Priorities.PREDICTOR)
    frontend.retain()
    rob.retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val decoder = getService[DecoderPlugin]
    val rob = getService[RobService]
    val branchContext = getService[BranchContextPlugin]
    val PC = getService[AddressTranslationService].PC

    val sliceShift = if(Frontend.RVC) 1 else 2
    val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
    val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
    branchDecoder.addNeeds(branchKeys, Masked.one)
    jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
    jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
    anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)

    val stage = frontend.pipeline.decoded
    val stagePrevious = frontend.pipeline.aligned
    import stage._
    val slots = for(slotId <- 0 until Frontend.DECODE_COUNT) yield new Area{
      implicit val _ = StageableOffset(slotId)
      def inst = Frontend.INSTRUCTION_DECOMPRESSED
      val isJal =  jalDecoder.build(inst, decoder.covers())
      val isJalR =  jalrDecoder.build(inst, decoder.covers())
      val isBranch = branchDecoder.build(inst, decoder.covers())
      val isAny = anyDecoder.build(inst, decoder.covers())

      val imm = IMM(inst)

      val offset = Frontend.INSTRUCTION_DECOMPRESSED(2).mux(
        False -> imm.b_sext,
        True  -> imm.j_sext
      )

      val slices = Frontend.INSTRUCTION_SLICE_COUNT+^1
      val pcInc = S(PC + (slices << sliceShift))
      val pcTarget = S(PC) + offset
      val branchedPrediction = offset.msb || isJal
      val pcNext = CombInit(pcInc) //TODO, ok for now as there is no predictor before decode
      val pcPrediction = branchedPrediction ? pcTarget otherwise pcInc
      val needCorrection = Frontend.DISPATCH_MASK && isAny && pcNext =/= pcPrediction

      branchContext.keys.BRANCH_SEL := isAny
      branchContext.keys.BRANCH_EARLY.pcNext := U(pcPrediction)
    }

    val hit = slots.map(_.needCorrection).orR
    val selOh = OHMasking.first(slots.map(_.needCorrection))
    setup.jumpPort.valid := isFireing && hit
    setup.jumpPort.pc := U(MuxOH(selOh, slots.map(_.pcPrediction)))

    flushIt(setup.jumpPort.valid, root = false)

    //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
    for(slotId <- 1 until Frontend.DECODE_COUNT){
      stage.overloaded(Frontend.DISPATCH_MASK) := Frontend.DISPATCH_MASK && !(0 until slotId).map(i => slots(i).needCorrection).orR
    }

    rob.release()
    frontend.release()
  }
}