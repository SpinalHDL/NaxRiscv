package naxriscv.frontend

import naxriscv.Frontend
import naxriscv.interfaces.{AddressTranslationService, JumpService}
import naxriscv.riscv.{IMM, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.logic.{DecodingSpec, DecodingSpecExample, Masked}
import spinal.lib.pipeline.StageableOffset


class PredictorPlugin() extends Plugin{



  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val jump = getService[JumpService]
    val jumpPort = jump.createJumpInterface(JumpService.Priorities.PREDICTOR)
    frontend.retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val decoder = getService[DecoderPlugin]
    val PC = getService[AddressTranslationService].PC
    val sliceShift = if(Frontend.RVC) 1 else 2
    val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
    val branchDecoder, jalDecoder, jalrDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
    branchDecoder.addNeeds(branchKeys, Masked.one)
    jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
    jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)

    val stage = frontend.pipeline.decoded
    val stagePrevious = frontend.pipeline.aligned
    import stage._
    val slots = for(slotId <- 0 until Frontend.DECODE_COUNT) yield new Area{
      implicit val _ = StageableOffset(slotId)
      def inst = Frontend.INSTRUCTION_DECOMPRESSED
      val isJal =  jalDecoder.build(inst, decoder.covers())
      val isJalR =  jalrDecoder.build(inst, decoder.covers())
      val isBranch = branchDecoder.build(inst, decoder.covers())


      val imm = IMM(inst)

      val offset = Frontend.INSTRUCTION_DECOMPRESSED(2).mux(
        False -> imm.b_sext,
        True  -> imm.j_sext
      )

      val pcPostPrediction = S(PC) + offset
      val slices = Frontend.INSTRUCTION_SLICE_COUNT+^1
      val pcPrePredicted = S(PC + (slices << sliceShift)) //TODO
      val needCorrection = Frontend.DISPATCH_MASK && (isJal) && pcPrePredicted =/= pcPostPrediction
    }

    val hit = slots.map(_.needCorrection).orR
    val selOh = OHMasking.first(slots.map(_.needCorrection))
    setup.jumpPort.valid := isFireing && hit
    setup.jumpPort.pc := U(MuxOH(selOh, slots.map(_.pcPostPrediction)))

    flushIt(setup.jumpPort.valid, root = false)
//    stagePrevious.flushIt(setup.jumpPort.valid)

    frontend.release()
  }
}