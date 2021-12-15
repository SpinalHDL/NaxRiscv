package naxriscv.frontend

import naxriscv.Fetch._
import naxriscv.backend.BranchContextPlugin
import naxriscv.fetch.{AlignerPlugin, FetchPlugin}
import naxriscv.{Frontend, ROB}
import naxriscv.interfaces.{AddressTranslationService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.logic.{DecodingSpec, DecodingSpecExample, Masked}
import spinal.lib.pipeline.{Stageable, StageableOffset}

class PredictorPlugin() extends Plugin{
  val btbJumpAt = 1

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val rob = getService[RobService]
    val PC = getService[AddressTranslationService].PC
    val decodeJump = jump.createJumpInterface(JumpService.Priorities.DECODE_PREDICTION)
    val btbJump = jump.createJumpInterface(JumpService.Priorities.FETCH_WORD(btbJumpAt, true))
    frontend.retain()
    fetch.retain()
    rob.retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val decoder = getService[DecoderPlugin]
    val rob = getService[RobService]
    val branchContext = getService[BranchContextPlugin]
    val PC = getService[AddressTranslationService].PC
    val ak = getService[AlignerPlugin].keys.get
    import ak._

    val FETCH_PC = fetch.keys.FETCH_PC_PRE_TRANSLATION

    val sliceShift = if(RVC) 1 else 2

    val btb = new Area{
      val btbDepth = 8096
      val hashWidth = 16
      val wordBytesWidth =log2Up(FETCH_DATA_WIDTH/8)
      assert(FETCH_DATA_WIDTH.get == 32) //also doesn't support rvc
      def getHash(value : UInt) = value(wordBytesWidth, hashWidth bits) //TODO better hash
      case class BtbEntry() extends Bundle {
        val hash = UInt(hashWidth bits)
        val slice  = UInt(log2Up(SLICE_COUNT) bits)
        val pcNext = PC()
      }

      val mem = Mem.fill(btbDepth)(BtbEntry())

      val onLearn = new Area{
        val event = branchContext.logic.free.learn
        val hash = getHash(event.pc)
        val port = mem.writePort
        port.valid := event.valid
        port.address := (event.pc >> wordBytesWidth).resized
        port.data.hash := hash
        port.data.slice := (event.pc >> sliceShift).resized
        port.data.pcNext := event.pcNext
      }

      val read = new Area{
        val stage = fetch.getStage(btbJumpAt-1)
        import stage._
        val hash = getHash(FETCH_PC)
        val entryAddress = (FETCH_PC >> wordBytesWidth).resize(mem.addressWidth)
      }
      val applyIt = new Area{
        val stage = fetch.getStage(btbJumpAt)
        import stage._
        val entry = mem.readSync(read.entryAddress, read.stage.isReady)
        val hit = isValid && entry.hash === getHash(FETCH_PC)
        flushNext(hit)
        setup.btbJump.valid := hit
        setup.btbJump.pc := entry.pcNext

        WORD_BRANCH_VALID := hit
        WORD_BRANCH_SLICE := entry.slice
        WORD_BRANCH_PC_NEXT := entry.pcNext
      }
    }

    val onDecode = new Area {
      val stage = frontend.pipeline.decoded
      val stagePrevious = frontend.pipeline.aligned
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
      branchDecoder.addNeeds(branchKeys, Masked.one)
      jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
      jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
      anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)

      import stage._

      val slots = for (slotId <- 0 until Frontend.DECODE_COUNT) yield new Area {
        implicit val _ = StageableOffset(slotId)

        def inst = Frontend.INSTRUCTION_DECOMPRESSED

        val isJal = jalDecoder.build(inst, decoder.covers())
        val isJalR = jalrDecoder.build(inst, decoder.covers())
        val isBranch = branchDecoder.build(inst, decoder.covers())
        val isAny = anyDecoder.build(inst, decoder.covers())

        val imm = IMM(inst)

        val offset = Frontend.INSTRUCTION_DECOMPRESSED(2).mux(
          False -> imm.b_sext,
          True -> imm.j_sext
        )

        val slices = INSTRUCTION_SLICE_COUNT +^ 1
        val pcInc = S(PC + (slices << sliceShift))
        val pcTarget = S(PC) + offset
        val canImprove = isJal// || isBranch
        val branchedPrediction = offset.msb || isJal
        val pcPrediction = branchedPrediction ? pcTarget otherwise pcInc
        val pcNext = canImprove ?  U(pcPrediction) otherwise ALIGNED_BRANCH_PC_NEXT
        val missmatch = ALIGNED_BRANCH_VALID =/= branchedPrediction || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(pcTarget)
        val needCorrection = Frontend.DISPATCH_MASK && isAny && canImprove && missmatch

        branchContext.keys.BRANCH_SEL := isAny
        branchContext.keys.BRANCH_EARLY.pcNext := pcNext
      }

      val hit = slots.map(_.needCorrection).orR
      val selOh = OHMasking.first(slots.map(_.needCorrection))
      setup.decodeJump.valid := isFireing && hit
      setup.decodeJump.pc := U(MuxOH(selOh, slots.map(_.pcPrediction)))

      flushIt(setup.decodeJump.valid, root = false)

      //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
      for (slotId <- 1 until Frontend.DECODE_COUNT) {
        stage.overloaded(Frontend.DISPATCH_MASK) := Frontend.DISPATCH_MASK && !(0 until slotId).map(i => slots(i).needCorrection).orR
      }
    }

    rob.release()
    frontend.release()
    fetch.release()
  }
}