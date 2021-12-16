package naxriscv.fetch

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.Fetch._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{AddressTranslationService, JumpService}
import naxriscv.utilities.Plugin
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{ConnectionLogic, ConnectionPoint, Stageable}

class AlignerPlugin() extends Plugin{

//  class CustomConnector extends ConnectionLogic{
//    override def on(m: ConnectionPoint, s: ConnectionPoint, flush: Bool, flushNext : Bool, flushNextHit : Bool) = new Area {
//      val l = logic.get
//      import l._
//      assert(flushNext == null)
//      s.valid := extractors.map(_.valid).orR
//      if(m.ready != null) m.ready := fireInput
//      if(flush != null) when(flush){
//        buffer.mask := 0
//      }
//      (s.payload, m.payload).zipped.foreach(_ := _)
//    }
//  }

  val keys = create early new AreaRoot{
    val ALIGNED_BRANCH_VALID = Stageable(Bool())
    val ALIGNED_BRANCH_PC_NEXT = Stageable(getService[AddressTranslationService].PC)

    val WORD_BRANCH_VALID = Stageable(Bool())
    val WORD_BRANCH_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits)) //TODO implement me
    val WORD_BRANCH_PC_NEXT = Stageable(getService[AddressTranslationService].PC)
  }

  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    val frontend = getService[FrontendPlugin]
    val jump = getService[JumpService]
    fetch.retain()
    frontend.retain()

    val sequenceJump = jump.createJumpInterface(JumpService.Priorities.ALIGNER)
    val buffer = fetch.pipeline.newStage()
    fetch.pipeline.connect(fetch.pipeline.stages.last, buffer)(new M2S()) //WARNING, maskGen pipeline is assuming there is a buffer stage (fmax)
//    frontend.pipeline.connect(buffer, frontend.pipeline.aligned)(new CustomConnector)
  }

  val MASK_BACK, MASK_FRONT = Stageable(Bits(FETCH_DATA_WIDTH/SLICE_WIDTH bits))
  val logic = create late new Area {
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val PC = getService[AddressTranslationService].PC
    val input = setup.buffer
    val output = setup.frontend.pipeline.aligned

    import input._


    val sliceRangeLow = if (RVC) 1 else 2
    val sliceRange = (sliceRangeLow + log2Up(SLICE_COUNT) - 1) downto sliceRangeLow
    val maskGen = new Area {
      val maskStage = fetch.pipeline.stages.last
      import maskStage._
      MASK_FRONT := B((0 until SLICE_COUNT).map(i => B((1 << SLICE_COUNT) - (1 << i), SLICE_COUNT bits)).read(fetch.keys.FETCH_PC_PRE_TRANSLATION(sliceRange)))
      MASK_BACK  := B((0 until SLICE_COUNT).map(i => B((2 << i)-1, SLICE_COUNT bits)).read(keys.WORD_BRANCH_SLICE))
      when(!keys.WORD_BRANCH_VALID){ MASK_BACK.setAll() }
    }

    val buffer = new Area {
      val data = Reg(WORD)
      val mask = Reg(MASK_FRONT) init (0)
      val pc   = Reg(PC())
      val branchValid = Reg(keys.WORD_BRANCH_VALID())
      val branchSlice = Reg(keys.WORD_BRANCH_SLICE())
      val branchPcNext = Reg(keys.WORD_BRANCH_PC_NEXT())
    }

    val slices = new Area {
      val data = (WORD ## buffer.data).subdivideIn(SLICE_WIDTH bits)
      var mask = (input.isValid ? input(MASK_FRONT) | B(0)) ## buffer.mask //Which slice have valid data
      var used = B(0, SLICE_COUNT*2 bits)
    }

    val decoders = for (i <- 0 until SLICE_COUNT * 2) yield new Area {
      val rvc = slices.data(i)(1 downto 0) =/= 3

      def mask16 = B(1 << i, SLICE_COUNT*2 bits)
      def mask32 = B(3 << i, SLICE_COUNT*2 bits)

      val usage = if(i == SLICE_COUNT*2 - 1)
        mask16
      else
        rvc ? mask16 | mask32

      val notEnoughData = if(i == SLICE_COUNT * 2 - 1)
        !rvc
      else if(i == SLICE_COUNT - 1)
        !rvc && !MASK_FRONT.lsb
      else
        False

      val pastPrediction = if(i <= SLICE_COUNT) False else keys.WORD_BRANCH_VALID && U(i - SLICE_COUNT) > keys.WORD_BRANCH_SLICE //TODO may use MASK_BACK instead for timings ?
      val usable = !notEnoughData && !pastPrediction
    }

    //This ensure that the input word isn't cut by some bad prediction aliasing (prediction from another part of the programm that had the same hash)
    val predictionSanity = new Area{
      val sliceMasks = (MASK_FRONT & MASK_BACK) ## buffer.mask
      var skip = False
      val skips = Bits(SLICE_COUNT*2 bits)
      val errors = Bits(SLICE_COUNT*2 bits)
      for(i <- 0 until SLICE_COUNT*2){ //TODO optimize, could skip the first SLICE_COUNT entry if their results was stored in the buffer
        errors(i) := skip && !sliceMasks(i)
        skip = !skip && sliceMasks(i) && !decoders(i).rvc
        skips(i) := skip
      }

      val backMaskError = errors(SLICE_COUNT, SLICE_COUNT bits).orR
      val partialFetchError = keys.WORD_BRANCH_SLICE.andR && skip
      val postPredictionPc = fetch.keys.FETCH_PC_PRE_TRANSLATION(sliceRange) > keys.WORD_BRANCH_SLICE
      val failure = input.isValid && keys.WORD_BRANCH_VALID && (backMaskError || partialFetchError || postPredictionPc) //TODO check that it only set in the right cases (as it can silently produce false positive)

      when(backMaskError){
        for(decoder <- decoders.drop(SLICE_COUNT)){
          decoder.pastPrediction := False
        }
      }
    }

    val extractors = for (i <- 0 until DECODE_COUNT) yield new Area {
      val maskOh = OHMasking.firstV2(slices.mask.drop(i))
      val usage = MuxOH.or(maskOh, decoders.drop(i).map(_.usage))
      val usable = MuxOH.or(maskOh, decoders.drop(i).map(_.usable))
      val rvc = MuxOH.or(maskOh, decoders.drop(i).map(_.rvc))
      val slice0 = MuxOH.or(maskOh, slices.data.drop(i))
      val slice1 = MuxOH.or(maskOh.dropHigh(1), slices.data.drop(i + 1))
      val instruction = slice1 ## slice0
      val valid = slices.mask.drop(i).orR && usable
      slices.used \= slices.used | usage
      slices.mask \= slices.mask & ~usage
      output(INSTRUCTION_ALIGNED,i) := instruction
      output(MASK_ALIGNED, i) := valid
      output(INSTRUCTION_SLICE_COUNT, i) := (if(RVC) U(!rvc) else U(0))

      val sliceLast = output(PC, i)(sliceRange) + U(!rvc)
      val bufferPredictionLast = buffer.branchSlice     === sliceLast
      val inputPredictionLast  = keys.WORD_BRANCH_SLICE === sliceLast
      val lastWord = maskOh.drop(SLICE_COUNT-i).orR || rvc && maskOh(SLICE_COUNT-i-1)
      when(lastWord) {
        output(keys.ALIGNED_BRANCH_VALID, i)   := keys.WORD_BRANCH_VALID && !predictionSanity.failure && inputPredictionLast
        output(keys.ALIGNED_BRANCH_PC_NEXT, i) := keys.WORD_BRANCH_PC_NEXT
      } otherwise {
        output(keys.ALIGNED_BRANCH_VALID, i)   := buffer.branchValid && bufferPredictionLast
        output(keys.ALIGNED_BRANCH_PC_NEXT, i) := buffer.branchPcNext
      }

      val sliceOffset = OHToUInt(maskOh << i)
      val pcWord = Vec(buffer.pc, input(fetch.keys.FETCH_PC_PRE_TRANSLATION)).read(U(sliceOffset.msb))
      output(PC, i) := (pcWord >> sliceRange.high+1) @@ U(sliceOffset.dropHigh(1)) @@ U(0, sliceRangeLow bits)
    }

    val fireOutput = CombInit(output.isFireing)
    val fireInput = isValid && buffer.mask === 0 || fireOutput && slices.mask(0, SLICE_COUNT bits) === 0 //WARNING NEED RVC FIX this seems to not handle unaligned none RVC


    when(fireOutput){
      buffer.mask := slices.mask(0, SLICE_COUNT bits)
    }

    val postMask = MASK_BACK.orMask(predictionSanity.failure)
    when(fireInput){
      buffer.mask := (fireOutput ? slices.mask(SLICE_COUNT, SLICE_COUNT bits) otherwise MASK_FRONT) & postMask
      buffer.data := WORD
      buffer.pc   := fetch.keys.FETCH_PC_PRE_TRANSLATION
      buffer.branchValid  := keys.WORD_BRANCH_VALID && !predictionSanity.failure
      buffer.branchSlice  := keys.WORD_BRANCH_SLICE
      buffer.branchPcNext := keys.WORD_BRANCH_PC_NEXT
    }

    val correctionSent = RegInit(False) setWhen(setup.sequenceJump.valid) clearWhen(input.isReady)
    fetch.getLastStage.flushIt(predictionSanity.failure && !correctionSent)
    setup.sequenceJump.valid := predictionSanity.failure && !correctionSent
    setup.sequenceJump.pc    := input(keys.WORD_BRANCH_PC_NEXT)



//    val fireOutput = CombInit(output.isFireing)
//    val fireInput = False
//    when(buffer.mask === 0 || fireOutput && slices.mask(0, SLICE_COUNT bits) === 0) {
//      buffer.mask := slices.mask(SLICE_COUNT, SLICE_COUNT bits)
//      buffer.data := WORD
//      fireInput := True
//    }

    output.valid := extractors.map(_.valid).orR
    input.haltIt(!fireInput)
    input.flushIt(output.isFlushed, root = false)
    when(output.isFlushed){
      buffer.mask := 0
    }

    setup.frontend.release()
    setup.fetch.release()
  }
}

