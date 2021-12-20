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
import naxriscv.interfaces.{AddressTranslationService, JumpService, LockedImpl}
import naxriscv.utilities.{Plugin, Service}
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.Stageable
import naxriscv.prediction.Prediction._

import scala.collection.mutable

trait FetchWordPrediction extends Service
trait FetchConditionalPrediction extends Service

class AlignerPlugin(inputAt : Int) extends Plugin with FetchPipelineRequirements{


  override def stagesCountMin = inputAt + 1

  val addedWordContext = mutable.LinkedHashSet[Stageable[_ <: Data]]()
  def addWordContext(key : Stageable[_ <: Data]*): Unit = addedWordContext ++= key

  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    val frontend = getService[FrontendPlugin]
    val jump = getService[JumpService]
    fetch.retain()
    frontend.retain()

    val sequenceJump = jump.createFetchJumpInterface(JumpService.Priorities.ALIGNER, inputAt)

    if(!isServiceAvailable[FetchWordPrediction]){
      val stage = fetch.getStage(inputAt-1)
      import stage._
      WORD_BRANCH_VALID := False //TODO instead of tidding it low, we should remove its usage conditionaly ? seems so painefull todo XD
      WORD_BRANCH_SLICE := 0
      WORD_BRANCH_PC_NEXT.assignDontCare()
    }
  }

  val MASK_BACK, MASK_FRONT = Stageable(Bits(FETCH_DATA_WIDTH/SLICE_WIDTH bits))
  val logic = create late new Area {
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val input = fetch.getStage(inputAt)
    val output = setup.frontend.pipeline.aligned

    import input._

    val ignoreInput = CombInit(input.isSelfRemoved) // This is to ensure we don't propagate stuff futher if the instruction cache has a cache miss on that instruction
    val isInputValid = input.isValid && !ignoreInput
    val sliceRangeLow = if (RVC) 1 else 2
    val sliceRange = (sliceRangeLow + log2Up(SLICE_COUNT) - 1) downto sliceRangeLow
    val maskGen = new Area {
      val maskStage = fetch.getStage(inputAt-1)
      import maskStage._
      MASK_FRONT := B((0 until SLICE_COUNT).map(i => B((1 << SLICE_COUNT) - (1 << i), SLICE_COUNT bits)).read(FETCH_PC(sliceRange)))
      MASK_BACK  := B((0 until SLICE_COUNT).map(i => B((2 << i)-1, SLICE_COUNT bits)).read(WORD_BRANCH_SLICE))
      when(!WORD_BRANCH_VALID){ MASK_BACK.setAll() }
    }

    val buffer = new Area {
      val data = Reg(WORD)
      val mask = Reg(MASK_FRONT) init (0)
      val pc   = Reg(PC())
      val branchValid = Reg(WORD_BRANCH_VALID())
      val branchSlice = Reg(WORD_BRANCH_SLICE())
      val branchPcNext = Reg(WORD_BRANCH_PC_NEXT())
      val wordContexts = addedWordContext.map(Reg(_))
    }

    val slices = new Area {
      val data = (WORD ## buffer.data).subdivideIn(SLICE_WIDTH bits)
      var mask = (isInputValid ? input(MASK_FRONT) | B(0)) ## buffer.mask //Which slice have valid data
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

      val pastPrediction = if(i <= SLICE_COUNT) False else WORD_BRANCH_VALID && U(i - SLICE_COUNT) > WORD_BRANCH_SLICE //TODO may use MASK_BACK instead for timings ?
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

      val backMaskError = errors(SLICE_COUNT, SLICE_COUNT bits).orR // The prediction is cutting on of the non RVC instruction (doesn't check last slice)
      val partialFetchError = WORD_BRANCH_SLICE.andR && skip  // The prediction is cutting the last slice non rvc instruction
      val postPredictionPc = FETCH_PC(sliceRange) > WORD_BRANCH_SLICE // The prediction was for an instruction before the input start PC
      val failure = isInputValid && WORD_BRANCH_VALID && (backMaskError || partialFetchError || postPredictionPc) //TODO check that it only set in the right cases (as it can silently produce false positive)

      when(backMaskError || postPredictionPc){
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
      val inputPredictionLast  = WORD_BRANCH_SLICE === sliceLast
      val lastWord = maskOh.drop(SLICE_COUNT-i).orR || rvc && maskOh(SLICE_COUNT-i-1)
      when(lastWord) {
        output(ALIGNED_BRANCH_VALID, i)   := WORD_BRANCH_VALID && !predictionSanity.failure && inputPredictionLast
        output(ALIGNED_BRANCH_PC_NEXT, i) := WORD_BRANCH_PC_NEXT
        for(key <- addedWordContext) output(key, i).assignFrom(input(key))
      } otherwise {
        output(ALIGNED_BRANCH_VALID, i)   := buffer.branchValid && bufferPredictionLast
        output(ALIGNED_BRANCH_PC_NEXT, i) := buffer.branchPcNext
        for((value, key) <- (buffer.wordContexts, addedWordContext).zipped) output(key, i).assignFrom(value)
      }

      val sliceOffset = OHToUInt(maskOh << i)
      val pcWord = Vec(buffer.pc, input(FETCH_PC)).read(U(sliceOffset.msb))
      output(PC, i) := (pcWord >> sliceRange.high+1) @@ U(sliceOffset.dropHigh(1)) @@ U(0, sliceRangeLow bits)
    }

    val fireOutput = CombInit(output.isFireing)
    val fireInput = isInputValid && buffer.mask === 0 || fireOutput && slices.mask(0, SLICE_COUNT bits) === 0 //WARNING NEED RVC FIX this seems to not handle unaligned none RVC


    when(fireOutput){
      buffer.mask := slices.mask(0, SLICE_COUNT bits)
    }

    val postMask = MASK_BACK.orMask(predictionSanity.failure)
    when(fireInput){
      buffer.mask := (fireOutput ? slices.mask(SLICE_COUNT, SLICE_COUNT bits) otherwise MASK_FRONT) & postMask
      buffer.data := WORD
      buffer.pc   := FETCH_PC
      buffer.branchValid  := WORD_BRANCH_VALID && !predictionSanity.failure
      buffer.branchSlice  := WORD_BRANCH_SLICE
      buffer.branchPcNext := WORD_BRANCH_PC_NEXT
      for((reg, key) <- (buffer.wordContexts, addedWordContext).zipped) reg := key
    }

    val correctionSent = RegInit(False) setWhen(setup.sequenceJump.valid) clearWhen(input.isReady || input.isFlushed)
    fetch.getStage(inputAt-1).flushIt(predictionSanity.failure && !correctionSent)
    setup.sequenceJump.valid := predictionSanity.failure && !correctionSent
    setup.sequenceJump.pc    := input(FETCH_PC_INC)



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

