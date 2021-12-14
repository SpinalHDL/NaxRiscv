package naxriscv.frontend

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.AddressTranslationService
import naxriscv.utilities.Plugin
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{ConnectionLogic, ConnectionPoint, Stageable}

class AlignerPlugin() extends Plugin{

  class CustomConnector extends ConnectionLogic{
    override def on(m: ConnectionPoint, s: ConnectionPoint, flush: Bool, flushNext : Bool, flushNextHit : Bool) = new Area {
      val l = logic.get
      import l._
      assert(flushNext == null)
      s.valid := extractors.map(_.valid).orR
      if(m.ready != null) m.ready := fireInput
      if(flush != null) when(flush){
        buffer.mask := 0
      }
      (s.payload, m.payload).zipped.foreach(_ := _)
    }
  }

  val keys = create early new AreaRoot{
    val ALIGNED_BRANCH_VALID = Stageable(Bool())
    val ALIGNED_BRANCH_PC_NEXT = Stageable(getService[AddressTranslationService].PC)

    val WORD_BRANCH_VALID = Stageable(Bool())
    val WORD_BRANCH_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits)) //TODO implement me
    val WORD_BRANCH_PC_NEXT = Stageable(getService[AddressTranslationService].PC)
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    val buffer = frontend.pipeline.newStage()
    frontend.pipeline.connect(frontend.pipeline.fetches.last, buffer)(new M2S())
    frontend.pipeline.connect(buffer, frontend.pipeline.aligned)(new CustomConnector)
  }

  val MASK = Stageable(Bits(FETCH_DATA_WIDTH/SLICE_WIDTH bits))
  val logic = create late new Area {
    val PC = getService[AddressTranslationService].PC
    val input = setup.buffer
    val output = setup.frontend.pipeline.aligned
    val frontend = getService[FrontendPlugin]

    import input._


    val sliceRangeLow = if (RVC) 1 else 2
    val sliceRange = (sliceRangeLow + log2Up(SLICE_COUNT) - 1) downto sliceRangeLow
    val _ = {
      val maskStage = frontend.getStage(1)
      import maskStage._
      MASK := (0 until SLICE_COUNT).map(i => B((1 << SLICE_COUNT) - (1 << i), SLICE_COUNT bits)).read(frontend.keys.FETCH_PC_PRE_TRANSLATION(sliceRange))
    }

    val buffer = new Area {
      val data = Reg(WORD)
      val mask = Reg(MASK) init (0)
      val pc   = Reg(PC())
      val branchValid = Reg(keys.WORD_BRANCH_VALID())
      val branchPcNext = Reg(keys.WORD_BRANCH_PC_NEXT())
    }

    val slices = new Area {
      val data = (WORD ## buffer.data).subdivideIn(SLICE_WIDTH bits)
      var mask = (input.isValid ? input(MASK) | B(0)) ## buffer.mask //Which slice have valid data
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
        !rvc && !MASK.lsb
      else
        False
    }


    val extractors = for (i <- 0 until DECODE_COUNT) yield new Area {
      val maskOh = OHMasking.firstV2(slices.mask.drop(i))
      val usage = MuxOH.or(maskOh, decoders.drop(i).map(_.usage))
      val notEnoughData = MuxOH.or(maskOh, decoders.drop(i).map(_.notEnoughData))
      val rvc = MuxOH.or(maskOh, decoders.drop(i).map(_.rvc))
      val slice0 = MuxOH.or(maskOh, slices.data.drop(i))
      val slice1 = MuxOH.or(maskOh.dropHigh(1), slices.data.drop(i + 1))
      val instruction = slice1 ## slice0
      val valid = slices.mask.drop(i).orR && !notEnoughData
      slices.used \= slices.used | usage
      slices.mask \= slices.mask & ~usage
      output(INSTRUCTION_ALIGNED,i) := instruction
      output(MASK_ALIGNED, i) := valid
      output(INSTRUCTION_SLICE_COUNT, i) := (if(RVC) U(!rvc) else U(0))

      val lastWord = maskOh.drop(SLICE_COUNT-i).orR || rvc && maskOh(SLICE_COUNT-i-1)
      when(lastWord) {
        output(keys.ALIGNED_BRANCH_VALID, i)   := input(keys.WORD_BRANCH_VALID)
        output(keys.ALIGNED_BRANCH_PC_NEXT, i) := input(keys.WORD_BRANCH_PC_NEXT)
      } otherwise {
        output(keys.ALIGNED_BRANCH_VALID, i)   := buffer.branchValid
        output(keys.ALIGNED_BRANCH_PC_NEXT, i) := buffer.branchPcNext
      }

      val sliceOffset = OHToUInt(maskOh << i)
      val pcWord = Vec(buffer.pc, output(frontend.keys.FETCH_PC_PRE_TRANSLATION)).read(U(sliceOffset.msb))
      output(PC, i) := (pcWord >> sliceRange.high+1) @@ U(sliceOffset.dropHigh(1)) @@ U(0, sliceRangeLow bits)
    }

    val fireOutput = CombInit(output.isFireing)
    val fireInput = isValid && buffer.mask === 0 || fireOutput && slices.mask(0, SLICE_COUNT bits) === 0

    when(fireOutput){
      buffer.mask := slices.mask(0, SLICE_COUNT bits)
    }
    when(fireInput){
      buffer.mask := fireOutput ? slices.mask(SLICE_COUNT, SLICE_COUNT bits) | MASK
      buffer.data := WORD
      buffer.pc   := frontend.keys.FETCH_PC_PRE_TRANSLATION
      buffer.branchValid := keys.WORD_BRANCH_VALID
      buffer.branchPcNext := keys.WORD_BRANCH_PC_NEXT
    }
//    val fireOutput = CombInit(output.isFireing)
//    val fireInput = False
//    when(buffer.mask === 0 || fireOutput && slices.mask(0, SLICE_COUNT bits) === 0) {
//      buffer.mask := slices.mask(SLICE_COUNT, SLICE_COUNT bits)
//      buffer.data := WORD
//      fireInput := True
//    }

    setup.frontend.release()
  }
}

