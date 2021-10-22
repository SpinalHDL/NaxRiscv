package naxriscv.frontend

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import naxriscv.Global
import naxriscv.pipeline._

import scala.collection.mutable.ArrayBuffer
import naxriscv.frontend.Frontend._
import naxriscv.utilities.Plugin

//case class CompactorCmd() extends Bundle {
//  val data = WORD()
//  val mask = MASK()
//}
//
//case class CompactorRspInstruction() extends Bundle {
//  val valid = Bool()
//  val value = INSTRUCTION()
//}
//
//case class CompactorRsp() extends Bundle {
//  val instructions = Vec.fill(FETCH_COUNT)(CompactorRspInstruction())
//}
//
//
//
//
//class CompactorLogic extends Component {
//  val io = new Bundle {
//    val input = Stream(CompactorCmd())
//    val output = Stream(CompactorRsp())
//  }
//}

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

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.fetches.last, frontend.pipeline.aligned)(new CustomConnector)
  }

  val logic = create late new Area {
    val input = setup.frontend.pipeline.fetches.last

    import input._

    val buffer = new Area {
      val data = Reg(WORD)
      val mask = Reg(MASK) init (0)
    }

    val slices = new Area {
      //0 -> 1111  1 -> 0111 2 > 0011 ...
      val pcMaskLut = Vec.tabulate(SLICE_COUNT)(i => B((1 << SLICE_COUNT - i) - 1, SLICE_COUNT bits))
      val pcMask = pcMaskLut.read(FETCH_PC_VIRTUAL(log2Up(SLICE_BYTES), log2Up(SLICE_COUNT) bits))
      val data = (WORD ## buffer.data).subdivideIn(SLICE_WIDTH bits)
      var mask = ((MASK & pcMask) ## buffer.mask)
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
      val maskOh = OHMasking.firstV2(slices.mask.drop(i));
      val usage = MuxOH.or(maskOh, decoders.drop(i).map(_.usage))
      val notEnoughData = MuxOH.or(maskOh, decoders.drop(i).map(_.notEnoughData))
      val slice0 = MuxOH.or(maskOh, slices.data.drop(i))
      val slice1 = MuxOH.or(maskOh.dropHigh(1), slices.data.drop(i + 1))
      val instruction = slice1 ## slice0
      val valid = slices.mask.drop(i).orR && !notEnoughData
      slices.used \= slices.used | usage
      slices.mask \= slices.mask & ~usage
      INSTRUCTION_ALIGNED(i) := instruction
    }


    val fire = False
    val fireInput = False
    when(buffer.mask === 0 || fire && slices.mask(0, SLICE_COUNT bits) === 0) {
      buffer.mask := slices.mask(SLICE_COUNT, SLICE_COUNT bits)
      buffer.data := WORD
      fireInput := True
    }
//    when(input.isRemoved){
//      buffer.mask := 0
//    }
    setup.frontend.release()
  }

//  val injection = create late{
//    val output = setup.frontend.pipeline.aligned
//    output.valid := logic.extractors.map(_.valid).orR
//    logic.input.haltIt(!logic.fireInput)
//    logic.input.flushIt(output.isRemoved)
//    logic.fire setWhen(output.isFireing)
//
//    output(ALIGNED_INSTRUCTION) := Vec(logic.extractors.map(_.instruction))
//    setup.frontend.release()
//  }

}

//
//class Compactor() extends Plugin{
//
//  val setup = create early new Area{
//    val frontend = getService(classOf[WordPipeline])
//    frontend.lock.retain()
//  }
//
//  val logic = create late{
//    import Frontend._
//    val stage = setup.frontend.getStage(3)
//    val pipeline = setup.frontend.getPipeline()
//    import stage._
//
//    val buffer = new Area{
//      val data = Reg(WORD)
//      val mask = Reg(MASK) init(0)
//    }
//
//    val slices = new Area{
//      //0 -> 1111  1 -> 0111 2 > 0011 ...
//      val pcMaskLut = Vec.tabulate(SLICE_COUNT)(i => B((1 << SLICE_COUNT-i)-1, SLICE_COUNT bits))
//      val pcMask = pcMaskLut.read(PC(log2Up(SLICE_BYTES), log2Up(SLICE_COUNT) bits))
//      val data = (WORD ## buffer.data).subdivideIn(SLICE_WIDTH bits)
//      var mask = ((MASK & pcMask) ## buffer.mask)
//      var used = B(0, SLICE_COUNT bits)
//    }
//
//    val decoders = for(i <- 0 until SLICE_COUNT*2) yield new Area{
//      val rvc = slices.data(i)(1 downto 0) =/= 3
//      def mask16 = B(1 << i, SLICE_COUNT bits)
//      def mask32 = B(3 << i, SLICE_COUNT bits)
//      val usage = i match {
//        case SLICE_COUNT-1 => mask16
//        case _ => rvc ? mask16 | mask32
//      }
//      val incomplet = i match {
//        case SLICE_COUNT*2-1 => !rvc
//        case SLICE_COUNT-1   => !rvc && !MASK.lsb
//        case _ => False
//      }
//    }
//
//
//    val extractors = for(i <- 0 until DECODE_COUNT) yield new Area{
//      val maskOh    = OHMasking.firstV2(slices.mask.drop(i));
//      val usage     = MuxOH.or(maskOh, decoders.drop(i).map(_.usage))
//      val incomplet = MuxOH.or(maskOh, decoders.drop(i).map(_.incomplet))
//      val slice0 = MuxOH.or(maskOh, slices.data.drop(i))
//      val slice1 = MuxOH.or(maskOh.dropHigh(1), slices.data.drop(i+1))
//      val instruction = slice1 ## slice0
//      slices.used \= slices.used |  usage
//      slices.mask \= slices.mask & ~usage
//    }
//
//
//    val fire : Bool = ???
//    when(buffer.mask === 0 || fire && slices.mask(0, SLICE_COUNT bits) === 0){
//      buffer.mask := slices.mask(SLICE_COUNT, SLICE_COUNT bits)
//      buffer.data := WORD
//    }
//
//    setup.frontend.lock.release()
//  }
//}
