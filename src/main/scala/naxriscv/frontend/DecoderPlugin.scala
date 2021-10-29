package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{DecoderService, Encoding, ExecuteUnitService, RegfileService, Riscv}
import naxriscv.pipeline.Connection.DIRECT
import naxriscv.pipeline.{Stageable, _}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


//case class DecoderParameter(decoderWidth : Int,
//                            instructionWidth : Int)  {
//
//}

//case class DecompressedInstruction() extends Bundle {
//  val valid = Bool()
//  val instruction = InstructionWidth.craft()
//}
//
//
//case class DecoderIo() extends Bundle {
//  val instructions = slave Stream(Vec.fill(DecoderWidth)(DecompressedInstruction()))
//}
//
//case class Decoder() extends Component {
//
//}



class DecoderPlugin() extends Plugin with DecoderService{
//  val defaults = mutable.LinkedHashMap[Stageable[_ <: BaseType], BaseType]()
//  val encodings = mutable.LinkedHashMap[MaskedLiteral,ArrayBuffer[(Stageable[_ <: BaseType], BaseType)]]()
//
//  override def add(encoding: Seq[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]): Unit = encoding.foreach(e => this.add(e._1,e._2))
//  override def add(key: MaskedLiteral, values: Seq[(Stageable[_ <: BaseType], Any)]): Unit = {
//    val instructionModel = encodings.getOrElseUpdate(key,ArrayBuffer[(Stageable[_ <: BaseType], BaseType)]())
//    values.map{case (a,b) => {
//      assert(!instructionModel.contains(a), s"Over specification of $a")
//      val value = b match {
//        case e: SpinalEnumElement[_] => e()
//        case e: BaseType => e
//      }
//      instructionModel += (a->value)
//    }}
//  }
//
//  override def addDefault(key: Stageable[_  <: BaseType], value: Any): Unit = {
//    assert(!defaults.contains(key))
//    defaults(key) = value match{
//      case e : SpinalEnumElement[_] => e()
//      case e : BaseType => e
//    }
//  }
  override def add(key: MaskedLiteral, values: Seq[(Stageable[_ <: BaseType], Any)]) = ???
  override def add(encoding: Seq[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]) = ???
  override def addDefault(key: Stageable[_ <: BaseType], value: Any) = ???

  val encodings = mutable.LinkedHashMap[ExecuteUnitService, ArrayBuffer[Encoding]]()
  override def addFunction(fu: ExecuteUnitService, enc: Encoding) = {
    encodings.getOrElseUpdate(fu, ArrayBuffer[Encoding]()) += enc
  }


  override def EU_SEL() = setup.EU_SEL
//  override def WAIT_ROB_RS(id: Int) = logic.regfiles.WAIT_ROB_RS(id)
//  override def WAIT_ENABLE_RS(id: Int) = logic.regfiles.WAIT_ENABLE_RS(id)
  override def READ_RS(id: Int) = logic.regfiles.READ_RS(id)
  override def PHYSICAL_RS(id: Int) = logic.regfiles.ARCH_RS(id)
  override def WRITE_RD = logic.regfiles.WRITE_RD
  override def PHYSICAL_RD = logic.regfiles.ARCH_RD
  override def rsCount = logic.regfiles.rsCount
  override def rsPhysicalDepthMax = logic.regfiles.physicalMax

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.decompressed, frontend.pipeline.decoded)(new DIRECT)
    val EU_SEL = Stageable(Bits(getServicesOf[ExecuteUnitService].size bits))
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]

    val regfiles = new Area {
      val plugins = getServicesOf[RegfileService]
      val physicalMax = plugins.map(_.getPhysicalDepth).max
      val rsCount = 2
      val ARCH_RS = List.fill(rsCount)(Stageable(UInt(log2Up(physicalMax) bits)))
      val ARCH_RD = Stageable(UInt(log2Up(physicalMax) bits))
//      val WAIT_ROB_RS    = List.fill(rsCount)(Stageable(ROB.ID_TYPE()))
//      val WAIT_ENABLE_RS = List.fill(rsCount)(Stageable(Bool()))
      val READ_RS = List.fill(rsCount)(Stageable(Bool()))
      val WRITE_RD = Stageable(Bool)
    }

    val stage = frontend.pipeline.decoded
    import stage._

    for(i <- 0 until Frontend.DECODE_COUNT) {
      (regfiles.READ_RS(0), i) := False
      (regfiles.READ_RS(1), i) := False
      (regfiles.WRITE_RD, i) := False
      (setup.EU_SEL  , i) := B(0)
//      implicit val offset = new StageableOffset(i)
//      Riscv.READ_RS1 := False
//      Riscv.READ_RS2 := False
//      Riscv.WRITE_RD := False
//      setup.EU_SEL   := B(0)
    }

    frontend.release()
  }
}