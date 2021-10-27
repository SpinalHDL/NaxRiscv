package naxriscv.frontend

import naxriscv.interfaces.{DecoderService, Encoding, ExecuteUnitService, Riscv}
import naxriscv.pipeline._
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


  override def getEuSel() = setup.EU_SEL

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
    val EU_SEL = Stageable(Bits(getServicesOf[ExecuteUnitService].size bits))
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    println("Encodings : ")
    println(encodings)

    val stage = frontend.pipeline.decoded
    import stage._

    for(i <- 0 until Frontend.DECODE_COUNT) {
      (Riscv.READ_RS1, i) := False
      (Riscv.READ_RS2, i) := False
      (Riscv.WRITE_RD, i) := False
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