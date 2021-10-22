package naxriscv.frontend

import naxriscv.interfaces.DecoderService
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
  val defaults = mutable.LinkedHashMap[Stageable[_ <: BaseType], BaseType]()
  val encodings = mutable.LinkedHashMap[MaskedLiteral,ArrayBuffer[(Stageable[_ <: BaseType], BaseType)]]()

  override def add(encoding: Seq[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]): Unit = encoding.foreach(e => this.add(e._1,e._2))
  override def add(key: MaskedLiteral, values: Seq[(Stageable[_ <: BaseType], Any)]): Unit = {
    val instructionModel = encodings.getOrElseUpdate(key,ArrayBuffer[(Stageable[_ <: BaseType], BaseType)]())
    values.map{case (a,b) => {
      assert(!instructionModel.contains(a), s"Over specification of $a")
      val value = b match {
        case e: SpinalEnumElement[_] => e()
        case e: BaseType => e
      }
      instructionModel += (a->value)
    }}
  }

  override def addDefault(key: Stageable[_  <: BaseType], value: Any): Unit = {
    assert(!defaults.contains(key))
    defaults(key) = value match{
      case e : SpinalEnumElement[_] => e()
      case e : BaseType => e
    }
  }
}