package spinal.lib.logic

import spinal.core.internals.Literal
import spinal.core.{BaseType, Bits, Component, EnumLiteral, False, HardType, RegInit, RegNext, SpinalEnumCraft, out}
import spinal.lib.KeepAttribute

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class DecodingSpec[HT <: HardType[_ <: BaseType]](key : HT){
  var default : Option[Masked] = None
  val needs = mutable.LinkedHashMap[Masked, Masked]() //key, value

  def setDefault(value : Masked) = default match {
    case Some(x) => ???
    case None => default = Some(value)
  }
  def addNeeds(key : Masked, value : Masked): Unit = needs.get(key) match {
    case Some(x) => ???
    case None => needs(key) = value
  }

  def build(sel : Bits, coverAll : Seq[Masked]) : Bits = {
    val defaultsKeys = mutable.LinkedHashSet[Masked]()
    defaultsKeys ++= coverAll
    defaultsKeys --= needs.keys
    val defaultNeeds = default match {
      case Some(x) => defaultsKeys.map(_ -> x)
      case None => Nil
    }
    val finalSpec = needs ++ defaultNeeds
    Symplify(sel, finalSpec, key.getBitsWidth)
  }
}