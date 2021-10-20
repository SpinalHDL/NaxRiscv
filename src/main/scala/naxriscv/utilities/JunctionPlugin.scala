//package vooxriscv.utilities
//
//import spinal.core._
//import vooxriscv.pipeline.Stageable
//
//import scala.collection.mutable
//
//class JunctionKey extends Nameable
//
//class Junction extends Area{
//  val elements = mutable.LinkedHashMap[Stageable[Data], Data]()
//
//  def apply[T <: Data](key : Stageable[T]) = elements.getOrElseUpdate(key.asInstanceOf[Stageable[Data]], Misc.outsideCondScope(key.craft().setCompositeName(this, key.getName())))
//}
//
//class JunctionPlugin extends Plugin{
//  val junctions = mutable.LinkedHashMap[Any, Junction]()
//  def apply(key : Nameable) = junctions.getOrElseUpdate(key, new Junction().setCompositeName(key))
//}
