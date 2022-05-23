package naxriscv.utilities

import spinal.core._
import spinal.core.fiber.AsyncThread
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AddressToMask{
  def apply(address : UInt, size : UInt, width : Int) : Bits ={
    size.muxListDc((0 to log2Up(width)).map(i => U(i) -> B((1 << (1 << i)) -1, width bits))) |<< address(log2Up(width)-1 downto 0)
  }
}

class Reservation{
  class Entry(val priority : Int) extends Area{
    val win = Bool()
    val take = False

    def takeIt() = take := True
  }
  val model = ArrayBuffer[Entry]()
  def create(priority : Int) : Entry = {
    val e = new  Entry( priority)
    model += e
    e
  }

//  val allowMultiplesAt = mutable.LinkedHashSet[Int]()
  Component.current.afterElaboration{
//    assert(model.map(_.priority).distinct.size == model.size)
    for(e <- model){
      e.win := !model.filter(_.priority < e.priority).map(_.take).orR
    }
  }
}

object MulSpliter{
  case class Splits(offsetA : Int, offsetB : Int, widthA : Int, widthB : Int, signedA : Boolean, signedB : Boolean, id : Int){
    val offsetC = offsetA+offsetB
    val widthC = widthA + widthB
    val endC = offsetC+widthC
  }

  def splits(inWidthA : Int, inWidthB : Int, splitWidthA : Int, splitWidthB : Int, signedA : Boolean, signedB : Boolean) = {
    val outWidth = inWidthA + inWidthB
    val splitsUnordered = for (offsetA <- 0 until inWidthA by splitWidthA;
                               offsetB <- 0 until inWidthB by splitWidthB;
                               widthA = (inWidthA - offsetA) min splitWidthA;
                               widthB = (inWidthB - offsetB) min splitWidthB) yield {
      Splits(offsetA, offsetB, widthA, widthB, offsetA + widthA == inWidthA, offsetB + widthB == inWidthB, -1)
    }
    val splits = splitsUnordered.sortWith(_.endC < _.endC).zipWithIndex.map(e => e._1.copy(id = e._2))
    splits
  }

}