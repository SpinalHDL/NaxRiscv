// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.utilities

import naxriscv.interfaces.RegfileSpec
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

//object MulAggregator{
//  case class Splits(offset : Int, width : Int, budget : Int, var id : Int = 0, from : Seq[Int]){
//
//  }
//
//  def apply(splits : Seq[Splits], widthMax : Int, srcsMax : Int) = {
//    var srcs = splits.sortBy(_.offset).toList
//    val works = ArrayBuffer[Splits]()
//    srcs ++= splits
//    for((e,i) <- srcs.zipWithIndex) e.id = i
//
//    while(srcs.size >= 2) {
//    /*  var elements = srcs.take(srcsMax)
//      val a = elements(0)
//      val b = elements(1)
//      if(a.offset != b.offset){
//        //Bad seed
//        assert(a.offset < b.offset)
//        val frontPadding = b.offset - a.offset
//        if(frontPadding >= a.width){
//          works += a.copy(from = List(a.id))
//          srcs = srcs.tail
//        } else {
//          works += a.copy(width = frontPadding, from = List(a.id))
//          srcs = a.copy(offset = b.offset, width = a.width-frontPadding) :: srcs.tail
//        }
//      } else {
//        val offset = a.offset
//        val endMax = offset + widthMax
//        elements = elements.filter(_.offset < endMax)
//
//        if (a.offset + a.width <= adderOffset) {
//          //'a' is completly isolated
//          works += a.copy(from = List(a.id))
//          srcs = srcs.tail
//        } else {
//          val frontPadding = b.offset - a.offset
//          if (frontPadding != 0) {
//            works += a.copy(width = frontPadding, from = List(a.id))
//          }
//          //        val adderWidth = widthMax :: elements.map(e => e.offset +
//
//        }
//      }*/
//      srcs = srcs.sortBy(_.offset)
//      val a = srcs(0)
//      val b = srcs(1)
//
//      if(a.offset + a.width <= b.offset){
//        works += a.copy(from = List(a.id))
//        srcs = srcs.tail
//      } else {
//        val frontPadding = b.offset - a.offset
//        if (frontPadding != 0) {
//          works += a.copy(width = frontPadding, from = List(a.id))
//        }
//        val adderWidth = List(a.width-frontPadding, b.width, widthMax).min
//        works += Splits(offset = b.offset, width = adderWidth, from = List(a.id, b.id))
//        if(adderWidth != b.width){
//          srcs = b.copy(offset = b.offset + adderWidth, width = b.width - adderWidth) :: srcs
//        }
//        if(frontPadding + adderWidth != a.width){
//          srcs = a.copy(offset = a.offset + frontPadding + adderWidth, width = a.width - frontPadding - adderWidth) :: srcs
//        }
//      }
//    }
//
//    if(srcs.nonEmpty) {
//      works += srcs.head.copy(from = List(srcs.head.id))
//    }
//
//    works
//  }
//}

trait WithRfWriteSharedSpec{
  case class RfWriteSharingSpec(key : Any, withReady : Boolean, priority : Int)
  private val rfWriteSharing = mutable.LinkedHashMap[RegfileSpec, RfWriteSharingSpec]()
  def addRfWriteSharing(rf : RegfileSpec, key : Any, withReady : Boolean, priority : Int) : this.type = {
    assert(!rfWriteSharing.contains(rf))
    rfWriteSharing(rf) = RfWriteSharingSpec(key, withReady, priority)
    this
  }
  def getRfWriteSharing(rf : RegfileSpec) = rfWriteSharing.getOrElseUpdate(rf,  RfWriteSharingSpec(new {}, false, 0))
}