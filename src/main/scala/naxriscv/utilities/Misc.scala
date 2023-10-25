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
  case class Splits(offsetA : Int, offsetB : Int,
                    widthA : Int, widthB : Int,
                    signedA : Boolean, signedB : Boolean, id : Int){
    val offsetC = offsetA+offsetB
    val widthC = if(widthB != 1) widthA + widthB else widthA
    val endC = offsetC+widthC
    def signedC = signedA || signedB

    def toMulU(srcA : Bits, srcB : Bits, signedWidth : Int): UInt = {
      val a = srcA(offsetA, widthA bits)
      val b = srcB(offsetB, widthB bits)
      val sw = signedWidth - offsetC
      (signedA, signedB) match {
        case (false, false) => if(widthOf(b) != 1) U(a) * U(b) else U(a).andMask(b.asBool)
        case (false, true) => (S(False ## a) * S(b)).resize(sw bits).asUInt
        case (true, false) => (S(a) * S(False ## b)).resize(sw bits).asUInt
        case (true, true) => (S(a) * S(b)).resize(sw).asUInt
      }
    }
  }

  def splits(inWidthA : Int, inWidthB : Int,
             splitWidthA : Int, splitWidthB : Int,
             signedA : Boolean, signedB : Boolean) = {
    val outWidth = inWidthA + inWidthB
    val splitsUnordered = for (offsetA <- 0 until inWidthA by splitWidthA;
                               offsetB <- 0 until inWidthB by splitWidthB;
                               widthA = (inWidthA - offsetA) min splitWidthA;
                               widthB = (inWidthB - offsetB) min splitWidthB) yield {
      Splits(offsetA, offsetB,
        widthA, widthB,
        signedA && offsetA + widthA == inWidthA,
        signedB && offsetB + widthB == inWidthB,
        -1
      )
    }
    val splits = splitsUnordered.sortWith(_.endC < _.endC).zipWithIndex.map(e => e._1.copy(id = e._2))
    splits
  }
}

/**
 * Facility to add together a large number of values in a optimized / staged manner
 * Able to cut / concat sources to build multiple adders
 */

object AdderAggregator {
  def Source(s : MulSpliter.Splits, signedWidth : Int) : Source = {
    val width = if(s.signedC) signedWidth - s.offsetC else s.widthC
    Source(s.offsetC, (BigInt(1) << width)-1)
  }
  case class Source(offset: Int, localMax: BigInt) extends OverridedEqualsHashCode{
    var offsetTmp = offset
    val width = log2Up(localMax + 1)

    def offsetNext = offset + width

    override def toString = s"Source($offset, $width bits, $localMax)"
  }

  case class LaneSource(s : Source){
    val offset = s.offsetTmp

    def valueMax(offset: Int, width: Int) = {
      val widthOverflow = (s.offsetNext) - (offset + width)
      val fixedValue = if (widthOverflow > 0) (BigInt(1) << s.width - widthOverflow) - 1 else s.localMax
      val shiftedTmp = fixedValue >> (this.offset - s.offset)
      val delta = this.offset - offset
      val shifted = if (delta > 0) shiftedTmp << delta else shiftedTmp >> -delta
      shifted
    }
  }

  case class Lane(from: Seq[LaneSource]) {
    def valueMax(offset: Int, width: Int) = from.map(_.valueMax(offset, width)).sum
    def craft(offset: Int, width: Int, s2u: scala.collection.Map[Source, UInt]): UInt = {
      val ret = U(0, width bits).allowOverride()
      for(s <- from){
        val l = Math.max(s.offset, offset)
        val h = Math.min(s.s.offsetNext, offset + width) - 1
        ret(h-offset downto l-offset) := s2u(s.s)(h-s.s.offset downto l-s.s.offset)
      }
      ret
    }
  }

  case class Adder(offset: Int, width: Int, lanes: Seq[Lane]) {
    def toSource() = {
      val source = Source(offset, lanes.map(_.valueMax(offset, width)).sum)
      source
    }

    def craft(s2u : scala.collection.Map[Source, UInt]): UInt = {
      val sRet = toSource()
      val uLanes = lanes.map(_.craft(offset, width, s2u).resize(sRet.width bits))
      uLanes.reduceBalancedTree(_ + _)
    }
  }


  def apply(splits: Seq[Source], widthMax: Int, lanesMax: Int): Seq[Adder] = {
    var srcs = ArrayBuffer[Source]()
    val adders = ArrayBuffer[Adder]()
    srcs ++= splits.sortBy(_.offset)
    srcs.foreach(s => s.offsetTmp = s.offset)

    while (srcs.size != 0) {
      for (i <- srcs.indices.dropRight(1)) assert(srcs(i).offsetTmp <= srcs(i + 1).offsetTmp)
      // Check if the have other srcs in range
      if (srcs.size == 1 || srcs(0).offsetNext <= srcs(1).offsetTmp) {
        val a = srcs.head
        adders += Adder(a.offsetTmp, a.offsetNext - a.offsetTmp, List(Lane(List(LaneSource(a)))))
        srcs.remove(0)
      } else {
        val adderOffset = srcs(0).offsetTmp
        val logicOffset = srcs(1).offsetTmp
        val logicOffsetNext = logicOffset + widthMax
        val lanes = ArrayBuffer[Lane]()
        // Build lanes
        while (lanes.size < lanesMax && srcs.nonEmpty && srcs(0).offsetTmp < logicOffsetNext) {
          val from = ArrayBuffer[LaneSource]()
          val lane = Lane(from)
          lanes += lane
          var ptr = adderOffset
          var continue = false
          // Build a lane
          do {
            continue = false
            val iSource = srcs.indexWhere(e => e.offsetTmp >= ptr)
            if (iSource != -1 && srcs(iSource).offsetTmp < logicOffsetNext) {
              continue = true;
              val source = srcs(iSource)
              ptr = source.offsetNext
              from += LaneSource(source)
              source.offsetTmp = logicOffsetNext
              srcs.remove(iSource)
              if (source.offsetTmp < source.offsetNext) {
                // soure need to stay
                val iTarget = srcs.indexWhere(source.offsetTmp < _.offsetTmp) match {
                  case -1 => srcs.size
                  case v => v
                }
                srcs.insert(iTarget, source)
              }
            }
          } while (continue)
        }

        val adderOffsetNext = lanes.map(_.from.map(_.s.offsetNext).max).max min logicOffsetNext
        val adder = Adder(
          offset = adderOffset,
          width = adderOffsetNext - adderOffset,
          lanes = lanes
        )
        adders += adder
      }
    }

    adders
  }

  def main(args: Array[String]): Unit = {
    import spinal.core.sim._
//    var sources = ArrayBuffer[Source]()
//    sources += Source(0,   255)
//    sources += Source(0,   255)
//
//    for(i <- 0 until 8){
//      val adders = AdderAggregator(sources, 4, 4)
//      println(adders.mkString("\n"))
//      println("------------")
//      sources.clear()
//      sources ++= adders.map(_.toSource())
//    }

    val aw = 65
    val bw = 65
    val cw = aw + bw
    SimConfig.withFstWave.compile(new Component{
      val doSigned = true
      val a = in Bits (aw bits)
      val b = in Bits (bw bits)
      val aSigned = out(S(a))
      val bSigned = out(S(b))
      val c = out Bits (cw bits)
      val cSigned = out(S(c))

      val splitsSpec = MulSpliter.splits(
        aw, bw,
//        3, 7,
        17, 17,
        doSigned, doSigned
      )
      val muls = splitsSpec.map(_.toMulU(a, b, cw))
      val sourceToSignal = mutable.LinkedHashMap[AdderAggregator.Source, UInt]()
      var sourcesSpec = splitsSpec.map(s => AdderAggregator.Source(s, cw)).toList
      for((s, m) <- (sourcesSpec, muls).zipped) sourceToSignal(s) = m

      var stepCounter = 0
      class Step() extends Area {
        val ss = sourcesSpec
        var addersSpec = stepCounter match {
//          case 0 => AdderAggregator(sourcesSpec, 17, 2)
          case _ => AdderAggregator(sourcesSpec, 64, 4)
        }
        val adders = addersSpec.map(_.craft(sourceToSignal))
        sourcesSpec = addersSpec.map(_.toSource()).toList
        for ((s, m) <- (sourcesSpec, adders).zipped) sourceToSignal(s) = m
        println(addersSpec.mkString("\n"))
        println("------------")
        stepCounter += 1
      }
      val stepsBuffer = ArrayBuffer[Step]()
      while(sourcesSpec.size != 1) stepsBuffer += new Step()
      val steps = stepsBuffer

      c := sourceToSignal(sourcesSpec(0)).asBits.resized


    }).doSim{ dut =>
      import dut.{a,b,c}
      def check(x : BigInt, y : BigInt): Unit = {
        a #= x
        b #= y
        sleep(1)
        val ar = if(dut.doSigned) dut.aSigned.toBigInt else dut.a.toBigInt
        val br = if(dut.doSigned) dut.bSigned.toBigInt else dut.b.toBigInt
        val ref = ar*br
        val got = if(dut.doSigned) dut.cSigned.toBigInt else dut.c.toBigInt
        assert(got == ref, f"$got%x $ref%x")
      }

      for(i <- 0 until aw; i2 <- 0 until bw){
        check(BigInt(1) << i, BigInt(1) << i2)
      }

      for(i <- 0 until 100000){
        check(a.randomizedBigInt(), b.randomizedBigInt())
      }
    }

  }
}

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