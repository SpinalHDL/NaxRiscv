// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.sandbox

import spinal.core._
import spinal.lib.{Flow, OHMasking, OHToUInt, UIntToOh}
import spinal.lib.blackbox.xilinx.s7.FDRE
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Stats extends App{
  val population = 8
  val spaceSize = 16
  val ways = 1
//  val population = 23
//  val spaceSize = 365


  var prob = 1.0
  for(i <- 0 until population){
    prob *= 1.0*(spaceSize-i)/spaceSize
  }
  println(1.0*(spaceSize-1)/spaceSize) //Chance for 1 item to not collide with 1 specific random item
  println(prob) //Chance for 1 item to not collide with a population

//  0.9375
//  0.12082010507583618

//  val loads = 16
//  val stores = 16
//  println(Math.pow(1-1.0/spaceSize, stores))
//  println(Math.pow(Math.pow(1-1.0/spaceSize, stores),loads))
}

object Stats2 extends App{
  val sets = 256
  val ways = 1
  val used = 16

  val setId = Random.nextInt(sets)
  var conflicts = 0
  val tries = 10000
  for(_ <- 0 until tries){
    var setFullness = 0
    for(_ <- 0 until used) if(Random.nextInt(sets) == setId) setFullness += 1
    if(setFullness >= ways) conflicts += 1
  }
  //Inserting 1 element
  println(f"Conflicts ${100.0f * conflicts / tries}")
}

object Stats3 extends App{
  val cacheSize = 16*1024
  val ways = 4
  val sets = cacheSize / 64 / ways
  val entries = 4*sets*ways
  val factor = entries/sets
  val state = Array.fill(entries)(0)
  for(s <- 0 until sets){
    for(w <- 0 until ways){
      state(Random.nextInt(factor)*sets+s) += 1
    }
  }

  val setId = Random.nextInt(sets)
  var conflicts = 0
  val tries = 10000
  for(_ <- 0 until tries){
    if(state(Random.nextInt(entries)) != 0) conflicts += 1
  }

  println(f"Conflicts ${100.0f * conflicts / tries}")
}


object SyntTest extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(new Component {
//    val entries = in(Vec.fill(16)(Bits(20 bits)))
//    val value = in Bits(20 bits)
//    val result = out(B(entries.map(_ === value)))

    val downPendingMax = 16
    val valids = Reg(Bits(downPendingMax bits)) init(0)
    val freeOh = OHMasking.firstV2(~valids)
    val freeId = OHToUInt(freeOh)
    val free = !valids.andR
    val allocate = Bool()
    val release = Flow(UInt(log2Up(downPendingMax) bits))
    valids := (valids | freeOh.andMask(allocate)) & (~UIntToOh(release.payload)).orMask(release.valid)
    in(allocate, release)
    out(free, freeId)
  }))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}



object SyntTest2 extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(new Component {
//    val rUp, event, col0, col1 = in Bool()
//    val r0 = out(Reg(Bool))
//    val r1 = out(Reg(Bool))
//
//    when(event){
//      r0 := False
//      r1 := False
//    } otherwise {
//      r0 := rUp && col0 || r0
//      r1 := r0 && col1 || r1
//    }

    val  event = in Bool()
    val logic = for(i <- 0 until 50) yield {
      val rUp, col0, col1 = in Bool()

      val r0, r1 = FDRE()
      r0.R := event
      r1.R := event
      r0.CE := True
      r1.CE := True

      val o0 = r0.Q.toIo()
      val o1 = r1.Q.toIo()

      r0.D := rUp && col0 || r0.Q
      r1.D := r0.Q && col1 || r1.Q
    }
  }))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}

//LUT6_2 #(
//.INIT(64’h0000000000000000) // Specify LUT Contents
//) LUT6_2_inst (
//.O6(O6), // 1-bit LUT6 output
//.O5(O5), // 1-bit lower LUT5 output
//.I0(I0), // 1-bit LUT input
//.I1(I1), // 1-bit LUT input
//.I2(I2), // 1-bit LUT input
//.I3(I3), // 1-bit LUT input
//.I4(I4), // 1-bit LUT input
//.I5(I5) // 1-bit LUT input (fast MUX select only available to O6 output)
//);



object SyntTest3 extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Component {
    val LSLEN = 64
    val sizeMax = log2Up(LSLEN/8)
    val rspSize = in UInt(2 bits)
    val rspShifted = in Bits(64 bits)
    val rspUnsigned = in Bool()
    val rspFormated = out(rspSize.muxListDc((0 to sizeMax).map{i =>
      val off = (1 << i) * 8
      i -> B((LSLEN - 1 downto off) -> (rspShifted(off-1) && !rspUnsigned), (off-1 downto 0) -> rspShifted(off-1 downto 0))
    }))
  })))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}


object RamPorts extends App{
  def xor(w : Int, r : Int) = w*(w-1)+r*w

  for(w <- List(2,3,4); r <- List(2,4,6)){
    println(s"w=$w r=$r XOR ${xor(w,r)}")
  }

}


/*
valid, tag, cached, cacheId, dirty, unique, owner, next
1+10+9
18+1+1+4
 */
object FallRandomly extends App {
  val cacheSize = 256*1024
  val cacheBlocks = cacheSize/64
  val shots = cacheBlocks * 9 / 8
  val sets = cacheBlocks
  val ways = 4

//  val ways = 8
//  val sets = cacheBlocks/ways

  val storage = Array.fill(sets)(0)
  for(i <- 0 until shots){
    storage(Random.nextInt(sets)) += 1
  }

  var sum = 0
  for(s <- storage) sum += s
  val avg = sum / sets.toFloat
  println(s"AVG => $avg")

  var latency = 0

//  def latOf(e: Int) = (1 + e * 2) - 2 max 0
  def latOf(e: Int) = ((e * 2).max(2)) - 2
//  def latOf(e: Int) = ((e * 2).max(1)) - 2

  latency = 0
  for(s <- storage) latency += latOf(s)
  println(s"Latency penality (miss) ${latency*1.0/sets}")

  latency = 0
  var nonEmpties = 0
  for (s <- storage) {
    for(i <- 1 to s) {
      latency += latOf(i)
      nonEmpties += 1
    }
  }
  println(s"Latency penality (hit) ${latency * 1.0 / nonEmpties}")

  var overWays = 0
  for(s <- storage) overWays += (s-ways).max(0)
  println(s"Overallocated $overWays")
  for(i <- 0 to 10){
    var count = 0
    for(s <- storage) if(s == i) count += 1
    println(s"$i => $count")
  }

}