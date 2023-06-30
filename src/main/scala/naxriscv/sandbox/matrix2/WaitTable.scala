// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.sandbox.matrix2

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.FDRE
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer


case class WaitTableParameter( slotCount : Int,
                               wayCount : Int,
                               eventCount : Int,
                               selCount : Int){
  assert(slotCount % wayCount == 0)
  val lineCount = slotCount/wayCount
  val eventType = HardType(Bits(eventCount bits))

}

case class WaitTablePush[T <: Data](p : WaitTableParameter, contextType : HardType[T]) extends Bundle{
  val line = Bits(p.lineCount bits)
  val slots = Vec.fill(p.wayCount)(WaitTablePushSlot(p, contextType))
}

case class WaitTablePushSlot[T <: Data](p : WaitTableParameter, contextType : HardType[T]) extends Bundle{
  val valid = Bool()
  val event = p.eventType()
  val context = contextType()
}

case class WaitTableIo[T <: Data](p : WaitTableParameter, slotContextType : HardType[T]) extends Bundle{
  val events = KeepAttribute(in(p.eventType()))
  val push = slave Stream(WaitTablePush(p, slotContextType))
  val pop = Vec.fill(p.slotCount)(master Stream(slotContextType()))
}

//Extra light in lut, but don't pack FF
class WaitTable[T <: SlotSelectorContext](p : WaitTableParameter, slotContextType : HardType[T]) extends Component{
  val io = WaitTableIo(p, slotContextType)

  io.push.ready := False

  val lines = for(line <- 0 until p.lineCount) yield new Area{
    val ways = for(way <- 0 until p.wayCount) yield new Area{
      val priority = line*p.wayCount+way
      val pop = io.pop(priority)
      val valid = RegInit(False)
      val context = Reg(slotContextType())
      val triggers = Reg(p.eventType())
      val done = triggers === 0
      val popComb = valid && done
      val popValid = RegNext(popComb) clearWhen(pop.ready)
      val popSel = RegNext(popComb ? context.sel | B(0))
      when(pop.ready){ popSel := 0}

      pop.valid := popValid
      pop.sel := popSel
    }
    val free = !ways.map(_.valid).orR
    val refill = KeepAttribute(io.push.valid && free && io.push.line(line))

    //Push in the table
    when(refill) {
      io.push.ready := True
      for (wayId <- 0 until p.wayCount) {
        val wSrc = io.push.slots(wayId)
        val wDst = ways(wayId)
        wDst.valid := wSrc.valid
        wDst.context := wSrc.context
        wDst.triggers := wSrc.event
      }
    }

    ways.foreach(w => w.valid clearWhen(w.pop.ready))
  }

  //Apply io events
  for(eventId <- 0 until p.eventCount) when(io.events(eventId)){
    lines.foreach(_.ways.foreach(_.triggers(eventId) := False))
  }
}


//Lut5 not infered
//class WaitTable[T <: Data](p : WaitTableParameter, slotContextType : HardType[T]) extends Component{
//  val io = WaitTableIo(p, slotContextType)
//
//
//  val lines = for(line <- 0 until p.lineCount) yield new Area{
//    val ways = for(way <- 0 until p.wayCount) yield new Area{
//      val priority = line*p.wayCount+way
//      val pop = io.pop(priority)
//      val valid = RegInit(False)
//      val context = Reg(slotContextType())
//      val triggers = p.eventType()
////      val triggers = Reg(p.eventType())
//      val done = triggers === 0
//      val triggersFF = Array.fill(p.eventCount)(FDRE())
//      triggersFF.foreach(_.CE := True)
//      (triggers.asBools, triggersFF).zipped.foreach(_ := _.Q)
//
//      pop.valid := valid && done
//      pop.payload := context
//    }
//    val free = !ways.map(_.valid).orR
//    val refill = KeepAttribute(io.push.valid && free && io.push.line(line))
//
//    //Push in the table
//    for (wayId <- 0 until p.wayCount) {
//      val wSrc = io.push.slots(wayId)
//      val wDst = ways(wayId)
//
//      when(refill) {
//        wDst.valid := wSrc.valid
//        wDst.context := wSrc.context
//      }
//
//      //      wDst.triggers := KeepAttribute(wDst.triggers | (wSrc.event & B(p.eventCount bits, default -> refill)))
////      (wDst.triggersFF, (wDst.triggers | (wSrc.event & B(p.eventCount bits, default -> refill))).asBools).zipped.foreach(_.D := _)
//      for(i <- 0 until p.eventCount){
//        wDst.triggersFF(i).D := (refill && wSrc.event(i)) ||  wDst.triggersFF(i).Q
//      }
//    }
//    ways.foreach(w => w.valid clearWhen(w.pop.ready))
//  }
//
//
//  io.push.ready := lines.map(_.refill).orR
//
//  //Apply io events
////  for(eventId <- 0 until p.eventCount) when(io.events(eventId)){
////    lines.foreach(_.ways.foreach(_.triggers(eventId) := False))
////  }
//  for(eventId <- 0 until p.eventCount){
//    lines.foreach(_.ways.foreach(_.triggersFF(eventId).R := io.events(eventId)))
//  }
//}

object WaitTableSynthBench extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  val p = WaitTableParameter(
    slotCount    = 32,
    wayCount     = 2,
    eventCount   = 32,
    selCount = 2
  )
//  rtls += Rtl(SpinalVerilog((new WaitTable(p, Bits(log2Up(p.slotCount) bits)))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}

/*
Artix 7 -> 148 Mhz 1378 LUT 2230 FF
Artix 7 -> 267 Mhz 1381 LUT 2230 FF

*******
Artix 7 -> 211 Mhz 433 LUT 431 FF
Artix 7 -> 353 Mhz 447 LUT 431 FF

Artix 7 -> 182 Mhz 433 LUT 731 FF
Artix 7 -> 344 Mhz 446 LUT 731 FF

Artix 7 -> 171 Mhz 816 LUT 923 FF
Artix 7 -> 311 Mhz 831 LUT 923 FF
*/