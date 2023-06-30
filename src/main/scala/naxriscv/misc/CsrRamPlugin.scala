// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.misc

import naxriscv.Global
import spinal.core._
import spinal.lib._
import naxriscv.interfaces.{CsrRamService, _}
import naxriscv.utilities.Plugin
import spinal.core.fiber.Handle

import scala.collection.mutable.ArrayBuffer

class CsrRamPlugin extends Plugin with CsrRamService with InitCycles {
  val allocations = ArrayBuffer[CsrRamAllocation]()
  val reads = ArrayBuffer[Handle[CsrRamRead]]()
  val writes = ArrayBuffer[Handle[CsrRamWrite]]()

  override def ramAllocate(entries: Int = 1) : CsrRamAllocation= allocations.addRet(new CsrRamAllocation(entries))
  override def ramReadPort(priority : Int) : Handle[CsrRamRead] = reads.addRet(Handle(CsrRamRead(setup.addressWidth, Global.XLEN.get, priority)))
  override def ramWritePort(priority : Int)  : Handle[CsrRamWrite] = writes.addRet(Handle(CsrRamWrite(setup.addressWidth, Global.XLEN.get, priority)))

  override def initCycles = 1 << (setup.addressWidth+1)

  val setup = create late new Area{
    allocationLock.await()

    val initPort = ramWritePort(CsrRamService.priority.INIT)

    val sorted = allocations.sortBy(_.entriesLog2).reverse
    var offset = 0
    for(alloc <- sorted){
      alloc.at = offset
      offset += alloc.entriesLog2
    }

    val addressWidth = log2Up(offset)

    for(alloc <- allocations) alloc.addressWidth = addressWidth
  }

  val logic = create late new Area{
    portLock.await()
    val ws = writes.sortBy(_.priority).reverse
    val rs = reads.sortBy(_.priority).reverse
    writes.clear(); writes ++= ws
    reads.clear() ; reads  ++= rs

    val addressWidth = setup.addressWidth
    val mem = Mem.fill(1 << addressWidth)(Bits(Global.XLEN bits))

    val flush = new Area{
      val counter = Reg(UInt(log2Up(mem.wordCount)+1 bits)) init(0)
      val done = counter.msb

      setup.initPort.valid  := !done
      setup.initPort.address := counter.resized
      setup.initPort.data := 0
      when(!done && setup.initPort.ready){
        counter := counter + 1
      }
    }

    val writeLogic = new Area{
      val hits = writes.map(_.valid).asBits
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val sel = OHToUInt(oh)
      val port = mem.writePort

      port.valid := hit
      port.address := writes.map(_.address).read(sel)
      port.data := writes.map(_.data).read(sel)
      (writes, oh.asBools).zipped.foreach(_.ready :=  _)
    }


    val readLogic = new Area{
      val hits = reads.map(_.valid).asBits
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val sel = OHToUInt(oh)
      val port = mem.readAsyncPort


      port.address := reads.map(_.address).read(sel)
      (reads, oh.asBools).zipped.foreach(_.ready := _)
      reads.foreach(_.data := port.data)
    }
  }
}
