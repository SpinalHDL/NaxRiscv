package naxriscv.misc

import naxriscv.Global
import spinal.core._
import spinal.lib._
import naxriscv.interfaces._
import naxriscv.utilities.Plugin
import spinal.core.fiber.Handle

import scala.collection.mutable.ArrayBuffer

class CsrRamPlugin extends Plugin with CsrRamService{
  val allocations = ArrayBuffer[CsrRamAllocation]()
  val reads = ArrayBuffer[Handle[CsrRamRead]]()
  val writes = ArrayBuffer[Handle[CsrRamWrite]]()

  override def ramAllocate(entries: Int = 1) = allocations.addRet(new CsrRamAllocation(entries))
  override def ramReadPort() = reads.addRet(Handle())
  override def ramWritePort() = writes.addRet(Handle())

  val setup = create late new Area{
    lock.await()

    val sorted = allocations.sortBy(_.entriesLog2).reverse
    var offset = 0
    for(alloc <- sorted){
      alloc.at = offset
      offset += alloc.entriesLog2
    }

    val addressWidth = log2Up(offset)

    for(alloc <- allocations) alloc.addressWidth = addressWidth
    for(read <- reads) read.load(CsrRamRead(addressWidth, Global.XLEN.get))
    for(write <- writes) write.load(CsrRamWrite(addressWidth, Global.XLEN.get))
  }

  val logic = create late new Area{
    setup.await()

    val addressWidth = setup.addressWidth
    val mem = Mem.fill(1 << addressWidth)(Bits(Global.XLEN bits))

    val writeLogic = new Area{
      val hits = writes.map(_.valid)
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val sel = OHToUInt(oh)
      val port = mem.writePort

      port.valid := hit
      port.address := writes.map(_.address).read(sel)
      port.data := writes.map(_.data).read(sel)
      (writes, oh).zipped.foreach(_.ready :=  _)
    }


    val readLogic = new Area{
      val hits = reads.map(_.valid)
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val sel = OHToUInt(oh)
      val port = mem.readAsyncPort


      port.address := reads.map(_.address).read(sel)
      (reads, oh).zipped.foreach(_.ready := _)
      reads.foreach(_.data := port.data)
    }
  }
}
