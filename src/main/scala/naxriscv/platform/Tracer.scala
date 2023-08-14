package naxriscv.platform

import java.io.{BufferedWriter, File, FileWriter}
import spinal.core._
import spinal.core.sim._

class TraceIo (var write: Boolean,
               var address: Long,
               var data: Long,
               var mask: Int,
               var size: Int,
               var error: Boolean){
  def serialized() = f"${write.toInt} $address%016x $data%016x $mask%02x $size ${error.toInt}"
}
trait TraceBackend{
  def newCpuMemoryView(viewId : Int, readIds : Long, writeIds : Long)
  def newCpu(hartId : Int, isa : String, priv : String, physWidth : Int, memoryViewId : Int) : Unit
  def loadElf(offset : Long, path : File) : Unit
  def loadBin(offset : Long, path : File) : Unit
  def setPc(hartId : Int, pc : Long): Unit
  def writeRf(hardId : Int, rfKind : Int, address : Int, data : Long) //address out of range mean unknown
  def readRf(hardId : Int, rfKind : Int, address : Int, data : Long) //address out of range mean unknown
  def commit(hartId : Int, pc : Long): Unit
  def trap(hartId: Int, interrupt : Boolean, code : Int)
  def ioAccess(hartId: Int, access : TraceIo) : Unit
  def setInterrupt(hartId : Int, intId : Int, value : Boolean) : Unit

  def loadExecute(hartId: Int, id : Long, addr : Long, len : Long, data : Long) : Unit
  def loadCommit(hartId: Int, id : Long) : Unit
  def loadFlush(hartId: Int) : Unit
  def storeCommit(hartId: Int, id : Long, addr : Long, len : Long, data : Long) : Unit
  def storeBroadcast(hartId: Int, id : Long) : Unit
  def storeConditional(hartId : Int, failure: Boolean) : Unit

  def time(value : Long) : Unit

  def flush() : Unit
  def close() : Unit
}

class DummyBackend() extends TraceBackend{
  override def newCpuMemoryView(viewId: Int, readIds: Long, writeIds: Long) = {}
  override def newCpu(hartId: Int, isa: String, priv: String, physWidth: Int, memoryViewId: Int) = {}
  override def loadElf(offset: Long, path: File) = {}
  override def loadBin(offset: Long, path: File) = {}
  override def setPc(hartId: Int, pc: Long) = {}
  override def writeRf(hardId: Int, rfKind: Int, address: Int, data: Long) = {}
  override def readRf(hardId: Int, rfKind: Int, address: Int, data: Long) = {}
  override def commit(hartId: Int, pc: Long) = {}
  override def trap(hartId: Int, interrupt: Boolean, code: Int) = {}
  override def ioAccess(hartId: Int, access: TraceIo) = {}
  override def setInterrupt(hartId: Int, intId: Int, value: Boolean) = {}
  override def loadExecute(hartId: Int, id: Long, addr: Long, len: Long, data: Long) = {}
  override def loadCommit(hartId: Int, id: Long) = {}
  override def loadFlush(hartId: Int) = {}
  override def storeCommit(hartId: Int, id: Long, addr: Long, len: Long, data: Long) = {}
  override def storeBroadcast(hartId: Int, id: Long) = {}
  override def storeConditional(hartId: Int, failure: Boolean) = {}
  override def time(value: Long) = {}
  override def flush() = {}
  override def close() = {}
}

class FileBackend(f : File) extends TraceBackend{
  val bf = new BufferedWriter(new FileWriter(f))

  override def commit(hartId: Int, pc: Long) = {
    bf.write(f"rv commit $hartId $pc%016x\n")
  }

  override def trap(hartId: Int, interrupt : Boolean, code : Int): Unit ={
    bf.write(f"rv trap $hartId ${interrupt.toInt} $code\n")
  }

  override def writeRf(hartId: Int, rfKind: Int, address: Int, data: Long) = {
    bf.write(f"rv rf w $hartId $rfKind $address $data%016x\n")
  }

  override def readRf(hartId: Int, rfKind: Int, address: Int, data: Long) = {
    bf.write(f"rv rf r $hartId $rfKind $address $data%016x\n")
  }

  def ioAccess(hartId: Int, access : TraceIo) : Unit = {
    bf.write(f"rv io $hartId ${access.serialized()}\n")
  }

  override def setInterrupt(hartId: Int, intId: Int, value: Boolean) = {
    bf.write(f"rv int set $hartId $intId ${value.toInt}\n")
  }

  override def loadElf(offset: Long, path: File) = {
    bf.write(f"elf load  $offset%016x ${path.getAbsolutePath}\n")
  }

  override def loadBin(offset: Long, path: File) = {
    bf.write(f"bin load $offset%016x ${path.getAbsolutePath} \n")
  }
  override def setPc(hartId: Int, pc: Long) = {
    bf.write(f"rv set pc $hartId $pc%016x\n")
  }

  override def newCpuMemoryView(memoryViewId : Int, readIds : Long, writeIds : Long) = {
    bf.write(f"memview new $memoryViewId $readIds $writeIds\n")
  }

  override def newCpu(hartId: Int, isa : String, priv : String, physWidth : Int, memoryViewId : Int) = {
    bf.write(f"rv new $hartId $isa $priv $physWidth $memoryViewId\n")
  }

  override def loadExecute(hartId: Int, id : Long, addr : Long, len : Long, data : Long) : Unit = {
    bf.write(f"rv load exe $hartId $id $len $addr%016x $data%016x\n")
  }
  override def loadCommit(hartId: Int, id : Long) : Unit = {
    bf.write(f"rv load com $hartId $id\n")
  }
  override def loadFlush(hartId: Int) : Unit = {
    bf.write(f"rv load flu $hartId\n")
  }
  override def storeCommit(hartId: Int, id : Long, addr : Long, len : Long, data : Long) : Unit = {
    bf.write(f"rv store com $hartId $id $len $addr%016x $data%016x\n")
  }
  override def storeBroadcast(hartId: Int, id : Long) : Unit = {
    bf.write(f"rv store bro $hartId $id\n")
  }
  override def storeConditional(hartId: Int, failure: Boolean) = {
    bf.write(f"rv store sc $hartId ${failure.toInt}\n")
  }

  override def time(value: Long) = bf.write(f"time $value\n")


  override def flush() = bf.flush()
  override def close() = bf.close()

  def spinalSimFlusher(period : Long): Unit ={
    periodicaly(period)(flush())
    onSimEnd(close())
  }
  def spinalSimTime(period : Long): Unit ={
    periodicaly(period)(time(simTime()))
  }
}