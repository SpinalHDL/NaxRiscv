package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

class MasterAgent (bus : Bus, cd : ClockDomain) {
  val driver = new Area{
    val a = StreamDriver.queue(bus.a, cd)._2
    val b = StreamReadyRandomizer(bus.b, cd)
    val c = StreamDriver.queue(bus.c, cd)._2
    val d = StreamReadyRandomizer(bus.d, cd)
    val e = StreamDriver.queue(bus.e, cd)._2
  }

  val monitor = new Area{
    val d = Array.fill[ChannelD => Unit](1 << bus.p.sourceWidth)(null)
    val bm = StreamMonitor(bus.b, cd){ b =>
      val opcode  = b.opcode.toEnum
      val param   = b.param.toInt
      val source  = b.source.toInt
      val address = b.address.toLong
      val size    = b.size.toInt

      opcode match{
        case Opcode.B.PROBE_BLOCK => probeBlock(source, param, address, 1 << size)
      }
    }
    val dm = StreamMonitor(bus.d, cd){ p =>
      d(p.source.toInt)(p)
    }
  }

  def probeBlock(source : Int,
                 param : Int,
                 address : Long,
                 bytes : Int): Unit ={
    ???
  }

  def probeAck(param : Int,
               source : Int,
               address : Long,
               bytes : Int): Unit ={
    driver.c.enqueue{p =>
      p.opcode  #= Opcode.C.PROBE_ACK
      p.param   #= param
      p.size    #= log2Up(bytes)
      p.source  #= source
      p.address #= address
      if(bus.p.withBCE) {
        p.data.randomize()
        p.corrupt.randomize()
      }
    }
  }


  def get(source : Int, address : BigInt, bytes : Int) : Seq[Byte] = {
    driver.a.enqueue{p =>
      p.opcode  #= Opcode.A.GET
      p.param   #= 0
      p.size    #= log2Up(bytes)
      p.source  #= source
      p.address #= address
      p.mask.randomize()
      p.data.randomize()
      p.corrupt.randomize()
    }

    val mutex = SimMutex().lock()
    val data = new Array[Byte](bytes)
    var offset = 0
    monitor.d(source) = {d =>
      val raw = d.data.toBytes
      for(i <- 0 until bus.p.dataBytes){
        data(offset + i) = raw(i)
      }
      assert(!d.denied.toBoolean)
      assert(!d.corrupt.toBoolean)

      offset += bus.p.dataBytes
      if(offset == bytes){
        monitor.d(source) = null
        mutex.unlock()
      }
    }
    mutex.await()
    data
  }

  def putFullData(source : Int, address : BigInt, data : Seq[Byte]) : Boolean = {
    val size = log2Up(data.length)
    for(offset <- 0 until data.length by bus.p.dataBytes) {
      driver.a.enqueue { p =>
        val buf = new Array[Byte](bus.p.dataBytes)
        (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
        p.opcode #= Opcode.A.PUT_FULL_DATA
        p.param #= 0
        p.size #= size
        p.source #= source
        p.address #= address + offset
        p.mask #= (BigInt(1) << bus.p.dataBytes)-1
        p.data #= buf
        p.corrupt #= false
      }
    }
    val mutex = SimMutex().lock()
    var denied = false
    monitor.d(source) = {d =>
      monitor.d(source) = null
      denied = d.denied.toBoolean
      mutex.unlock()
    }
    mutex.await()
    denied
  }

  def putPartialData(source : Int, address : BigInt, data : Seq[Byte], mask : Seq[Boolean]) : Boolean = {
    val size = log2Up(data.length)
    for(offset <- 0 until data.length by bus.p.dataBytes) {
      driver.a.enqueue { p =>
        val buf = new Array[Byte](bus.p.dataBytes)
        (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
        val buf2 = Array.fill[Byte]((bus.p.dataBytes+7)/8)(0)
        (0 until bus.p.dataBytes).foreach(i => buf2(i >> 3) = (buf2(i >> 3) | (mask(offset + i).toInt << (i & 7))).toByte)
        p.opcode #= Opcode.A.PUT_PARTIAL_DATA
        p.param #= 0
        p.size #= size
        p.source #= source
        p.address #= address + offset
        p.mask #= buf2
        p.data #= buf
        p.corrupt #= false
      }
    }
    val mutex = SimMutex().lock()
    var denied = false
    monitor.d(source) = {d =>
      monitor.d(source) = null
      denied = d.denied.toBoolean
      mutex.unlock()
    }
    mutex.await()
    denied
  }
}
