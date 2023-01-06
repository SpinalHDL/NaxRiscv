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
        case Opcode.B.PROBE_BLOCK => probeBlock(param, source, address, 1 << size)
      }
    }
    val dm = StreamMonitor(bus.d, cd){ p =>
      d(p.source.toInt)(p)
    }
  }

  def probeBlock(param : Int,
                source : Int,
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


  def get(address : BigInt, bytes : Int, source : Int) : Seq[Byte] = {
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
}
