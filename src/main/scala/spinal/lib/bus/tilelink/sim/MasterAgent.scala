package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.collection.{breakOut, mutable}
import scala.util.Random

class Block(val address : Long,
            var cap : Int,
            var dirty : Boolean = false,
            var data : Array[Byte] = null,
            var orderingBody : () => Unit = () => Unit,
            var retains : Int = 0){
  def retain() = retains += 1
  def release() = {
    assert(retains > 0)
    retains -= 1
  }
  var probe = Option.empty[Probe]
  def ordering(body : => Unit) = orderingBody = () => body
}
case class Probe(source : Int, param : Int, address : Long, size : Int, perm : Boolean){

}

class MasterAgent (val bus : Bus, cd : ClockDomain, blockSize : Int = 64) {
  val debug = true
  val driver = new Area{
    val a = StreamDriverOoo(bus.a, cd)
    val b = StreamReadyRandomizer(bus.b, cd)
    val c = StreamDriverOoo(bus.c, cd)
    val d = StreamReadyRandomizer(bus.d, cd)
    val e = StreamDriverOoo(bus.e, cd)
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
        case Opcode.B.PROBE_BLOCK => {
          def ok(param : Int) = probeAck(
            param   = param,
            source  = source,
            address = address,
            bytes   = 1 << size
          )
          block.blocks.get(block.sourceToMaster(source) -> address) match {
            case Some(b) => {
              b.cap < param match {
                case false => ok(Param.Report.fromCap(b.cap))
                case true  => {
                  b.probe match {
                    case Some(x) => ???
                    case None => b.probe = Some(Probe(source, param, address, size, false))
                  }
                  b.retains match {
                    case 0 => block.changeBlockCap(source, address, param)
                    case _ =>  println(f"Retained $address%x ${simTime()}")
                  }
                }
              }
            }
            case None => ok(Param.Report.NtoN)
          }
          probeBlock(source, param, address, 1 << size)
        }
      }
    }
    val dm = StreamMonitor(bus.d, cd){ p =>
      d(p.source.toInt)(p)
    }
  }

  val ordering = new Area{
    val map = Array.fill[() => Unit](1 << bus.p.sourceWidth)(null)
    def apply(source : Int)(body : => Unit) = map(source) = () => body
    def checkDone(source : Int) = assert(!map.contains(source))
  }

  //  case class Block(var cap : Int,
  //                   var dirty : Boolean = false,
  //                   var data : Array[Byte] = null,
  //                   var orderingBody : Block => Unit = _ => Unit,
  //                   var retains : Int = 0){
  //    def retain() = retains += 1
  //    def release() = {
  //      assert(retains > 0)
  //      retains -= 1
  //    }
  //    var probe = Option.empty[Probe]
  //    def ordering(body : Block => Unit) = orderingBody = body
  //  }


  val block = new Area{
    val sourceToMaster = (0 until  1 << bus.p.sourceWidth).map(source => bus.p.node.m.getMasterFromSource(source))
    val blocks = mutable.LinkedHashMap[(MasterParameters, Long), Block]()
    def apply(source : Int, address : Long) = blocks(sourceToMaster(source) -> address)
    def get(source : Int, address : Long) = blocks.get(sourceToMaster(source) -> address)
    def contains(source : Int, address : Long) = blocks.contains(sourceToMaster(source) -> address)
    def update(key : (Int, Long), block : Block) = {
      val key2 = (sourceToMaster(key._1) -> key._2)
      assert(!blocks.contains(key2))
      blocks(key2) = block
    }
    def removeBlock(source : Int, address : Long) = {
      blocks.remove(sourceToMaster(source) -> address)
    }
    def changeBlockCap(source : Int, address : Long, cap : Int) = {
      val block = apply(source, address)
      if(debug) println(f"src=$source addr=$address%x ${block.cap} -> $cap time=${simTime()}")
      val oldCap = block.cap
      cap match {
        case Param.Cap.toN => block.cap = Param.Cap.toN; removeBlock(source, address)
        case _ => block.cap = Param.Cap.toB
      }
//      if(block.retains == 0) {
        block.probe match {
          case Some(probe) => {
            block.probe = None
            assert(probe.perm == false)
            block.dirty match {
              case false => probeAck(
                param = Param.reportPruneToCap(oldCap, cap),
                source = probe.source,
                address = probe.address,
                bytes = probe.size
              )
              case true => {
                probeAckData(
                  param = Param.reportPruneToCap(oldCap, cap),
                  source = probe.source,
                  address = probe.address,
                  data = block.data
                )(block.orderingBody())
                block.dirty = false
              }
            }
          }
          case None =>
        }
//      }
    }

    def retain(source : Int, address : Long) = blocks(sourceToMaster(source) -> address).retain()
    def release(source : Int, address : Long) = blocks(sourceToMaster(source) -> address).release()
  }

  def probeBlock(source : Int,
                 param : Int,
                 address : Long,
                 bytes : Int): Unit ={
//    ???
  }

  def probeAck(source : Int,
               param : Int,
               address : Long,
               bytes : Int): Unit ={
    driver.c.single{p =>
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

  def probeAckData(source : Int,
                   param : Int,
                   address : Long,
                   data : Seq[Byte])
                 (orderingBody : => Unit) : Unit = {
    //    ordering(source)(orderingBody)
    orderingBody
    val size = log2Up(data.length)
    driver.c.burst { push =>
      for (offset <- 0 until data.length by bus.p.dataBytes) {
        push { p =>
          val buf = new Array[Byte](bus.p.dataBytes)
          (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
          p.opcode #= Opcode.C.PROBE_ACK_DATA
          p.param #= param
          p.size #= size
          p.source #= source
          p.address #= address + offset
          p.data #= buf
          p.corrupt #= false
        }
      }
    }
  }

  def onGrant(source : Int, address : Long, param : Int) : Unit = {}
  def acquireBlock(source : Int,
                   param : Int,
                   address : Long,
                   bytes : Int)
                  (orderingBody : => Unit): Block ={
    ordering(source)(orderingBody)
    driver.a.single{p =>
      p.opcode  #= Opcode.A.ACQUIRE_BLOCK
      p.param   #= param
      p.size    #= log2Up(bytes)
      p.source  #= source
      p.address #= address
      if(p.withData) {
        p.mask.randomize()
        p.data.randomize()
        p.corrupt.randomize()
      }
    }

    val mutex = SimMutex().lock()
    val data = new Array[Byte](bytes)
    var offset = 0
    var b : Block = null
    var sink = -1
    monitor.d(source) = {d =>
      d.opcode.toEnum match {
        case Opcode.D.GRANT => {
          val param = d.param.toInt
          sink = d.sink.toInt
          b = block(source, address)
          assert(b.cap == Param.Cap.toB)
          b.cap = Param.Cap.toT
          monitor.d(source) = null
          mutex.unlock()
          if(debug) println(f"src=$source addr=$address%x 1 -> 0 time=${simTime()}")
          onGrant(source, address, param)
        }
        case Opcode.D.GRANT_DATA => { //TODO on naxriscv, may sent a aquire BtoT but may have been probed out meanwhile => test
          assert(!block.contains(source, address))
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
            val param = d.param.toInt
            onGrant(source, address, param)
            b = new Block(address, param, false, data){
              override def release() = {
                super.release()
                if(retains == 0) probe match {
                  case Some(probe) => {
                    println(f"Retained probe execute $address%x ${simTime()}")
                    block.changeBlockCap(probe.source, probe.address, probe.param)
                  }
                  case None =>
                }
              }
            }
            if(debug) println(f"src=$source addr=$address%x 2 -> $param time=${simTime()}")
            block(source -> address) = b
            sink = d.sink.toInt
          }
        }
      }
    }
    mutex.await()
    ordering.checkDone(source)
    driver.e.single{p =>
      p.sink  #= sink
    }

    b
  }



  def get(source : Int, address : Long, bytes : Int)
         (orderingBody : => Unit) : Seq[Byte] = {
    ordering(source)(orderingBody)
    driver.a.single{p =>
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
    val byteOffset = (address & (bus.p.dataBytes-1)).toInt
    monitor.d(source) = {d =>
      assert(d.opcode.toEnum == Opcode.D.ACCESS_ACK_DATA)
      val raw = d.data.toBytes
      for(i <- 0 until bus.p.dataBytes){
        val ptr = offset + i - byteOffset
        if(ptr >= 0 && ptr < bytes) data(ptr) = raw(i)
      }
      assert(!d.denied.toBoolean)
      assert(!d.corrupt.toBoolean)

      offset += bus.p.dataBytes
      if(offset >= bytes){
        monitor.d(source) = null
        mutex.unlock()
      }
    }
    mutex.await()
    ordering.checkDone(source)
    data
  }

  def releaseData(source : Int, param : Int, address : Long, data : Seq[Byte])
                 (orderingBody : => Unit) : Boolean = {
    ordering(source)(orderingBody)
    val size = log2Up(data.length)
    driver.c.burst { push =>
      for (offset <- 0 until data.length by bus.p.dataBytes) {
        push { p =>
          val buf = new Array[Byte](bus.p.dataBytes)
          (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
          p.opcode #= Opcode.C.RELEASE_DATA
          p.param #= param
          p.size #= size
          p.source #= source
          p.address #= address + offset
          p.data #= buf
          p.corrupt #= false
        }
      }
    }
    val mutex = SimMutex().lock()
    var denied = false
    monitor.d(source) = {d =>
      monitor.d(source) = null
      assert(d.opcode.toEnum == Opcode.D.RELEASE_ACK)
      mutex.unlock()
    }
    mutex.await()
    ordering.checkDone(source)

//    val block = this.block(source, address)
    val newCap = Param.reportPruneToCap(param)
    this.block.changeBlockCap(source, address, newCap)

    denied
  }


  def release(source : Int, param : Int, address : Long, bytes : Int) : Boolean = {
    val mutex = SimMutex().lock()
    driver.c.single { p =>
      p.opcode #= Opcode.C.RELEASE
      p.param #= param
      p.size #= log2Up(bytes)
      p.source #= source
      p.address #= address
      p.data.randomize()
      p.corrupt #= false
    }
    var denied = false
    monitor.d(source) = {d =>
      monitor.d(source) = null
      assert(d.opcode.toEnum == Opcode.D.RELEASE_ACK)
      mutex.unlock()
    }
    mutex.await()
    ordering.checkDone(source)

    val newCap = Param.reportPruneToCap(param)
    this.block.changeBlockCap(source, address, newCap)

    denied
  }


  def putFullData(source : Int, address : Long, data : Seq[Byte])
                 (orderingBody : => Unit) : Boolean = {
    ordering(source)(orderingBody)
    val size = log2Up(data.length)
    driver.a.burst { push =>
      for (offset <- 0 until data.length by bus.p.dataBytes) {
        push { p =>
          val buf = new Array[Byte](bus.p.dataBytes)
          val byteOffset = (address & (bus.p.dataBytes - 1)).toInt
          val bytes = data.size
          for (i <- 0 until bus.p.dataBytes) {
            val ptr = offset + i - byteOffset
            (ptr >= 0 && ptr < bytes) match {
              case false => buf(i) = Random.nextInt().toByte
              case true => buf(i) = data(ptr)
            }
          }
          p.opcode #= Opcode.A.PUT_FULL_DATA
          p.param #= 0
          p.size #= size
          p.source #= source
          p.address #= address + offset
          p.mask #= ((BigInt(1) << bus.p.dataBytes.min(data.size)) - 1) << (address.toInt & (bus.p.dataBytes - 1))
          p.data #= buf
          p.corrupt #= false
        }
      }
    }
    val mutex = SimMutex().lock()
    var denied = false
    monitor.d(source) = {d =>
      monitor.d(source) = null
      assert(d.opcode.toEnum == Opcode.D.ACCESS_ACK)
      denied = d.denied.toBoolean
      mutex.unlock()
    }
    mutex.await()
    ordering.checkDone(source)
    denied
  }

  def putPartialData(source : Int, address : Long, data : Seq[Byte], mask : Seq[Boolean])
                    (orderingBody : => Unit) : Boolean = {
    ordering(source)(orderingBody)
    val size = log2Up(data.length)
    driver.a.burst { push =>
      for (offset <- 0 until data.length by bus.p.dataBytes) {
        push { p =>
          val buf = new Array[Byte](bus.p.dataBytes)
          //        (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
          val buf2 = Array.fill[Byte]((bus.p.dataBytes + 7) / 8)(0)
          //        (0 until bus.p.dataBytes).foreach(i => buf2(i >> 3) = (buf2(i >> 3) | (mask(offset + i).toInt << (i & 7))).toByte)

          val byteOffset = (address & (bus.p.dataBytes - 1)).toInt
          val bytes = data.size
          for (i <- 0 until bus.p.dataBytes) {
            val ptr = offset + i - byteOffset
            (ptr >= 0 && ptr < bytes) match {
              case false => buf(i) = Random.nextInt().toByte
              case true => {
                buf(i) = data(ptr)
                buf2(i >> 3) = (buf2(i >> 3) | (mask(ptr).toInt << (i & 7))).toByte
              }
            }
          }

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
    }
    val mutex = SimMutex().lock()
    var denied = false
    monitor.d(source) = {d =>
      monitor.d(source) = null
      assert(d.opcode.toEnum == Opcode.D.ACCESS_ACK)
      denied = d.denied.toBoolean
      mutex.unlock()
    }
    mutex.await()
    ordering.checkDone(source)
    denied
  }
}
