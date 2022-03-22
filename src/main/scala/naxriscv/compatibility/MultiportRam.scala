package naxriscv.compatibility

import spinal.core._
import spinal.core.internals._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class MemWriteCmd[T <: Data](payloadType : HardType[T], depth : Int) extends Bundle{
  val address = UInt(log2Up(depth) bits)
  val data    = payloadType()
}


case class MemRead[T <: Data](payloadType : HardType[T], depth : Int) extends Bundle with IMasterSlave {
  val cmd = Flow(UInt(log2Up(depth) bits))
  val rsp = payloadType()

  override def asMaster() = {
    master(cmd)
    in(rsp)
  }
}


case class RamAxyncMwIo[T <: Data](payloadType : HardType[T], depth : Int, writePorts : Int, readPorts : Int) extends Bundle {
  val writes = Vec.fill(writePorts)(slave(Flow(MemWriteCmd(payloadType, depth))))
  val read = Vec.fill(readPorts)(slave(MemRead(payloadType, depth)))
}

case class RamAsyncMwXor[T <: Data](payloadType : HardType[T], depth : Int, writePorts : Int, readPorts : Int) extends Component {
  val io = RamAxyncMwIo(payloadType, depth, writePorts, readPorts)
  val rawBits = payloadType.getBitsWidth
  val rawType = HardType(Bits(rawBits bits))
  val ram = List.fill(writePorts)(Mem.fill(depth)(rawType))

  val writes = for((port, storage) <- (io.writes, ram).zipped) yield new Area{
    val values = ram.filter(_ != storage).map(_.readAsync(port.address))
    val xored = (port.data.asBits :: values).reduceBalancedTree(_ ^ _)
    storage.write(
      enable = port.valid,
      address = port.address,
      data = xored
    )
  }

  val reads = for(port <- io.read) yield new Area{
    val values = ram.map(_.readAsync(port.cmd.payload))
    val xored = values.reduceBalancedTree(_ ^ _)
    port.rsp := xored.as(payloadType)
  }

  def addMemTags(spinalTags: Seq[SpinalTag]) : this.type = {
    ram.foreach(_.addTags(spinalTags))
    this
  }
}

case class RamAsyncMwMux[T <: Data](payloadType : HardType[T],
                                    depth : Int,
                                    writePorts : Int,
                                    readPorts : Int) extends Component {
  val io = RamAxyncMwIo(payloadType, depth, writePorts, readPorts)
  val rawBits = payloadType.getBitsWidth
  val rawType = HardType(Bits(rawBits bits))
  val ram = List.fill(writePorts)(Mem.fill(depth)(rawType))

  val location = RamAsyncMwXor(
    payloadType = UInt(log2Up(writePorts) bits),
    depth       = depth,
    writePorts  = writePorts,
    readPorts   = readPorts
  )

  val writes = for((port, storage, loc) <- (io.writes, ram, location.io.writes).zipped) yield new Area{
    storage.write(
      enable = port.valid,
      address = port.address,
      data = port.data.asBits
    )
    loc.valid := port.valid
    loc.address := port.address
    loc.data := U(ram.indexOf(storage))
  }

  val reads = for((port, loc) <- (io.read, location.io.read).zipped) yield new Area{
    loc.cmd.valid := port.cmd.valid
    loc.cmd.payload := port.cmd.payload

    val reads  = ram.map(_.readAsync(port.cmd.payload))
    port.rsp := reads.read(loc.rsp).as(payloadType)
  }

  def addMemTags(spinalTags: Seq[SpinalTag]) : this.type = {
    ram.foreach(_.addTags(spinalTags))
    this
  }
}

case class RamSyncMwXor[T <: Data](payloadType : HardType[T], depth : Int, writePorts : Int, readPorts : Int) extends Component {
  val io = new Bundle {
    val writes = Vec.fill(writePorts)(slave(Flow(MemWriteCmd(payloadType, depth))))
    val read = Vec.fill(readPorts)(slave(MemRead(payloadType, depth)))
  }
  val rawBits = payloadType.getBitsWidth
  val rawType = HardType(Bits(rawBits bits))
  val ram = List.fill(writePorts)(Mem.fill(depth)(rawType))

  val writes = for((port, storage) <- (io.writes, ram).zipped) yield new Area{
    val values = ram.filter(_ != storage).map(_.readAsync(port.address))
    val xored = (port.data.asBits :: values).reduceBalancedTree(_ ^ _)
    storage.write(
      enable = port.valid,
      address = port.address,
      data = xored
    )
  }

  val reads = for(port <- io.read) yield new Area{
    val values = ram.map(_.readSync(port.cmd.payload, port.cmd.valid))
    val xored = values.reduceBalancedTree(_ ^ _)
    port.rsp := xored.as(payloadType)
  }

  def addMemTags(spinalTags: Seq[SpinalTag]) : this.type = {
    ram.foreach(_.addTags(spinalTags))
    this
  }
}

case class RamMr[T <: Data](payloadType : HardType[T], depth : Int, readPorts : Int) extends Component {
  val io = new Bundle {
    val write = slave(Flow(MemWriteCmd(payloadType, depth)))
    val read = Vec.fill(readPorts)(slave(MemRead(payloadType, depth)))
  }

  val banks = for(read <- io.read) yield new Area{
    val ram = Mem.fill(depth)(payloadType)
    ram.initBigInt((0 until depth).map(_ => BigInt(0)))
    ram.write(
      enable = io.write.valid,
      address = io.write.address,
      data = io.write.data
    )
    read.rsp := ram.readAsync(read.cmd.payload)
  }
}

object RamMwXorSynth extends App{
  LutInputs.set(6)

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new RamAsyncMwXor(
    payloadType = UInt(1 bits),
    depth       = 32,
    writePorts  = 2,
    readPorts   = 2
  ).setDefinitionName("P1"))))

  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new RamAsyncMwXor(
    payloadType = UInt(6 bits),
    depth       = 32,
    writePorts  = 2,
    readPorts   = 2
  ).setDefinitionName("P6"))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}


case class RamMwStorage[T <: Data](payloadType : HardType[T], depth : Int, ways : Int, readPorts : Int) extends Component{
  val io = new Bundle {

  }
}


case class ClockDomainFasterTag(times : Int, cd : ClockDomain) extends SpinalTag
case class MultiPortWritesSymplifierTag() extends SpinalTag

class MultiPortWritesSymplifier(onlyTagged : Boolean = false) extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo: MemTopology) : Unit = {
    if(onlyTagged && typo.mem.getTag(classOf[MultiPortWritesSymplifierTag]).isEmpty) return

    if(typo.writes.size > 1 && typo.readsSync.size == 0){
      typo.writes.foreach(w => assert(w.mask == null))
      typo.writes.foreach(w => assert(w.clockDomain == typo.writes.head.clockDomain))
      val cd = typo.writes.head.clockDomain

      import typo._

      val ctx = List(mem.parentScope.push(), cd.push())

      val io = if(typo.mem.width >= 10){
        RamAsyncMwMux(
          payloadType = Bits(mem.width bits),
          depth       = mem.wordCount,
          writePorts  = writes.size,
          readPorts   = readsAsync.size
        ).setCompositeName(mem).addMemTags(mem.getTags().toSeq).io
      } else {
        RamAsyncMwXor(
          payloadType = Bits(mem.width bits),
          depth       = mem.wordCount,
          writePorts  = writes.size,
          readPorts   = readsAsync.size
        ).setCompositeName(mem).addMemTags(mem.getTags().toSeq).io
      }

      for((dst, src) <- (io.writes, writes).zipped){
        dst.valid.assignFrom(src.writeEnable)
        dst.address.assignFrom(src.address)
        dst.data.assignFrom(src.data)
      }


      for((reworked, old) <- (io.read, readsAsync).zipped){
        reworked.cmd.payload.assignFrom(old.address)
        wrapConsumers(typo, old, reworked.rsp)
      }

      mem.removeStatement()
      mem.foreachStatements(s => s.removeStatement())

      ctx.foreach(_.restore())
    }

    if(typo.writes.size > 1 && typo.readsAsync.size == 0){
      typo.writes.foreach(w => assert(w.mask == null))
      typo.writes.foreach(w => assert(w.clockDomain == typo.writes.head.clockDomain))
      val cd = typo.writes.head.clockDomain

      import typo._

      val ctx = List(mem.parentScope.push(), cd.push())

      val c = RamSyncMwXor(
        payloadType = Bits(mem.width bits),
        depth       = mem.wordCount,
        writePorts  = writes.size,
        readPorts   = readsSync.size
      ).setCompositeName(mem)

      for((dst, src) <- (c.io.writes, writes).zipped){
        dst.valid.assignFrom(src.writeEnable)
        dst.address.assignFrom(src.address)
        dst.data.assignFrom(src.data)
      }


      for((reworked, old) <- (c.io.read, readsSync).zipped){
        reworked.cmd.valid.assignFrom(old.readEnable)
        reworked.cmd.payload.assignFrom(old.address)
        wrapConsumers(typo, old, reworked.rsp)
      }

      mem.removeStatement()
      mem.foreachStatements(s => s.removeStatement())

      ctx.foreach(_.restore())
    }
  }
}


class MultiPortReadSymplifier extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo: MemTopology) = {
    if(typo.writes.size == 1 && typo.readsSync.size == 0 && typo.readsAsync.size > 1){
      typo.writes.foreach(w => assert(w.mask == null))
      typo.writes.foreach(w => assert(w.clockDomain == typo.writes.head.clockDomain))
      val cd = typo.writes.head.clockDomain

      import typo._

      val ctx = List(mem.parentScope.push(), cd.push())

      val c = RamMr(
        payloadType = Bits(mem.width bits),
        depth       = mem.wordCount,
        readPorts   = readsAsync.size
      ).setCompositeName(mem)

      c.io.write.valid.assignFrom(typo.writes.head.writeEnable)
      c.io.write.address.assignFrom(typo.writes.head.address)
      c.io.write.data.assignFrom(typo.writes.head.data)


      for((reworked, old) <- (c.io.read, readsAsync).zipped){
        reworked.cmd.payload.assignFrom(old.address)
        wrapConsumers(typo, old, reworked.rsp)
      }

      mem.removeStatement()
      mem.foreachStatements(s => s.removeStatement())

      ctx.foreach(_.restore())
    }
  }
}

class MemReadDuringWriteHazardPhase extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo: MemTopology) = {
    import typo._
    for(read <- typo.readsSync if read.readUnderWrite == dontCare){
      val ctx = List(mem.parentScope.push(), read.clockDomain.push())

      val readed = Bits(read.getWidth bits)
      readed.assignFrom(read)
      wrapConsumers(typo, read, readed)

      for(write <- typo.writes){
        assert(read.width == write.width && read.clockDomain == write.clockDomain)
        val hazard = RegInit(False)
        val mask = (write.mask != null) generate Reg(Bits(write.mask.getWidth bits))
        when(read.readEnable.asInstanceOf[Bool]){
          hazard := write.writeEnable.asInstanceOf[Bool] && write.address.asInstanceOf[UInt] === read.address.asInstanceOf[UInt]
          if(mask != null) mask := write.mask.asInstanceOf[Bits]
        }
        when(hazard){
          mask match {
            case null => readed.assignDontCare()
            case mask => for((data, sel) <- (readed.subdivideIn(widthOf(mask) slices), mask.asBools).zipped){
              when(sel){ data.assignDontCare() }
            }
          }
        }
      }

      ctx.foreach(_.restore())
    }
  }
}

class MemReadAsyncToPhasedReadSyncPhaseTag(val cd : ClockDomain) extends SpinalTag
class MemReadAsyncToPhasedReadSyncPhase extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo: MemTopology) : Unit = {
    import typo._

    val tag = mem.getTag(classOf[MemReadAsyncToPhasedReadSyncPhaseTag])
    if(tag.isEmpty) return
    val cd = tag.get.cd
    for(read <- typo.readsAsync){

      val ctx = List(mem.parentScope.push(), cd.push())

      val readed = mem.readSync(read.address.asInstanceOf[UInt]).asInstanceOf[Data].asBits
      wrapConsumers(typo, read, readed)
      read.removeStatement()

      ctx.foreach(_.restore())
    }
  }
}




class EnforceSyncRamPhase extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo: MemTopology) = {
    if(typo.readsAsync.size == 0){
      typo.mem.addAttribute("ram_style", "block")
    }
  }
}