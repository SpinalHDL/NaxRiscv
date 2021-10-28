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

case class RamMwXor[T <: Data](payloadType : HardType[T], depth : Int, writePorts : Int, readPorts : Int) extends Component {
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
    val values = ram.map(_.readAsync(port.cmd.payload))
    val xored = values.reduceBalancedTree(_ ^ _)
    port.rsp := xored.as(payloadType)
  }
}

object RamMwXorSynth extends App{
  LutInputs.set(6)

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new RamMwXor(
    payloadType = UInt(1 bits),
    depth       = 32,
    writePorts  = 2,
    readPorts   = 2
  ).setDefinitionName("P1"))))

  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new RamMwXor(
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

class MultiPortWritesSymplifier extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo: MemTopology) = {
    if(typo.writes.size > 1 && typo.readsSync.size == 0){
      typo.writes.foreach(w => assert(w.mask == null))
      typo.writes.foreach(w => assert(w.clockDomain == typo.writes.head.clockDomain))
      val cd = typo.writes.head.clockDomain

      import typo._

      val ctx = List(mem.parentScope.push(), cd.push())

      val c = RamMwXor(
        payloadType = Bits(mem.width bits),
        depth       = mem.wordCount,
        writePorts  = writes.size,
        readPorts   = readsAsync.size
      ).setCompositeName(mem)

      for((dst, src) <- (c.io.writes, writes).zipped){
        dst.valid.assignFrom(src.writeEnable)
        dst.address.assignFrom(src.address)
        dst.data.assignFrom(src.data)
      }


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