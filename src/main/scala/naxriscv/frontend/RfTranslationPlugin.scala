package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces.{DecoderService, RegfileService, RegfileSpec, Riscv}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class TranslatorUpdate[T <: Data](payloadType : HardType[T], depth : Int) extends Bundle{
  val address = UInt(log2Up(depth) bits)
  val data    = payloadType()
}


case class TranslatorRead[T <: Data](payloadType : HardType[T], depth : Int) extends Bundle with IMasterSlave {
  val cmd = Flow(UInt(log2Up(depth) bits))
  val rsp = Flow(payloadType())

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

class TranslatorWithRollback[T <: Data](payloadType : HardType[T],
                                             depth : Int,
                                             commitPorts : Int,
                                             writePorts : Int,
                                             readPorts : Int) extends Component {
  val io = new Bundle {
    val rollback = in Bool()
    val writes = Vec.fill(writePorts)(slave(Flow(TranslatorUpdate(payloadType,depth))))
    val commits = Vec.fill(commitPorts)(slave(Flow(TranslatorUpdate(payloadType,depth))))
    val reads = Vec.fill(readPorts)(slave(TranslatorRead(payloadType,depth)))
  }

  class UpdatedState(ports : Vec[Flow[TranslatorUpdate[T]]]) extends Area{
    val ram = Mem.fill(depth)(payloadType)
    for(p <- ports){
      ram.write(
        address = p.address,
        data = p.data,
        enable = p.valid
      )
    }
  }

  val writes = new UpdatedState(io.writes)
  val commits = new UpdatedState(io.commits)

  val location = new Area{
    val updated = Reg(Bits(depth bits))
    val set = for(i <- 0 until depth) yield new Area{
      val hits =  io.writes.map(p => p.valid && p.address === i)
      when(hits.orR){
        updated(i) := True
      }
    }

    val booted = RegNext(True) init(False)
    when(io.rollback || !booted){
      updated := 0
    }
  }

  val read = for(p <- io.reads) yield new Area{
    val written = writes.ram.readAsync(p.cmd.payload)
    val commited = commits.ram.readAsync(p.cmd.payload)
    val sel = location.updated(p.cmd.payload)
    val muxed = sel ? written | commited
    p.rsp.valid := p.cmd.valid
    p.rsp.payload := muxed
  }
}


object TranslatorWithRollback extends App{
  LutInputs.set(6)

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new TranslatorWithRollback(
    payloadType = UInt(6 bits),
    depth       = 32,
    commitPorts =  2,
    writePorts  = 2,
    readPorts   = 4
  ))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
//  {
    //    val x2 = ClockDomain.external("x2", withReset = false).setSynchronousWith(ClockDomain.current)
    //    ClockDomain.current.addTag(ClockDomainFasterTag(2, x2))
//  }
}


class RfTranslationPlugin() extends Plugin {
  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
  }

  val logic = create late new Area{
    val decoder = getService[DecoderService]

    val frontend = getService[FrontendPlugin]
    val impl = new TranslatorWithRollback(
      payloadType = UInt(log2Up(decoder.rsPhysicalDepthMax) bits),
      depth       = 32,
      commitPorts = Global.COMMIT_COUNT,
      writePorts  = DISPATCH_COUNT,
      readPorts   = DISPATCH_COUNT*decoder.rsCount
    )

    val translation = new Area{
      val stage = frontend.pipeline.renamed
      import stage._
      for(slotId <- 0 until DISPATCH_COUNT) {
        for(rsId <- 0 until decoder.rsCount) {
          val port = impl.io.reads(slotId*decoder.rsCount+rsId)
          port.cmd.payload := U((INSTRUCTION_DECOMPRESSED, slotId) (Riscv.rsRange(rsId)))
          (decoder.PHYSICAL_RS(rsId), slotId) := port.rsp.payload
        }
      }
    }
    frontend.release()
  }
}
