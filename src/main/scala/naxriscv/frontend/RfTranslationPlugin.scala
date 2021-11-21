package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces.{CommitService, DecoderService, InitCycles, RegfileService, RegfileSpec, RobService}
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
    for((p, portId) <- ports.zipWithIndex){
      // Bypass
      val bypass = new Area{
        val hits = for(otherId <- portId+1 until ports.size; po = ports(otherId)) yield po.valid && po.address === p.address
        val hit = hits.orR
      }
      ram.write(
        address = p.address,
        data = p.data,
        enable = p.valid && !bypass.hit
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

    when(io.rollback){
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


class RfTranslationPlugin() extends Plugin with InitCycles {
  override def initCycles = 32

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
    getService[RobService].retain()
  }

  val logic = create late new Area{
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val commit = getService[CommitService]
    val rob = getService[RobService]

    val stage = frontend.pipeline.allocated
    import stage._

    val entryCount = decoder.rsPhysicalDepthMax
    val impl = new TranslatorWithRollback(
      payloadType = UInt(log2Up(entryCount) bits),
      depth       = 32,
      commitPorts = COMMIT_COUNT,
      writePorts  = DISPATCH_COUNT,
      readPorts   = DISPATCH_COUNT*(decoder.rsCount+1)
    )

    impl.io.rollback := getService[CommitService].reschedulingPort().valid

    val writes = for(slotId <- 0 until DISPATCH_COUNT) {
      impl.io.writes(slotId).valid := isFireing && (DISPATCH_MASK, slotId) && (decoder.WRITE_RD, slotId)
      impl.io.writes(slotId).address := stage(decoder.ARCH_RD, slotId)
      impl.io.writes(slotId).data := stage(decoder.PHYS_RD, slotId)
      // Bypass, but now it is done in the sub comonent
      //      for(otherId <- slotId+1 until DISPATCH_COUNT) {
      //        when(impl.io.writes(otherId).valid && stage(decoder.ARCH_RD, otherId) === stage(decoder.ARCH_RD, slotId)){
      //          impl.io.writes(slotId).valid := False
      //        }
      //      }
    }

    val onCommit = new Area{
      val event = commit.onCommit()
      val writeRd = rob.readAsync(decoder.WRITE_RD, COMMIT_COUNT, event.robId)
      val physRd = rob.readAsync(decoder.PHYS_RD, COMMIT_COUNT, event.robId)
      val archRd = rob.readAsync(decoder.ARCH_RD, COMMIT_COUNT, event.robId)
      for(slotId <- 0 until COMMIT_COUNT){
        val port = impl.io.commits(slotId)
        port.valid := event.mask(slotId) && writeRd(slotId)
        port.address := archRd(slotId)
        port.data := physRd(slotId)
      }
    }

    val init = new Area {
      assert(isPow2(entryCount))
      val counter = Reg(UInt(log2Up(32*2) bits)) init (0)
      val busy = !counter.msb

      when(busy) {
        impl.io.rollback := True

        val port = impl.io.commits(0)
        port.valid := True
        port.address := counter.resized
        port.data    := 0 //NOTE is map all the registers to physical 0

        counter := counter + 1
      }
    }


    val translation = new Area{
      for(slotId <- 0 until DISPATCH_COUNT) {
        val portRd = impl.io.reads(slotId*(1+decoder.rsCount))
        val archRd = stage(decoder.ARCH_RD, slotId)
        portRd.cmd.valid := (decoder.WRITE_RD, slotId)
        portRd.cmd.payload := archRd
        (decoder.PHYS_RD_FREE, slotId) := portRd.rsp.payload

        val rs = for(rsId <- 0 until decoder.rsCount) yield new Area{
          val id = rsId
          val port = impl.io.reads(slotId*(1+decoder.rsCount)+rsId+1)
          val archRs = stage(decoder.ARCH_RS(rsId), slotId)
          port.cmd.valid := (decoder.READ_RS(rsId), slotId)
          port.cmd.payload := archRs
          (decoder.PHYS_RS(rsId), slotId) := port.rsp.payload
        }

        //Slot bypass
        for(priorId <- 0 until slotId){
          val useRd = (decoder.WRITE_RD, priorId) && (DISPATCH_MASK, priorId)
          val writeRd = (decoder.ARCH_RD, priorId)
          when(useRd && writeRd === archRd){
            (decoder.PHYS_RD_FREE, slotId) := stage(decoder.PHYS_RD, priorId)
          }
          for(e <- rs){
            when(useRd && writeRd === e.archRs){
              (decoder.PHYS_RS(e.id), slotId) := stage(decoder.PHYS_RD, priorId)
            }
          }
        }
      }
    }
    frontend.release()
    rob.release()
  }
}
