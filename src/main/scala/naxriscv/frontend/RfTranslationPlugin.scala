package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces.{CommitService, DecoderService, RegfileService, RegfileSpec, Riscv, RobService}
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

    val impl = new TranslatorWithRollback(
      payloadType = UInt(log2Up(decoder.rsPhysicalDepthMax) bits),
      depth       = 32,
      commitPorts = COMMIT_COUNT,
      writePorts  = DISPATCH_COUNT,
      readPorts   = DISPATCH_COUNT*decoder.rsCount
    )

    impl.io.rollback := getService[CommitService].reschedulingPort().valid

    val writes = for(slotId <- 0 until DISPATCH_COUNT) {
      impl.io.writes(slotId).valid := isFireing && (DISPATCH_MASK, slotId)
      impl.io.writes(slotId).address := U((INSTRUCTION_DECOMPRESSED, slotId) (Riscv.rdRange))
      impl.io.writes(slotId).data := stage(decoder.PHYS_RD, slotId)
    }

    val onCommit = new Area{
      val event = commit.onCommit()
      val writeRd = rob.readAsyncLine(decoder.WRITE_RD, COMMIT_COUNT, event.robId)
      val physRd = rob.readAsyncLine(decoder.PHYS_RD, COMMIT_COUNT, event.robId)
      val archRd = rob.readAsyncLine(decoder.ARCH_RD, COMMIT_COUNT, event.robId)
      for(slotId <- 0 until COMMIT_COUNT){
        val port = impl.io.commits(slotId)
        port.valid := event.mask(slotId) && writeRd(slotId)
        port.address := archRd(slotId)
        port.data := physRd(slotId)
      }
    }

//    val commit = new Area{
//      case class Ctx() extends Bundle{
//        val enable = Bool()
//        val rd = UInt(log2Up(getService[RegfileService](rf).getPhysicalDepth) bits) //TODO
//      }
//      val contextRam = Seq.fill(Frontend.DISPATCH_COUNT)(Mem.fill(ROB.SIZE/Frontend.DISPATCH_COUNT)(Ctx()))
//
//      assert(Global.COMMIT_COUNT.get == Frontend.DISPATCH_COUNT)
//      val store = for(slotId <- 0 until Frontend.DISPATCH_COUNT) yield new Area{
//        assert(isPow2(Frontend.DISPATCH_COUNT))
//
//        val ctx = Ctx()
//        ctx.enable := stage(decoder.WRITE_RD, slotId)
//        ctx.rd     := stage(decoder.PHYSICAL_RD, slotId)
//
//        contextRam(slotId).write(
//          enable = isFireing,
//          address = Frontend.ROB_ID >> log2Up(Frontend.DISPATCH_COUNT),
//          data = ctx
//        )
//      }
//
//      val port = commit.onCommit()
//      for((sel, port) <- (port.mask.asBools, impl.io.commits).zipped){
//        port.valid := sel
//        port.address := event.payload
//      }
//    }


    val translation = new Area{
      for(slotId <- 0 until DISPATCH_COUNT) {
        for(rsId <- 0 until decoder.rsCount) {
          val port = impl.io.reads(slotId*decoder.rsCount+rsId)
          val archRs = (Frontend.INSTRUCTION_DECOMPRESSED, slotId) (Riscv.rsRange(rsId))
          port.cmd.payload := U(archRs)
          (decoder.PHYS_RS(rsId), slotId) := port.rsp.payload

          //Slot bypass
          for(priorId <- 0 until slotId){
            val useRd = (decoder.WRITE_RD, priorId) && (DISPATCH_MASK, priorId)
            val writeRd = (Frontend.INSTRUCTION_DECOMPRESSED, priorId)(Riscv.rdRange)
            when(useRd && writeRd === archRs){
              (decoder.PHYS_RS(rsId), slotId) := stage(decoder.PHYS_RS(rsId), priorId)
            }
          }
        }
      }
    }
    frontend.release()
    rob.release()
  }
}
