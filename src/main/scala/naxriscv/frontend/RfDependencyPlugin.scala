package naxriscv.frontend

import naxriscv.Frontend.{DISPATCH_MASK, ROB_ID}
import naxriscv.{Frontend, Global, ROB}
import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces.{CommitService, DecoderService, WakeService, IssueService, RegfileService, RegfileSpec, Riscv}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class DependencyUpdate(archDepth : Int, physDepth : Int) extends Bundle{
  val address = UInt(log2Up(archDepth) bits)
  val data    = UInt(log2Up(physDepth) bits)
}

case class DependencyCommit(physDepth : Int) extends Bundle{
  val address = UInt(log2Up(physDepth) bits)
}

case class DependencyReadRsp(physDepth : Int) extends Bundle{
  val enable = Bool()
  val rob = UInt(log2Up(physDepth) bits)
}

case class DependencyRead(archDepth : Int, physDepth : Int) extends Bundle with IMasterSlave {
  val cmd = Flow(UInt(log2Up(archDepth) bits))
  val rsp = Flow(DependencyReadRsp(physDepth))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

class DependencyStorage(archDepth : Int,
                        physDepth : Int,
                        commitPorts : Int,
                        writePorts : Int,
                        readPorts : Int) extends Component {
  val io = new Bundle {
    val clear = in Bool()
    val writes = Vec.fill(writePorts)(slave(Flow(DependencyUpdate(archDepth, physDepth))))
    val commits = Vec.fill(commitPorts)(slave(Flow(DependencyCommit(physDepth))))
    val reads = Vec.fill(readPorts)(slave(DependencyRead(archDepth, physDepth)))
  }

  val dependencies = new Area{
    val enable = Reg(Bits(physDepth bits))
    val dependency = Mem.fill(archDepth)(UInt(log2Up(physDepth) bits))

    val booted = RegNext(True) init(False)
    when(io.clear || !booted){
      enable := 0
    }
  }

  val writes = new Area{
    for(p <- io.writes){
      dependencies.dependency.write(
        address = p.address,
        data = p.data,
        enable = p.valid
      )
    }
  }


  val commits = for(p <- io.commits) yield new Area{
    when(p.valid){
      dependencies.enable(p.address) := True
    }
  }

  val read = for(p <- io.reads) yield new Area{
    val translated = dependencies.dependency.readAsync(p.cmd.payload)
    val enabled = dependencies.enable(translated)
    p.rsp.enable := enabled
    p.rsp.rob := translated
  }
}


object DependencyStorageSynth extends App{
  LutInputs.set(6)

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new DependencyStorage(
    archDepth   = 32,
    physDepth   = 64,
    commitPorts =  2,
    writePorts  =  2,
    readPorts   =  4
  ))).printPruned())
  val targets = XilinxStdTargets().take(2)


  Bench(rtls, targets)
//  {
    //    val x2 = ClockDomain.external("x2", withReset = false).setSynchronousWith(ClockDomain.current)
    //    ClockDomain.current.addTag(ClockDomainFasterTag(2, x2))
//  }
}


class RfDependencyPlugin() extends Plugin {
  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val issue    = getService[IssueService]
    val decoder  = getService[DecoderService]
    frontend.retain()
    issue.retain()

    val waits = List.fill(decoder.rsCount)(issue.newRobWait())

    issue.release()
  }

  val logic = create late new Area{
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val wakers = getServicesOf[WakeService]
    val wakeIds = wakers.flatMap(_.wakeRobs)

    val stage = frontend.pipeline.renamed
    import stage._

    val impl = new DependencyStorage(
      archDepth   = 32,
      physDepth   = decoder.rsPhysicalDepthMax,
      commitPorts = wakeIds.size,
      writePorts  = Global.COMMIT_COUNT,
      readPorts   = Frontend.DISPATCH_COUNT*decoder.rsCount
    )

    //Write
    for(slotId <- 0 until Frontend.DISPATCH_COUNT) {
      val port = impl.io.writes(slotId)
      port.valid := stage.isFireing && (decoder.WRITE_RD, slotId) && (DISPATCH_MASK, slotId)
      port.address := U((Frontend.INSTRUCTION_DECOMPRESSED, slotId) (Riscv.rdRange))
      port.data := ROB_ID | slotId
    }

    //Commit
    for((event, port) <- (wakeIds, impl.io.commits).zipped){
      port.valid := event.valid
      port.address := event.payload
    }

    impl.io.clear := getService[CommitService].rollback()

    //Read
    val dependency = new Area{
      for(slotId <- 0 until Frontend.DISPATCH_COUNT) {
        for(rsId <- 0 until decoder.rsCount) {
          val archRs = (Frontend.INSTRUCTION_DECOMPRESSED, slotId) (Riscv.rsRange(rsId))
          val useRs = (decoder.READ_RS(rsId), slotId)
          val port = impl.io.reads(slotId*decoder.rsCount+rsId)
          port.cmd.payload := U(archRs)
          (setup.waits(rsId).ENABLE, slotId) := port.rsp.enable
          (setup.waits(rsId).ID    , slotId) := port.rsp.rob

          //Slot write bypass
          for(priorId <- 0 until slotId){
            val useRd = (decoder.WRITE_RD, priorId) && (DISPATCH_MASK, priorId)
            val writeRd = (Frontend.INSTRUCTION_DECOMPRESSED, priorId)(Riscv.rdRange)
            when(useRd && writeRd === archRs){
              (setup.waits(rsId).ENABLE, slotId) := True
              (setup.waits(rsId).ID    , slotId) := ROB_ID | priorId
            }
          }

          when(!useRs){
            (setup.waits(rsId).ENABLE, slotId) := False
          }
        }
      }
    }
    frontend.release()
  }
}
