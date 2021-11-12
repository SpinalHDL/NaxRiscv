package naxriscv.frontend

import naxriscv.Frontend.{DISPATCH_MASK, ROB_ID}
import naxriscv.{Frontend, Global, ROB, riscv}
import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces.{CommitService, DecoderService, InitCycles, IssueService, RegfileService, RegfileSpec, WakeService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class DependencyUpdate(physDepth : Int, robDepth : Int) extends Bundle{
  val physical = UInt(log2Up(physDepth) bits)
  val rob = UInt(log2Up(robDepth) bits)
}

case class DependencyCommit(physDepth : Int) extends Bundle{
  val physical = UInt(log2Up(physDepth) bits)
}

case class DependencyReadRsp(robDepth : Int) extends Bundle{
  val enable = Bool()
  val rob = UInt(log2Up(robDepth) bits)
}

case class DependencyRead(physDepth : Int, robDepth : Int) extends Bundle with IMasterSlave {
  val cmd = Flow(UInt(log2Up(physDepth) bits))
  val rsp = Flow(DependencyReadRsp(robDepth))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

class DependencyStorage(physDepth : Int,
                        robDepth : Int,
                        commitPorts : Int,
                        writePorts : Int,
                        readPorts : Int) extends Component {
  val io = new Bundle {
    val writes = Vec.fill(writePorts)(slave(Flow(DependencyUpdate(physDepth, robDepth))))
    val commits = Vec.fill(commitPorts)(slave(Flow(DependencyCommit(physDepth))))
    val reads = Vec.fill(readPorts)(slave(DependencyRead(physDepth, robDepth)))
  }

  val translation = new Area{
    val storage = Mem.fill(physDepth)(UInt(log2Up(robDepth) bits))
    for(p <- io.writes){
      storage.write(
        address = p.physical,
        data = p.rob,
        enable = p.valid
      )
    }
  }

  val status = new Area{
    val busy = Mem.fill(physDepth)(Bool())
    for(p <- io.writes){
      busy.write(
        address = p.physical,
        data = True,
        enable = p.valid
      )
    }
    for(p <- io.commits) yield new Area{
      when(p.valid){
        busy.write(
          address = p.physical,
          data = False,
          enable = p.valid
        )
      }
    }
  }

  val read = for(p <- io.reads) yield new Area{
    val translated = translation.storage.readAsync(p.cmd.payload)
    val enabled = status.busy.readAsync(p.cmd.payload)
    p.rsp.valid := p.cmd.valid
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
    physDepth   = 64,
    robDepth    = 64,
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
/*
Artix 7 -> 225 Mhz 180 LUT 303 FF
Artix 7 -> 379 Mhz 180 LUT 303 FF
 */

class RfDependencyPlugin() extends Plugin with InitCycles{
  override def initCycles = logic.entryCount

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

    val stage = frontend.pipeline.allocated
    import stage._

    val entryCount = decoder.rsPhysicalDepthMax

    val impl = new DependencyStorage(
      robDepth    = ROB.SIZE,
      physDepth   = entryCount,
      commitPorts = wakeIds.size,
      writePorts  = Global.COMMIT_COUNT,
      readPorts   = Frontend.DISPATCH_COUNT*decoder.rsCount
    )

    //Write
    for(slotId <- 0 until Frontend.DISPATCH_COUNT) {
      val port = impl.io.writes(slotId)
      port.valid := stage.isFireing && (decoder.WRITE_RD, slotId) && (DISPATCH_MASK, slotId)
      port.physical := stage(decoder.PHYS_RD, slotId)
      port.rob := ROB_ID | slotId
    }

    //Commit
    for((event, port) <- (wakeIds, impl.io.commits).zipped){
      port.valid := event.valid
      port.physical := event.payload
    }

    val init = new Area{
      assert(isPow2(entryCount))
      val counter = Reg(UInt(log2Up(entryCount*2) bits)) init (0)
      val busy = !counter.msb

      when(busy) {
        val port = impl.io.commits.head
        port.valid := True
        port.physical := counter.resized

        counter := counter + 1
      }
    }

    //Read
    val dependency = new Area{
      for(slotId <- 0 until Frontend.DISPATCH_COUNT) {
        for(rsId <- 0 until decoder.rsCount) {
          val archRs = (decoder.ARCH_RS(rsId), slotId)
          val useRs = (decoder.READ_RS(rsId), slotId)
          val port = impl.io.reads(slotId*decoder.rsCount+rsId)
          port.cmd.valid := isValid && (DISPATCH_MASK, slotId) && useRs
          port.cmd.payload := stage(decoder.PHYS_RS(rsId), slotId)
          (setup.waits(rsId).ENABLE, slotId) := port.rsp.enable
          (setup.waits(rsId).ID    , slotId) := port.rsp.rob

          //Slot write bypass
          //TODO maybe the bypass of the RfTranslationPlugin can be ignored ?
          for(priorId <- 0 until slotId){
            val useRd = (decoder.WRITE_RD, priorId) && (DISPATCH_MASK, priorId)
            val writeRd = (decoder.ARCH_RD, priorId)
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
