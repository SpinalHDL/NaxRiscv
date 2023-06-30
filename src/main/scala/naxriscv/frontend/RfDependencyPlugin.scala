// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.frontend

import naxriscv.Frontend.DISPATCH_MASK
import naxriscv.{DecodeList, Frontend, Global, ROB, riscv}
import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces.{CommitService, DecoderService, InitCycles, IssueService, MicroOp, RegfileService, RegfileSpec, WakeRegFileService, WakeWithBypassService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.pipeline.{Stageable, StageableOffset}

import scala.collection.mutable.ArrayBuffer

case class DependencyUpdate(physDepth : Int, robDepth : Int) extends Bundle{
  val physical = UInt(log2Up(physDepth) bits)
  val robId = UInt(log2Up(robDepth) bits)
}

case class DependencyCommit(physicalDepth : Int) extends Bundle{
  val physical = UInt(log2Up(physicalDepth) bits)
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
  assert(isPow2(physDepth))
  val io = new Bundle {
    val writes = Vec.fill(writePorts)(slave(Flow(DependencyUpdate(physDepth, robDepth))))
    val commits = Vec.fill(commitPorts)(slave(Flow(DependencyCommit(physDepth))))
    val reads = Vec.fill(readPorts)(slave(DependencyRead(physDepth, robDepth)))
  }

  val translation = new Area{
    val physToRob = Mem.fill(physDepth)(UInt(log2Up(robDepth) bits))
    for(p <- io.writes){
      physToRob.write(
        address = p.physical,
        data = p.robId,
        enable = p.valid
      )
    }
  }

  val status = new Area{
    val busy = Mem.fill(physDepth)(Bool())
    val write = for(p <- io.writes){
      busy.write(
        address = p.physical,
        data = True,
        enable = p.valid
      )
    }
    val commit = for(p <- io.commits) yield new Area{
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
    val robId = translation.physToRob.readAsync(p.cmd.payload)
    val enabled = status.busy.readAsync(p.cmd.payload) //TODO as it was optimize, maybe we do not need the [dispatch] stage anymore in the frontend plugin
    p.rsp.valid := p.cmd.valid
    p.rsp.enable := enabled
    p.rsp.rob := robId
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

//Tracking depedeancies using physical registers avoid rollbacks, but require arch to phys translation first
class RfDependencyPlugin() extends Plugin with InitCycles{
  override def initCycles = logic.forRf.map(_.entryCount).max

  case class IssueSkipSpec(microOp: MicroOp, rsId : Int)
  val issueSkipSpecs = ArrayBuffer[IssueSkipSpec]()
  def issueSkipRs(microOp: MicroOp, rsId : Int): Unit ={
    issueSkipSpecs += IssueSkipSpec(microOp, rsId)
  }

  def getRsWait(rsId : Int) = setup.waits(rsId)

  val retains = create early new Area{
    getService[FrontendPlugin].retain()
    getService[IssueService].retain()
    getService[DecoderService].retain()
  }

  val setup = create late new Area{
    val issue    = getService[IssueService]
    val decoder  = getService[DecoderService]

    val waits = List.fill(decoder.rsCountMax())(issue.newRobDependency())
    val SKIP = List.fill(decoder.rsCountMax())(Stageable(Bool()))

    for(rsId <- 0 until decoder.rsCountMax()){
      decoder.addMicroOpDecodingDefault(SKIP(rsId), False)
    }
    val skipGroupe = issueSkipSpecs.groupByLinked(_.rsId)
    for((rsId, specs) <- skipGroupe; spec <- specs){
      decoder.addMicroOpDecoding(spec.microOp, DecodeList(SKIP(rsId) -> True))
    }

    issue.release()
    decoder.release()
  }

  val logic = create late new Area{
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val wakeIds = getServicesOf[WakeRegFileService].flatMap(_.wakeRegFile)
    val stage = frontend.pipeline.dispatch
    import stage._


    val forRf = for(rf <- getServicesOf[RegfileService]) yield new Area{
      val spec = rf.rfSpec
      val entryCount = rf.getPhysicalDepth
      val impl = new DependencyStorage(
        robDepth = ROB.SIZE,
        physDepth = entryCount,
        commitPorts = wakeIds.size,
        writePorts = Global.COMMIT_COUNT,
        readPorts = Frontend.DISPATCH_COUNT * decoder.rsCount(rf.rfSpec)
      )

      //Write
      for (slotId <- 0 until Frontend.DISPATCH_COUNT) {
        val port = impl.io.writes(slotId)
        port.valid := stage.isFireing && (decoder.WRITE_RD, slotId) && (DISPATCH_MASK, slotId) && (decoder.REGFILE_RD, slotId) === decoder.REGFILE_RD.rfToId(rf.rfSpec)
        port.physical := stage(decoder.PHYS_RD, slotId)
        port.robId := ROB.ID | slotId
      }

      //Commit
      for ((event, port) <- (wakeIds, impl.io.commits).zipped) {
        port.valid := event.valid && event.regfile === decoder.REGFILE_RD.rfToId(rf.rfSpec) //TODO FPU staticaly filter interface able to write the given RF
        port.physical := event.physical
      }

      val init = new Area {
        assert(isPow2(entryCount))
        val counter = Reg(UInt(log2Up(entryCount * 2) bits)) init (0)
        val busy = !counter.msb

        when(busy) {
          val port = impl.io.commits.head
          port.valid := True
          port.physical := counter.resized

          counter := counter + 1
        }
      }
    }.setCompositeName(this, "forRf_" + rf.rfSpec.getName())

    //Read
    val dependency = new Area{
      for(slotId <- 0 until Frontend.DISPATCH_COUNT) {
        for(rsId <- 0 until decoder.rsCountMax()) {
          val rsRfSpecs = getServicesOf[RegfileService].map(_.rfSpec)
          val archRs = (decoder.ARCH_RS(rsId), slotId)
          val useRs = (decoder.READ_RS(rsId), slotId)
          val rfs = for(rfSpec <- rsRfSpecs if rsId < decoder.rsCount(rfSpec)) yield new Area{
            val rf = rfSpec
            val port = forRf.find(_.spec == rf).get.impl.io.reads(slotId*decoder.rsCount(rfSpec)+rsId)
            port.cmd.valid := isValid && (DISPATCH_MASK, slotId) && useRs
            port.cmd.payload := stage(decoder.PHYS_RS(rsId), slotId)
          }

          val rsp = stage(decoder.REGFILE_RS(rsId), slotId).muxListDc(rsRfSpecs.filter(rsId < decoder.rsCount(_)).map{rf =>
            decoder.REGFILE_RS(rsId).rfToId(rf) -> rfs.find(_.rf == rf).get.port.rsp
          })
          (setup.waits(rsId).ENABLE_UNSKIPED, slotId) := rsp.enable
          (setup.waits(rsId).ID    , slotId) := rsp.rob

          //Slot write bypass
          //TODO maybe the bypass of the RfTranslationPlugin can be ignored ?
          for(priorId <- 0 until slotId){
            val useRd = (decoder.WRITE_RD, priorId) && (DISPATCH_MASK, priorId)
            val writeRd = (decoder.ARCH_RD, priorId)
            val sameRegfile = (decoder.REGFILE_RD, priorId) === (decoder.REGFILE_RS(rsId), slotId)
            when(useRd && sameRegfile && writeRd === archRs){
              (setup.waits(rsId).ENABLE_UNSKIPED, slotId) := True
              (setup.waits(rsId).ID    , slotId) := ROB.ID | priorId
            }
          }
          for(wake <- wakeIds; if wake.needBypass){
            when(wake.valid && wake.physical === stage(decoder.PHYS_RS(rsId), slotId) && wake.regfile === (decoder.REGFILE_RS(rsId), slotId)){
              (setup.waits(rsId).ENABLE_UNSKIPED, slotId) := False
            }
          }
          when(!useRs){
            (setup.waits(rsId).ENABLE_UNSKIPED, slotId) := False
          }
          (setup.waits(rsId).ENABLE, slotId) := (setup.waits(rsId).ENABLE_UNSKIPED, slotId) && !(setup.SKIP(rsId), slotId)
        }
      }
    }
    frontend.release()
  }
}
