package naxriscv.debug

import naxriscv.interfaces.PulseHandshake
import spinal.core._
import spinal.lib._

case class DebugModuleParameter(version : Int,
                                harts : Int,
                                progBufSize : Int,
                                datacount : Int)


object DebugDmToHartOp extends SpinalEnum{
  val DATA, INSTRUCTION, EXECUTE = newElement()
}

case class DebugDmToHart() extends Bundle{
  val op = DebugDmToHartOp()
  val address = UInt(5 bits)
  val data = Bits(32 bits)
}

case class DebugHartToDm() extends Bundle{
  val address = UInt(4 bits)
  val data = Bits(32 bits)
}

case class DebugHartBus() extends Bundle with IMasterSlave {
  val halted, running, unavailable = Bool()
  val resume, halt = PulseHandshake()

  val dmToHart = Flow(DebugDmToHart())
  val hartToDm = Flow(DebugHartToDm())

  override def asMaster() = {
    in(halted, running, unavailable)
    master(resume, halt)
    master(dmToHart)
    slave(hartToDm)
  }
}

object DebugModuleCmdErr extends SpinalEnum(binarySequential){
  val NONE, BUSY, NOT_SUPPORTED, EXCEPTION, HALT_RESUME, BUS, OTHER = newElement()
}

case class DebugModule(p : DebugModuleParameter) extends Component{
  val io = new Bundle {
    val ctrl = slave(DebugBus(7))
    val ndmreset = out Bool()
    val harts = Vec.fill(p.harts)(master(DebugHartBus()))
  }

  val factory = new DebugBusSlaveFactory(io.ctrl)

  val dmactive = factory.createReadAndWrite(Bool(), 0x10, 0) init(False)
  val dmCd = ClockDomain(ClockDomain.current.clock, reset = dmactive, config = ClockDomain.current.config.copy(resetKind = SYNC, resetActiveLevel = LOW))

  val logic = dmCd on new Area{
    val dmcontrol = new Area {
      val ndmreset = factory.createReadAndWrite(Bool(), 0x10, 1) init(False)
      val clrresethaltreq = null //TODO ?
      val setresethaltreq = null //TODO ?
      val hartreset = null

      val hartSelNew = factory.nonStopWrite(UInt(20 bits), 6)
      val hartSel = factory.createReadAndWrite(UInt(20 bits), 0x10, 6) init(0)
      val haltReq = factory.setOnSet(False, 0x10, 31)
      val resumeReq = factory.setOnSet(False, 0x10, 30) clearWhen(haltReq)

      io.harts.foreach{bus =>
        bus.resume.setIdle()
        bus.halt.setIdle()
      }
      io.harts.onSel(hartSelNew, relaxedWidth = true){bus =>
        bus.resume.request := resumeReq
        bus.halt.request := haltReq
      }

      io.ndmreset := ndmreset
    }

    val harts = for(hartId <- 0 until p.harts) yield new Area{
      val bus = io.harts(hartId)
      val resumeReady = !bus.resume.isPending()
    }

    val dmstatus = new Area{
      val version = factory.read(U(p.version, 4 bits), 0x11, 0)
      val authenticated = factory.read(True, 0x11, 7)

      val anyHalted = factory.read(io.harts.map(_.halted).orR, 0x11, 8)
      val allHalted = factory.read(io.harts.map(_.halted).andR, 0x11, 9)
      val anyRunning = factory.read(io.harts.map(_.running).orR, 0x11, 10)
      val allRunning = factory.read(io.harts.map(_.running).andR, 0x11, 11)
      val anyUnavail = factory.read(io.harts.map(_.unavailable).orR, 0x11, 12)
      val allUnavail = factory.read(io.harts.map(_.unavailable).andR, 0x11, 13)
      val anyNonExistent = factory.read(dmcontrol.hartSel >= p.harts, 0x11, 14)
      val allNonExistent = factory.read(anyNonExistent, 0x11, 15)
      val anyResumeAck = factory.read(harts.map(_.resumeReady).orR, 0x11, 16)
      val allResumeAck = factory.read(harts.map(_.resumeReady).andR, 0x11, 17)

      val hasresethaltreq = null // TODO ?
      val impebreak = null //TODO
    }

    val hartInfo = new Area{
      val dataaddr   = factory.read(U(0, 4 bits), 0x12, 0)
      val datasize   = factory.read(U(0, 4 bits), 0x12, 12)
      val dataaccess = factory.read(False, 0x12, 16)
      val nscratch   = factory.read(U(0, 4 bits), 0x12, 20)
    }

    val sbcs = new Area{
      val sbversion  = factory.read(U(1, 3 bits), 0x38, 29)
      val sbaccess   = factory.read(U(2, 3 bits), 0x38, 17)
    }

    val abstractcs = new Area{
      val dataCount = factory.read(U(p.datacount, 4 bits), 0x16, 0)
      val cmdErr = factory.createReadAndClearOnSet(DebugModuleCmdErr(), 0x16, 8) //TODO
      val busy = factory.createReadOnly(Bool(), 0x16, 12) init(False) //TODO
      val progBufSize = factory.read(U(p.progBufSize, 5 bits), 0x16, 24)
    }
  }

}
