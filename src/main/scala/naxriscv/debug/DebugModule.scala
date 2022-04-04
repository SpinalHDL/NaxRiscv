package naxriscv.debug

import naxriscv.Global
import naxriscv.Global.XLEN
import naxriscv.interfaces.PulseHandshake
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

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

object DebugModule{
  val CSR_DATA = 0x7B4
  def csrr(dataId : Int, regId : UInt) = B(0x00002073 | (CSR_DATA + dataId << 20), 32 bits) | (regId.asBits << 7).resized
  def csrw(dataId : Int, regId : UInt) = B(0x00001073 | (CSR_DATA + dataId << 20), 32 bits) | (regId.asBits << 15).resized
  def ebreak() = B(0x00100073, 32 bits)
}

case class DebugModule(p : DebugModuleParameter) extends Component{
  import DebugModule._
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


    val toHarts = Flow(DebugDmToHart()).setIdle()
    val fromHarts = Flow(DebugHartToDm())
    fromHarts.valid := io.harts.map(_.hartToDm.valid).orR
    fromHarts.payload := MuxOH.or(io.harts.map(_.hartToDm.valid), io.harts.map(_.hartToDm.payload), bypassIfSingle = true)
    val harts = for(hartId <- 0 until p.harts) yield new Area{
      val bus = io.harts(hartId)
      bus.dmToHart << toHarts
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

    val progbufX = new Area{
      val trigged = io.ctrl.cmd.valid && io.ctrl.cmd.write && (io.ctrl.cmd.address & 0x70) === 0x20
      when(trigged){
        factory.cmdToRsp.error := False
        toHarts.valid   := True
        toHarts.op      := DebugDmToHartOp.INSTRUCTION
        toHarts.address := io.ctrl.cmd.address.resized
        toHarts.data    := io.ctrl.cmd.data
      }
    }

    val dataX = new Area{
      val readMem = Mem.fill(p.datacount)(Bits(32 bits))
      readMem.write(
        address = fromHarts.address.resized,
        data = fromHarts.data,
        enable = fromHarts.valid
      )

      val trigged = False
      val cmdAddress = (io.ctrl.cmd.address - 4).resize(log2Up(p.datacount) bits)
      when(io.ctrl.cmd.valid && io.ctrl.cmd.address >= 0x04 &&  io.ctrl.cmd.address < 0x04 + p.datacount){
        trigged := True

        toHarts.valid   setWhen(io.ctrl.cmd.write)
        toHarts.op      := DebugDmToHartOp.DATA
        toHarts.address := cmdAddress.resized
        toHarts.data    := io.ctrl.cmd.data

        factory.cmdToRsp.error := False
        factory.cmdToRsp.data := readMem.readAsync(cmdAddress.resized)
      }
    }

    val abstracts = new Area{
      val dataCount = factory.read(U(p.datacount, 4 bits), 0x16, 0)
      val cmdErr = factory.createReadAndClearOnSet(DebugModuleCmdErr(), 0x16, 8) init(DebugModuleCmdErr.NONE)
      val busy = factory.createReadOnly(Bool(), 0x16, 12) init(False) //TODO
      val progBufSize = factory.read(U(p.progBufSize, 5 bits), 0x16, 24)
    }

    val abstractAuto = new Area{
      val autoexecdata = factory.createReadAndWrite(Bits(p.datacount bits), 0x18, 0) init(0)
      val autoexecProgbuf = factory.createReadAndWrite(Bits(p.progBufSize bits), 0x18, 16) init(0)

      val trigger = progbufX.trigged && autoexecProgbuf(io.ctrl.cmd.address.resized) || dataX.trigged && autoexecdata(dataX.cmdAddress.resized)
    }

    val command = new StateMachine{
      val IDLE, DECODE, READ_REG_0, WRITE_REG_0, READ_REG_EBREAK, READ_REG_START, WAIT_DONE, POST_EXEC, POST_EXEC_WAIT = new State()

      setEntry(IDLE)

      val enabled = io.ctrl.cmd.valid && io.ctrl.cmd.address === 0x17 && io.ctrl.cmd.write
      val data = Reg(Bits(32 bits))
      val access = new Area{
        case class Args() extends Bundle{
          val regno = UInt(16 bits)
          val write = Bool()
          val transfer = Bool()
          val postExec = Bool()
          val aarpostincrement = Bool()
          val aarsize = UInt(3 bits)
        }
        val args = data.as(Args())
        val notSupported = args.aarsize > log2Up(Global.XLEN/8) || args.aarpostincrement || args.regno(5, 11 bits) =/= 0x1000 >> 5
      }

      val selected = new Area{
        val hart = Reg(UInt(log2Up(p.harts) bits))
        val running = io.harts.map(_.running).read(hart)
        val halted = io.harts.map(_.halted).read(hart)
        val completion = halted.rise(False)
      }


      IDLE.whenIsActive{
        when(enabled || abstractAuto.trigger) {
          selected.hart := dmcontrol.hartSel.resized
          goto(DECODE)
        }
        when(enabled){
          factory.writeHalt()
          data := io.ctrl.cmd.data
        }
      }
      DECODE.whenIsActive{
        factory.cmdToRsp.error := False
        goto(IDLE)
        switch(io.ctrl.cmd.data(31 downto 24)) {
          is(0) { //access register
            when(access.notSupported) {
              abstracts.cmdErr := DebugModuleCmdErr.NOT_SUPPORTED
            } otherwise {
              when(access.args.postExec){
                goto(POST_EXEC)
              }
              when(access.args.transfer) {
                when(access.args.write) {
                  goto(WRITE_REG_0)
                } otherwise {
                  goto(READ_REG_0)
                }
              }
            }
          }
          default{
            abstracts.cmdErr := DebugModuleCmdErr.NOT_SUPPORTED
          }
        }
      }

      def writeInstruction(states : (State, State))(address : Int, instruction : Bits): Unit ={
        states._1 whenIsActive {
          toHarts.valid := True
          toHarts.op := DebugDmToHartOp.INSTRUCTION
          toHarts.address := address
          toHarts.data := instruction
          goto(states._2)
        }
      }
      def startInstruction(states : (State, State)) (at : Int) : Unit ={
        states._1.whenIsActive{
          toHarts.valid := True
          toHarts.op := DebugDmToHartOp.EXECUTE
          toHarts.address := at
          goto(states._2)
        }
      }
      assert(XLEN.get == 32)
      writeInstruction(WRITE_REG_0     -> READ_REG_EBREAK) (12, csrr(0, access.args.regno(4 downto 0)))
      writeInstruction(READ_REG_0      -> READ_REG_EBREAK) (12, csrw(0, access.args.regno(4 downto 0)))
      writeInstruction(READ_REG_EBREAK -> READ_REG_START ) (13, ebreak())
      startInstruction(READ_REG_START  -> WAIT_DONE      ) (12)


      WAIT_DONE.whenIsActive{
        when(selected.completion){
          goto(IDLE)
          when(access.args.postExec){
            goto (POST_EXEC)
          }
        }
      }

      startInstruction(POST_EXEC  -> POST_EXEC_WAIT      ) (0)
      POST_EXEC_WAIT.whenIsActive{
        when(selected.completion) {
          goto(IDLE)
        }
      }
    }
  }

}
