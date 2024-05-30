// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.misc

import naxriscv.{Fetch, Frontend, Global}
import naxriscv.Global._
import naxriscv.execute.EnvCallPlugin.{CAUSE_FLUSH, CAUSE_REDO, CAUSE_XRET}
import naxriscv.execute.{CsrAccessPlugin, EnvCallPlugin}
import naxriscv.fetch.{AlignerPlugin, FetchCachePlugin, FetchPlugin, PcPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.JumpService.Priorities
import naxriscv.interfaces.{CommitService, CsrListFilter, CsrRamService, DecoderService, FetchInjector, PrivilegedService}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.cpu.riscv.debug.{DebugDmToHartOp, DebugHartBus, DebugModule}
import spinal.lib.fsm._
import spinal.lib.pipeline.StageableOffset

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PrivilegedConfig{
  def full = PrivilegedConfig(
    withSupervisor = true,
    withUser       = true,
    withUserTrap   = false,
    withRdTime     = true,
    withDebug      = false,
    vendorId       = 0,
    archId         = 5, //As spike
    impId          = 0,
    hartId         = 0,
    debugTriggers  = 0
  )
}

case class PrivilegedConfig(withSupervisor : Boolean,
                            withUser: Boolean,
                            withUserTrap: Boolean,
                            withRdTime : Boolean,
                            withDebug: Boolean,
                            debugTriggers : Int,
                            vendorId: Int,
                            archId: Int,
                            impId: Int,
                            var hartId: Int) {

}



class PrivilegedPlugin(var p : PrivilegedConfig) extends Plugin with PrivilegedService{
  override def hasMachinePriv = setup.withMachinePrivilege
  override def hasSupervisorPriv = setup.withSupervisorPrivilege
  override def getPrivilege() = setup.privilege
  override def xretAwayFromMachine = setup.xretAwayFromMachine

  override def implementSupervisor = p.withSupervisor
  override def implementUserTrap = p.withUserTrap
  override def implementUser = p.withUser

  override def setFpDirty() = setup.setFpDirty := True
  override def isFpuEnabled() = setup.isFpuEnabled


  create config{
    Global.RV_DEBUG.set(p.withDebug)
  }


  val misaIds = mutable.LinkedHashSet[Int]()
  override def addMisa(id: Int) = {
    misaIds += id
  }
  override def addMisa(id: Char) = super.addMisa(id)

  val io = create early new Area{
    val int = new Area{
      val machine = new Area{
        val timer    = Verilator.public(in Bool())
        val software = Verilator.public(in Bool())
        val external = Verilator.public(in Bool())
      }
      val supervisor = p.withSupervisor generate new Area{
        val external = Verilator.public(in Bool())
      }
      val user = p.withUserTrap generate new Area{
        val external = Verilator.public(in Bool())
      }
    }

    val rdtime = in UInt(64 bits)
  }

  val setup = create early new Area{
    val csr = getService[CsrAccessPlugin]
    val ram = getService[CsrRamPlugin]
    val fetch = getService[FetchPlugin]
    val frontend = getService[FrontendPlugin]
    val rob = getService[RobPlugin]
    csr.retain()
    ram.allocationLock.retain()
    fetch.retain()
    rob.retain()
    frontend.retain()

    val jump = getService[PcPlugin].createJumpInterface(Priorities.COMMIT_TRAP).setIdle()
    val ramRead  = ram.ramReadPort(CsrRamService.priority.TRAP)
    val ramWrite = ram.ramWritePort(CsrRamService.priority.TRAP)

    val injector = p.withDebug generate getService[FetchInjector].injectPort()

    val debugMode = p.withDebug generate Bool()
    val privilege = RegInit(U"11")
    val withMachinePrivilege    = privilege >= U"11"
    val withSupervisorPrivilege = privilege >= U"01"
    val xretAwayFromMachine = False

    addMisa('I')
    if(RVC) addMisa('C')
    if(RVF) addMisa('F')
    if(RVD) addMisa('D')
    if(p.withUser) addMisa('U')
    if(p.withSupervisor) addMisa('S')

    val debugBus = p.withDebug generate slave(DebugHartBus())

    val trapEvent = False
    val redoTriggered = False
    val setFpDirty = False
    val isFpuEnabled = False
  }

  val logic = create late new Area{
    val csr = setup.csr
    val ram = setup.ram
    val fetch = setup.fetch
    val rob = setup.rob
    val frontend = setup.frontend
    val decoder = getService[DecoderService]
    val commit = getService[CommitService]
    val doc = getService[DocPlugin]


    case class Delegator(var enable : Bool, privilege : Int)
    case class InterruptSpec(var cond : Bool, id : Int, privilege : Int, delegators : List[Delegator])
    case class ExceptionSpec(id : Int, delegators : List[Delegator])
    var interruptSpecs = ArrayBuffer[InterruptSpec]()
    var exceptionSpecs = ArrayBuffer[ExceptionSpec]()

    def addInterrupt(cond : Bool, id : Int, privilege : Int, delegators : List[Delegator]): Unit = {
      interruptSpecs += InterruptSpec(cond, id, privilege, delegators)
    }


    val defaultTrap = new Area{
      val csrPrivilege = csr.onDecodeAddress(8, 2 bits)
      val csrReadOnly  = csr.onDecodeAddress(10, 2 bits).andR
      when(csrReadOnly && csr.onDecodeWrite || csrPrivilege > setup.privilege){
        csr.onDecodeTrap()
      }
    }

    val debug = p.withDebug generate new Area{
      def bus = setup.debugBus

      val running = RegInit(True)
      setup.debugMode := !running

      fetch.getStage(0).haltIt(!running)

      bus.hartToDm.setIdle()
      bus.exception := False
      bus.ebreak := False
      bus.redo := False
      bus.commit := setup.debugMode && commit.onCommit().mask.orR
      bus.resume.rsp.valid := False

      bus.running :=  running
      bus.halted  := !running
      bus.unavailable := BufferCC(ClockDomain.current.isResetActive)
      bus.regSuccess := False
      val enterHalt = running.getAheadValue().fall(False)

      val reseting   = RegNext(False) init(True)
      bus.haveReset := RegInit(False) setWhen(reseting) clearWhen(bus.ackReset)

      val doHalt = RegInit(False) setWhen(bus.haltReq && bus.running && !setup.debugMode) clearWhen(enterHalt)
      val doResume = bus.resume.isPending(1)

      val execute = new Area{
        setup.injector.valid := bus.dmToHart.valid && bus.dmToHart.op === DebugDmToHartOp.EXECUTE
        setup.injector.payload := bus.dmToHart.data
      }

      val dataCsrr = new Area{
        val state = RegInit(U(0, log2Up(XLEN/32) bits))

        csr.onWrite(DebugModule.CSR_DATA, onlyOnFire = false){
          when(state =/= state.maxValue) {
            csr.onWriteHalt()
          }
          bus.hartToDm.valid := True
          bus.hartToDm.address := state.resized
          bus.hartToDm.data  := csr.onWriteBits.subdivideIn(32 bits).read(state)
          state := (state + 1).resized
        }
      }

      val dataCsrw = new Area{
        val value = Reg(Bits(XLEN bits))

        val fromDm = new Area{
          when(bus.dmToHart.valid && bus.dmToHart.op === DebugDmToHartOp.DATA){
            value.subdivideIn(32 bits).onSel(bus.dmToHart.address, relaxedWidth = true){ chunk =>
              chunk := bus.dmToHart.data
            }
          }
        }

        val toHart = new Area{
          csr.read(value, DebugModule.CSR_DATA)
        }
      }

      val dpc = csr.readWriteRam(CSR.DPC)
      val dcsr = new Area{
        val prv = RegInit(U"11")
        val step = RegInit(False) //TODO
        val nmip = False
        val mprven = True
        val cause = RegInit(U"000")
        val stoptime = False
        val stopcount = False
        val stepie = RegInit(False) //TODO
        val ebreaku = p.withUser generate RegInit(False)
        val ebreaks = p.withSupervisor generate RegInit(False)
        val ebreakm = RegInit(False)
        val xdebugver = U(4, 4 bits)

        val stepLogic = new StateMachine{
          val IDLE, SINGLE, WAIT, DELAY = new State()
          setEntry(IDLE)

          val stage = frontend.pipeline.aligned
          val isCause = RegInit(False)

          IDLE whenIsActive{
            when(step && bus.resume.rsp.valid){
              goto(SINGLE)
            }
          }
          SINGLE whenIsActive{
            getService[AlignerPlugin].setSingleFetch()
            when(stage.isFireing){
              goto(WAIT)
            }
          }
          WAIT whenIsActive{
            stage.haltIt()
            when(commit.onCommit().mask =/= 0 || setup.trapEvent){ //TODO
              doHalt := True
              isCause := True
              goto(DELAY)
            }
            when(setup.redoTriggered){
              goto(SINGLE)
            }
          }
          DELAY whenIsActive{
            stage.haltIt()
            goto(IDLE)
          }

          always{
            when(enterHalt){
              goto(IDLE)
            }
          }
          build()
        }


        csr.read(CSR.DCSR, 3 -> nmip, 6 -> cause,  4 -> mprven, 9 -> stoptime, 10 -> stopcount, 28 -> xdebugver)
        csr.readWrite(CSR.DCSR, 0 -> prv, 2 -> step, 11 -> stepie, 15 -> ebreakm)
        if(p.withSupervisor) csr.readWrite(CSR.DCSR, 13 -> ebreaks)
        if(p.withUser)       csr.readWrite(CSR.DCSR, 12 -> ebreaku)

        when(csr.onDecodeRead || csr.onDecodeWrite){
          when(!setup.debugMode && csr.onDecodeAddress >> 4 === 0x7B){
            csr.onDecodeTrap()
          }
        }
      }

      //Very limited subset of the trigger spec
      val trigger = (p.debugTriggers > 0) generate new Area {
        val tselect = new Area{
          val index = Reg(UInt(log2Up(p.debugTriggers) bits))
          csr.readWrite(index, CSR.TSELECT)

          val outOfRange = if(isPow2(p.debugTriggers)) False else index < p.debugTriggers
        }

        val tinfo = new Area{
          csr.read(CSR.TINFO, 0 -> tselect.outOfRange, 2 -> !tselect.outOfRange)
        }

        val slots = for(slotId <- 0 until p.debugTriggers) yield new Area {
          val selected = tselect.index === slotId
          def csrw(csrId : Int, thats : (Int, Data)*): Unit ={
            csr.onWrite(csrId, onlyOnFire = true){
              when(selected) {
                for((offset, data) <- thats){
                  data.assignFromBits(csr.onWriteBits(offset, widthOf(data) bits))
                }
              }
            }
          }
          def csrr(csrId : Int, read : Bits, thats : (Int, Data)*): Unit ={
            when(selected) {
              for((offset, data) <- thats){
                read(offset, widthOf(data) bits) := data.asBits
              }
            }
          }
          def csrrw(csrId : Int, read : Bits, thats : (Int, Data)*) : Unit = {
            csrw(csrId, thats :_*)
            csrr(csrId, read, thats :_*)
          }

          val tdata1 = new Area{
            val read = B(0, XLEN bits)
            val tpe = U(2, 4 bits)
            val dmode = Reg(Bool()) init(False)

            val execute = RegInit(False)
            val m, s, u = RegInit(False)
            val action = RegInit(U"0000")
            val privilegeHit = !setup.debugMode && setup.privilege.mux(
              0 -> u,
              1 -> s,
              3 -> m,
              default -> False
            )

            csrrw(CSR.TDATA1, read, 2 -> execute , 3 -> u, 4-> s, 6 -> m, 32 - 5 -> dmode, 12 -> action)
            csrr(CSR.TDATA1, read, 32 - 4 -> tpe)

            //TODO action sizelo timing select sizehi maskmax
          }

          val tdata2 = new Area{
            val value = Reg(PC)
            csrw(CSR.TDATA2, 0 -> value)


            val execute = new Area{
              val enabled = !setup.debugMode && tdata1.action === 1 && tdata1.execute && tdata1.privilegeHit
              val slots = for(i <- 0 until Frontend.DECODE_COUNT) yield new Area {
                val hit = enabled && value === frontend.pipeline.decoded(PC, i)
                when(hit){
                  decoder.debugEnter(i)
                }
              }
            }
          }
        }

        csr.read(CSR.TDATA1, 0 -> slots.map(_.tdata1.read).read(tselect.index))
      }
    }

    val withFs = RVF || p.withSupervisor //See spike XD

    val machine = new Area {
      val cause = new Area{
        val interrupt = RegInit(False)
        val code = Reg(UInt(commit.rescheduleCauseWidth bits)) init(0)
      }
      val mstatus = new Area{
        val mie, mpie = RegInit(False)
        val mpp = RegInit(U"00")
        val fs = withFs generate RegInit(U"00")
        val sd = False
        if(RVF) setup.isFpuEnabled setWhen(fs =/= 0)
        if(withFs) sd setWhen(fs === 3)

        val tsr, tw, tvm = p.withSupervisor generate RegInit(False)
      }
      val mip = new Area{
        val meip = RegNext(io.int.machine.external) init(False)
        val mtip = RegNext(io.int.machine.timer)    init(False)
        val msip = RegNext(io.int.machine.software) init(False)
      }
      val mie = new Area{
        val meie, mtie, msie = RegInit(False)
      }

      val medeleg = p.withSupervisor generate new Area {
        val iam, bp, eu, es, ipf, lpf, spf = RegInit(False)
        val mapping = mutable.LinkedHashMap(0 -> iam, 3 -> bp, 8 -> eu, 9 -> es, 12 -> ipf, 13 -> lpf, 15 -> spf)
      }

      val mideleg = new Area {
        val st, se, ss = RegInit(False)
      }

      val tvec    = csr.readWriteRam(CSR.MTVEC)
      val tval    = csr.readWriteRam(CSR.MTVAL)
      val epc     = csr.readWriteRam(CSR.MEPC)
      val scratch = csr.readWriteRam(CSR.MSCRATCH)

      csr.read(U(p.vendorId),CSR.MVENDORID) // MRO Vendor ID.
      csr.read(U(p.archId),  CSR.MARCHID) // MRO Architecture ID.
      csr.read(U(p.impId),   CSR.MIMPID) // MRO Implementation ID.
      csr.read(U(p.hartId),  CSR.MHARTID) // MRO Hardware thread ID.Machine Trap Setup
      val misaExt = misaIds.map(1l << _).reduce(_ | _)
      val misaMxl = XLEN.get match {
        case 32 => BigInt(1) << XLEN.get-2
        case 64 => BigInt(2) << XLEN.get-2
      }
      val misa = misaMxl | misaExt
      csr.read(U(misa, XLEN bits), CSR.MISA) // MRW ISA and extensions

      csr.readWrite(CSR.MCAUSE, XLEN-1 -> cause.interrupt, 0 -> cause.code)
      csr.readWrite(CSR.MSTATUS, 11 -> mstatus.mpp, 7 -> mstatus.mpie, 3 -> mstatus.mie)
      csr.read     (CSR.MSTATUS, XLEN-1 -> mstatus.sd)
      csr.read     (CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip, 3 -> mip.msip)
      csr.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)
      if(p.withSupervisor) csr.readWrite(CSR.MSTATUS, 22 -> mstatus.tsr, 21 -> mstatus.tw, 20 -> mstatus.tvm)


      if(withFs) csr.readWrite(CSR.MSTATUS, 13 -> mstatus.fs)
      //TODO FPU trap illegal instruction if FPU instruction and "00"
      val rvf = RVF.get generate new Area {


        val csrDirty = CsrListFilter(List(CSR.FRM, CSR.FCSR, CSR.FFLAGS))
        csr.onWrite(csrDirty, true){
          setFpDirty()
        }

        //Reschedule when the FPU status is changing
        val fpuStatusChange = CsrListFilter(List(CSR.MSTATUS) ++ Some(CSR.SSTATUS).filter(_ => p.withSupervisor))
        csr.onWrite(fpuStatusChange, false){
          when(csr.onWriteBits(13, 2 bits).asUInt =/= mstatus.fs){
            csr.onWriteFlushPipeline()
          }
        }

        when(setup.setFpDirty){
          mstatus.fs := 3
        }
      }

      if(p.withSupervisor) {
        for((id, enable) <- medeleg.mapping) csr.readWrite(CSR.MEDELEG, id -> enable)
        csr.readWrite(CSR.MIDELEG, 9 -> mideleg.se, 5 -> mideleg.st, 1 -> mideleg.ss)
        if(XLEN.get == 64){
          csr.read(CSR.MSTATUS, 34 -> U"10")
        }
      }

      addInterrupt(mip.mtip && mie.mtie, id = 7,  privilege = 3, delegators = Nil)
      addInterrupt(mip.msip && mie.msie, id = 3,  privilege = 3, delegators = Nil)
      addInterrupt(mip.meip && mie.meie, id = 11, privilege = 3, delegators = Nil)
    }

    val supervisor = p.withSupervisor generate new Area {
      val cause = new Area{
        val interrupt = RegInit(False)
        val code = Reg(UInt(commit.rescheduleCauseWidth bits)) init(0)
      }

      val sstatus = new Area{
        val sie, spie = RegInit(False)
        val spp = RegInit(U"0")
      }

      val sip = new Area {
        val seipSoft = RegInit(False)
        val seipInput = RegNext(io.int.supervisor.external)
        val seipOr = seipSoft || seipInput
        val stip = RegInit(False)
        val ssip = RegInit(False)

        val seipMasked = seipOr && machine.mideleg.se
        val stipMasked = stip   && machine.mideleg.st
        val ssipMasked = ssip   && machine.mideleg.ss
      }

      val sie = new Area{
        val seie, stie, ssie = RegInit(False)
      }

      val tvec    = csr.readWriteRam(CSR.STVEC)
      val tval    = csr.readWriteRam(CSR.STVAL)
      val epc     = csr.readWriteRam(CSR.SEPC)
      val scratch = csr.readWriteRam(CSR.SSCRATCH)

      csr.readWrite(CSR.SCAUSE, XLEN-1 -> cause.interrupt, 0 -> cause.code)

      for(offset <- List(CSR.MSTATUS, CSR.SSTATUS))  csr.readWrite(offset, 8 -> sstatus.spp, 5 -> sstatus.spie, 1 -> sstatus.sie)
      if(withFs)  csr.readWrite(CSR.SSTATUS, 13 -> machine.mstatus.fs)
      csr.read(CSR.SSTATUS, XLEN-1 -> machine.mstatus.sd)

      def mapMie(machineCsr : Int, supervisorCsr : Int, bitId : Int, reg : Bool, machineDeleg : Bool, sWrite : Boolean = true): Unit ={
        csr.read(reg, machineCsr, bitId)
        csr.write(reg, machineCsr, bitId)
        csr.read(reg && machineDeleg, supervisorCsr, bitId)
        if(sWrite) csr.writeWhen(reg, machineDeleg, supervisorCsr, bitId)
      }
      mapMie(CSR.MIE, CSR.SIE, 9, sie.seie, machine.mideleg.se)
      mapMie(CSR.MIE, CSR.SIE, 5, sie.stie, machine.mideleg.st)
      mapMie(CSR.MIE, CSR.SIE, 1, sie.ssie, machine.mideleg.ss)

      csr.read(sip.seipOr, CSR.MIP, 9)
      csr.write(sip.seipSoft, CSR.MIP, 9)
      csr.read(sip.seipOr && machine.mideleg.se, CSR.SIP, 9)
      mapMie(CSR.MIP, CSR.SIP, 5, sip.stip, machine.mideleg.st, sWrite = false)
      mapMie(CSR.MIP, CSR.SIP, 1, sip.ssip, machine.mideleg.ss)
      csr.readToWrite(sip.seipSoft, CSR.MIP, 9) //Avoid an external interrupt value to propagate to the soft external interrupt register.


      addInterrupt(sip.ssip && sie.ssie,    id = 1, privilege = 1, delegators = List(Delegator(machine.mideleg.ss, 3)))
      addInterrupt(sip.stip && sie.stie,    id = 5, privilege = 1, delegators = List(Delegator(machine.mideleg.st, 3)))
      addInterrupt(sip.seipOr && sie.seie,  id = 9, privilege = 1, delegators = List(Delegator(machine.mideleg.se, 3)))

      for((id, enable) <- machine.medeleg.mapping) exceptionSpecs += ExceptionSpec(id, List(Delegator(enable, 3)))

      if(XLEN.get == 64){
        csr.read(CSR.MSTATUS, 32 -> U"10")
        csr.read(CSR.SSTATUS, 32 -> U"10")
      }
    }

    val userTrap = p.withUserTrap generate new Area {
      val tvec    = csr.readWriteRam(CSR.UTVEC)
      val tval    = csr.readWriteRam(CSR.UTVAL)
      val epc     = csr.readWriteRam(CSR.UEPC)
      val scratch = csr.readWriteRam(CSR.USCRATCH)
    }

    if(p.withRdTime) {
      XLEN.get match {
        case 32 => {
          csr.read(io.rdtime(31 downto 0), CSR.UTIME)
          csr.read(io.rdtime(63 downto 32), CSR.UTIMEH)
        }
        case 64 => {
          csr.read(io.rdtime, CSR.UTIME)
        }
      }
    }

    csr.release()
    ram.allocationLock.release()



    val rescheduleUnbuffered = Stream(new Bundle{
      val cause      = UInt(commit.rescheduleCauseWidth bits)
      val epc        = PC()
      val tval       = Bits(TVAL_WIDTH bits)
      val slices     = Fetch.INSTRUCTION_SLICE_COUNT()
      val fromCommit = Bool() //Ensure commited things have priority over interrupts as their may have side effect
      val debugEnter = p.withDebug generate Bool()
    })
    val reschedule = rescheduleUnbuffered.stage()

    val cr = commit.reschedulingPort(onCommit = true)
    rescheduleUnbuffered.valid := cr.valid && cr.trap
    rescheduleUnbuffered.cause := cr.cause
    rescheduleUnbuffered.epc   := rob.readAsyncSingle(Global.PC, cr.robId)
    rescheduleUnbuffered.slices := rob.readAsyncSingle(Fetch.INSTRUCTION_SLICE_COUNT, cr.robId)
    rescheduleUnbuffered.tval  := cr.tval
    rescheduleUnbuffered.fromCommit := cr.valid && cr.trap
    if(p.withDebug) rescheduleUnbuffered.debugEnter := False

    val dt = decoder.getTrap()
    when(dt.valid && !rescheduleUnbuffered.fromCommit) {
      rescheduleUnbuffered.valid := True
      rescheduleUnbuffered.cause := dt.cause
      rescheduleUnbuffered.epc   := dt.epc
      rescheduleUnbuffered.tval  := dt.tval
      if(p.withDebug) rescheduleUnbuffered.debugEnter := dt.debugEnter
    }

//    assert(!rescheduleUnbuffered.isStall)


    val targetMachine = True

    val readed = Reg(Bits(Global.XLEN bits))

    reschedule.ready := False
    setup.ramWrite.valid := False
    setup.ramWrite.address.assignDontCare()
    setup.ramWrite.data.assignDontCare()
    setup.ramRead.valid := False
    setup.ramRead.address.assignDontCare()


    //Process interrupt request, code and privilege
    val interrupt = new Area {
      val valid = False
      val code = UInt(commit.rescheduleCauseWidth bits).assignDontCare()
      val targetPrivilege = UInt(2 bits).assignDontCare()

      val privilegeAllowInterrupts = mutable.LinkedHashMap[Int, Bool]()
      var privilegs: List[Int] = Nil
      privilegs :+= 3
      privilegeAllowInterrupts += 3 -> (machine.mstatus.mie || !setup.withMachinePrivilege)

      if (p.withSupervisor) {
        privilegs = 1 :: privilegs
        privilegeAllowInterrupts += 1 -> ((supervisor.sstatus.sie && !setup.withMachinePrivilege) || !setup.withSupervisorPrivilege)
      }

      if (p.withUserTrap) {
        privilegs = 0 :: privilegs
        ??? // privilegeAllowInterrupts += 1 -> ((ustatus.UIE && !setup.supervisorPrivilege))
      }

      while (privilegs.nonEmpty) {
        val p = privilegs.head
        when(privilegeAllowInterrupts(p)) {
          for (i <- interruptSpecs
               if i.privilege <= p //EX : Machine timer interrupt can't go into supervisor mode
               if privilegs.tail.forall(e => i.delegators.exists(_.privilege == e))) { // EX : Supervisor timer need to have machine mode delegator
            val delegUpOn = i.delegators.filter(_.privilege > p).map(_.enable).fold(True)(_ && _)
            val delegDownOff = !i.delegators.filter(_.privilege <= p).map(_.enable).orR
            when(i.cond && delegUpOn && delegDownOff) {
              valid := True
              code := i.id
              targetPrivilege := p
            }
          }
        }
        privilegs = privilegs.tail
      }

      if(p.withDebug) {
        when(debug.dcsr.step && debug.dcsr.stepie && !setup.debugMode){
          valid := False
        }

        when(debug.doHalt){
          valid := True
        }
      }
    }

    val decoderInterrupt = new Area{
      val raised = RegInit(False)
      val pendingInterrupt = RegNext(interrupt.valid) init(False)
      val counter = Reg(UInt(3 bits)) init(0) //Implement a little delay to ensure propagation of everthing in the calculation of the interrupt
      val doIt = counter.msb

      when(pendingInterrupt){
        decoder.trapHalt()
        counter := counter + 1
      }
      when(!pendingInterrupt || !decoder.trapReady() || raised){
        counter := 0
      }

      when(doIt && !raised){
        decoder.trapRaise()
        raised := True
      }

      val buffer = new Area{
        val sample          = interrupt.valid && !raised
        val code            = RegNextWhen(interrupt.code, sample)
        val targetPrivilege = RegNextWhen(interrupt.targetPrivilege, sample)
      }
    }


    val exception = new Area{
      val exceptionTargetPrivilegeUncapped = U"11"

      val code = CombInit(reschedule.cause)
      when(reschedule.cause === CSR.MCAUSE_ENUM.ECALL_MACHINE){
        code(1 downto 0) := setup.privilege
      }

      switch(code){
        for(s <- exceptionSpecs){
          is(s.id){
            var exceptionPrivilegs = if (p.withSupervisor) List(1, 3) else List(3)
            while(exceptionPrivilegs.length != 1){
              val p = exceptionPrivilegs.head
              if (exceptionPrivilegs.tail.forall(e => s.delegators.exists(_.privilege == e))) {
                val delegUpOn = s.delegators.filter(_.privilege > p).map(_.enable).fold(True)(_ && _)
                val delegDownOff = !s.delegators.filter(_.privilege <= p).map(_.enable).orR
                when(delegUpOn && delegDownOff) {
                  exceptionTargetPrivilegeUncapped := p
                }
              }
              exceptionPrivilegs = exceptionPrivilegs.tail
            }
          }
        }
      }

      val targetPrivilege = setup.privilege.max(exceptionTargetPrivilegeUncapped)
    }

    def privilegeMux[T <: Data](priv : UInt)(machine : => T, supervisor : => T): T ={
      val ret = CombInit(machine)
      switch(priv) {
        if(p.withSupervisor) is(1) { ret := supervisor }
      }
      ret
    }

    val fsm = new StateMachine{
      val IDLE, SETUP, EPC_WRITE, TVAL_WRITE, EPC_READ, TVEC_READ, XRET, FLUSH_CALC, FLUSH_JUMP, TRAP = new State()
      val DPC_WRITE, DEBUG_ENTER, DPC_READ, RESUME = p.withDebug generate new State()
      setEntry(IDLE)

      val trap = new Area{
        val fire  = False
        val interrupt = Reg(Bool())
        val code      = Reg(UInt(commit.rescheduleCauseWidth bits))
        val targetPrivilege = Reg(UInt(2 bits))
        val debug = p.withDebug generate Reg(Bool())
        val dcause = p.withDebug generate Reg(UInt(3 bits))
        val debugException = p.withDebug generate RegInit(False)
        val ebreak = p.withDebug generate RegInit(False)
        val redo = p.withDebug generate RegInit(False)
      }

      val xret = new Area{
        val sourcePrivilege = reschedule.tval(1 downto 0).asUInt
        val targetPrivilege = privilegeMux(sourcePrivilege)(
          machine.mstatus.mpp,
          U"0" @@ supervisor.sstatus.spp
        )
      }

      IDLE.onEntry{
        decoderInterrupt.raised := False
      }
      IDLE.whenIsActive{
        reschedule.ready := True
        when(rescheduleUnbuffered.valid){
          goto(SETUP)
        }
        if(p.withDebug) when(!debug.running && debug.doResume){
          goto(DPC_READ)
        }
      }
      SETUP.whenIsActive{
        if(p.withDebug) {
          trap.debugException := False
          trap.ebreak := False
          trap.redo := False
        }
        when(!reschedule.fromCommit && decoderInterrupt.raised){
          trap.interrupt := True
          trap.code := decoderInterrupt.buffer.code
          trap.targetPrivilege := decoderInterrupt.buffer.targetPrivilege
          goto(TVEC_READ)

          if(p.withDebug) {
            trap.dcause := 3
            when(debug.doHalt){
              goto(DPC_WRITE)
            }
            when(debug.dcsr.stepLogic.isCause){
              trap.dcause := 4
            }
          }
        } otherwise {
          switch(reschedule.cause){
            is(CAUSE_FLUSH){
              goto(FLUSH_CALC)
            }
            is(CAUSE_REDO){
              setup.redoTriggered := True
              if(p.withDebug) trap.redo := True
              goto(FLUSH_CALC)
            }
            is(CAUSE_XRET){
              goto(EPC_READ)
            }
            default{
              trap.interrupt := False
              trap.code := exception.code
              trap.targetPrivilege := exception.targetPrivilege
              goto(TVEC_READ)

              if(p.withDebug){
                trap.dcause := 1
                when(exception.code === CSR.MCAUSE_ENUM.BREAKPOINT){
                  val doIt = setup.privilege === 3 && debug.dcsr.ebreakm
                  if(p.withUser) doIt setWhen(setup.privilege === 0 && debug.dcsr.ebreaku)
                  if(p.withSupervisor) doIt setWhen(setup.privilege === 1 && debug.dcsr.ebreaks)
                  when(doIt){ goto(DPC_WRITE) }
                }
                when(setup.debugMode){
                  when(exception.code =/= CSR.MCAUSE_ENUM.BREAKPOINT){
                    trap.debugException := True
                  } otherwise {
                    trap.ebreak := True
                  }
                  goto(DPC_WRITE)
                }
              }
            }
          }
          if(p.withDebug) when(reschedule.debugEnter){
            trap.debugException := False
            goto(DPC_WRITE)
          }
        }
      }

      FLUSH_CALC whenIsActive{
        readed(0, PC_WIDTH bits) := B(reschedule.epc + (reschedule.slices +^ 1 << Fetch.SLICE_RANGE_LOW).andMask(reschedule.cause =/= CSR.MCAUSE_ENUM.ECALL_HYPERVISOR))
        goto(FLUSH_JUMP)
      }

      FLUSH_JUMP whenIsActive{
        setup.jump.valid := True
        setup.jump.pc := U(readed).resized
        if(p.withDebug) setup.debugBus.redo := setup.debugMode && trap.redo
        goto(IDLE)
      }

      EPC_READ.whenIsActive{
        setup.ramRead.valid   := True
        setup.ramRead.address := privilegeMux(xret.sourcePrivilege)(
          machine.epc.getAddress(),
          supervisor.epc.getAddress()
        )
        readed := setup.ramRead.data
        when(setup.ramRead.ready){
          goto(XRET)
        }
      }
      TVEC_READ.whenIsActive{
        setup.ramRead.valid   := True
        setup.ramRead.address := privilegeMux(trap.targetPrivilege)(
          machine.tvec.getAddress(),
          supervisor.tvec.getAddress()
        )
        readed := setup.ramRead.data
        when(setup.ramRead.ready){
          goto(TVAL_WRITE)
        }
      }
      TVAL_WRITE.whenIsActive{
        setup.ramWrite.valid   := True
        setup.ramWrite.address := privilegeMux(trap.targetPrivilege)(
          machine.tval.getAddress(),
          supervisor.tval.getAddress()
        )
        setup.ramWrite.data    := S(reschedule.tval).resize(XLEN).asBits
        when(decoderInterrupt.raised){ setup.ramWrite.data    := 0 }
        when(setup.ramWrite.ready){
          goto(EPC_WRITE)
        }
      }
      EPC_WRITE.whenIsActive{
        setup.ramWrite.valid   := True
        setup.ramWrite.address := privilegeMux(trap.targetPrivilege)(
          machine.epc.getAddress(),
          supervisor.epc.getAddress()
        )
        setup.ramWrite.data    := S(reschedule.epc, XLEN bits).asBits //TODO PC sign extends ? (DONE)
        when(setup.ramWrite.ready){
          goto(TRAP)
        }
      }
      if(p.withDebug) {
        DPC_WRITE.whenIsActive{
          setup.ramWrite.valid   := !setup.debugMode
          setup.ramWrite.address := debug.dpc.getAddress()
          setup.ramWrite.data    := S(reschedule.epc, XLEN bits).asBits //TODO PC sign extends ? (DONE)
          when(setup.debugMode || setup.ramWrite.ready){
            goto(DEBUG_ENTER)
          }
        }
        DEBUG_ENTER.whenIsActive{
          debug.running := False
          when(!setup.debugMode) {
            debug.dcsr.cause := trap.dcause
            debug.dcsr.prv := setup.privilege
          } otherwise {
            setup.debugBus.exception := trap.debugException
            setup.debugBus.ebreak    := trap.ebreak
          }
          setup.privilege  := 3
          goto(IDLE)
        }
        DPC_READ.whenIsActive{
          setup.ramRead.valid   := True
          setup.ramRead.address := debug.dpc.getAddress()
          readed := setup.ramRead.data
          when(setup.ramRead.ready){
            goto(RESUME)
          }
        }
        RESUME.whenIsActive{
          setup.jump.valid := True
          setup.jump.pc := U(readed).resized
          setup.privilege := debug.dcsr.prv
          debug.running := True
          debug.bus.resume.rsp.valid := True
          goto(IDLE)
        }
      }
      TRAP.whenIsActive{
        setup.jump.valid := True
        setup.jump.pc := U(readed).resized //TODO mask
        trap.fire := True
        setup.privilege := trap.targetPrivilege
        switch(trap.targetPrivilege){
          is(3){
            machine.mstatus.mie  := False
            machine.mstatus.mpie := machine.mstatus.mie
            machine.mstatus.mpp  := setup.privilege

            machine.cause.interrupt := trap.interrupt
            machine.cause.code      := trap.code
          }
          p.withSupervisor generate is(1){
            supervisor.sstatus.sie  := False
            supervisor.sstatus.spie := supervisor.sstatus.sie
            supervisor.sstatus.spp  := setup.privilege(0, 1 bits)

            supervisor.cause.interrupt := trap.interrupt
            supervisor.cause.code      := trap.code
          }
        }
        setup.trapEvent := True
        goto(IDLE)
      }
      XRET.whenIsActive{
        setup.jump.valid := True
        setup.jump.pc    := U(readed).resized

        setup.privilege  := xret.targetPrivilege
        setup.xretAwayFromMachine setWhen(xret.targetPrivilege < 3)
        switch(reschedule.tval(1 downto 0)){
          is(3){
            machine.mstatus.mpp  := 0
            machine.mstatus.mie  := machine.mstatus.mpie
            machine.mstatus.mpie := True
          }
          p.withSupervisor generate is(1){
            supervisor.sstatus.spp  := U"0"
            supervisor.sstatus.sie  := supervisor.sstatus.spie
            supervisor.sstatus.spie := True
          }
        }

        goto(IDLE)
      }
      fetch.getStage(0).haltIt(!isActive(IDLE) || commit.hasPendingTrapRescheduling())
    }


    val whitebox = new AreaRoot{
      val trap = new Area{
        val fire      = Verilator.public(CombInit(fsm.trap.fire     ))
        val code      = Verilator.public(CombInit(fsm.trap.code     ))
        val interrupt = Verilator.public(CombInit(fsm.trap.interrupt))
        val tval      = Verilator.public(CombInit(reschedule.tval))
      }
    }


    doc.property("SUPERVISOR", p.withSupervisor.toInt)
    doc.property("TVAL_WIDTH", widthOf(reschedule.tval))

    frontend.release()
    fetch.release()
    rob.release()
  }
}
//TODO access privilege checks