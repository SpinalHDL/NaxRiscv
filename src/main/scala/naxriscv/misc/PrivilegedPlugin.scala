package naxriscv.misc

import naxriscv.{Fetch, Frontend, Global}
import naxriscv.Global._
import naxriscv.debug.{DebugDmToHartOp, DebugHartBus, DebugModule}
import naxriscv.execute.EnvCallPlugin.{CAUSE_FLUSH, CAUSE_REDO, CAUSE_XRET}
import naxriscv.execute.{CsrAccessPlugin, EnvCallPlugin}
import naxriscv.fetch.{FetchCachePlugin, FetchPlugin, PcPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.JumpService.Priorities
import naxriscv.interfaces.{CommitService, CsrListFilter, CsrRamService, DecoderService, PrivilegedService}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
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
    archId         = 0,
    impId          = 0,
    hartId         = 0,
    debugVector = null
  )
}

case class PrivilegedConfig(withSupervisor : Boolean,
                            withUser: Boolean,
                            withUserTrap: Boolean,
                            withRdTime : Boolean,
                            withDebug: Boolean,
                            debugVector : SizeMapping,
                            vendorId: Int,
                            archId: Int,
                            impId: Int,
                            hartId: Int) {

}



class PrivilegedPlugin(var p : PrivilegedConfig) extends Plugin with PrivilegedService{
  override def hasMachinePriv = setup.withMachinePrivilege
  override def hasSupervisorPriv = setup.withSupervisorPrivilege
  override def getPrivilege() = setup.privilege
  override def xretAwayFromMachine = setup.xretAwayFromMachine

  override def implementSupervisor = p.withSupervisor
  override def implementUserTrap = p.withUserTrap

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
        val timer = in Bool()
        val software = in Bool()
        val external = in Bool()
      }
      val supervisor = p.withSupervisor generate new Area{
        val external = in Bool()
      }
      val user = p.withUserTrap generate new Area{
        val external = in Bool()
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

    val debugMode = p.withDebug generate RegInit(False)
    val privilege = RegInit(U"11")
    val withMachinePrivilege    = privilege >= U"11"
    val withSupervisorPrivilege = privilege >= U"01"
    val xretAwayFromMachine = False

    addMisa('I')
    if(RVC) addMisa('C')
    if(p.withUser) addMisa('U')
    if(p.withSupervisor) addMisa('S')

    val debugBus = p.withDebug generate master(DebugHartBus())
    val fetchBypass = p.withDebug generate getService[FetchCachePlugin].createBypass()
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

      val halted = RegInit(False)
      val running = RegInit(True)

      fetch.getStage(0).haltIt(!running)

      bus.hartToDm.setIdle()
      bus.halt.served := False
      bus.resume.served := False

      bus.halted := halted
      bus.running := running
      bus.unavailable := False

      val doHalt = bus.halt.isPending()
      val doResume = bus.resume.isPending()

      val fetchBypass = new Area{
        val words = p.debugVector.size.toInt/4
        val banksCount = widthOf(setup.fetchBypass.data)/32
        val wordsPerBank = words / banksCount
        val banks = List.fill(banksCount)(Mem.fill(wordsPerBank)(Bits(32 bits)))

        val write = new Area{
          for((bank, id) <- banks.zipWithIndex){
            bank.write(
              address = (bus.dmToHart.address >> log2Up(banksCount)).resized,
              data = bus.dmToHart.data,
              enable = bus.dmToHart.valid && bus.dmToHart.op === DebugDmToHartOp.INSTRUCTION && bus.dmToHart.address(0, log2Up(banksCount) bits) === id
            )
          }
        }

        val stage = fetch.getStage(1)
        val read = new Area{
          val address = stage(Fetch.FETCH_PC)
          val hit = setup.debugMode && p.debugVector.hit(address)
          val shift = log2Up(widthOf(setup.fetchBypass.data)/8)
          val values = banks.map(_.readAsync((address >> shift).resized))
          stage(setup.fetchBypass.valid) := hit
          stage(setup.fetchBypass.data) := B(values)
        }
      }

      val execute = new Area{
        val start = bus.dmToHart.valid && bus.dmToHart.op === DebugDmToHartOp.EXECUTE
        when(start) {
          setup.jump.valid := True
          setup.jump.pc := U(p.debugVector.base, PC_WIDTH bits) + (bus.dmToHart.address << 2)
          setup.privilege  := 3
          setup.debugMode  := True
          halted := False
          running := True
        }
      }

      val dataFilter = CsrListFilter(DebugModule.CSR_DATA to DebugModule.CSR_DATA + 11)
      val dataRead = new Area{
        val address = csr.onWriteAddress-DebugModule.CSR_DATA
        csr.onWrite(dataFilter, onlyOnFire = false){
          bus.hartToDm.valid := True
          bus.hartToDm.address := address.resized
          bus.hartToDm.data  := csr.onWriteBits.subdivideIn(32 bits).read((address).resized)
        }
      }

      val dataWrite = new Area{
        val words = 12
        val banksCount = XLEN.get/32
        val wordsPerBank = words / banksCount
        val banks = List.fill(banksCount)(Mem.fill(banksCount)(Bits(32 bits)))

        val write = new Area{
          for((bank, id) <- banks.zipWithIndex){
            bank.write(
              address = (bus.dmToHart.address >> log2Up(banksCount)).resized,
              data = bus.dmToHart.data,
              enable = bus.dmToHart.valid && bus.dmToHart.op === DebugDmToHartOp.DATA && bus.dmToHart.address(0, log2Up(banksCount) bits) === id
            )
          }
        }

        val read = new Area{
          val address = csr.onReadAddress-DebugModule.CSR_DATA
          val values = B(banks.map(_.readAsync((address >> log2Up(XLEN.get/32)).resized)))
          csr.read(values, dataFilter)
        }
      }

      val dpc = csr.readWriteRam(CSR.DPC)
      val dcsr = new Area{
        val prv = RegInit(U"11")
        val step = RegInit(False)
        val nmip = False
        val mprven = False
        val cause = RegInit(U"000")
        val stoptime = False
        val stopcount = False
        val stepie = RegInit(False)
        val ebreaku = p.withUser generate RegInit(False)
        val ebreaks = p.withSupervisor generate RegInit(False)
        val ebreakm = RegInit(False)
        val xdebugver = U(4, 4 bits)


        csr.read(CSR.DCSR, 3 -> nmip, 6 -> cause, 28 -> xdebugver)
        csr.readWrite(CSR.DCSR, 0 -> prv, 2 -> step, 4 -> mprven, 9 -> stoptime, 10 -> stopcount, 11 -> stepie, 15 -> ebreakm)
        if(p.withSupervisor) csr.readWrite(CSR.DCSR, 13 -> ebreaks)
        if(p.withUser)       csr.readWrite(CSR.DCSR, 12 -> ebreaku)

        when(csr.onDecodeRead || csr.onDecodeWrite){
          when(!setup.debugMode && csr.onDecodeAddress >> 4 === 0x7B){
            csr.onDecodeTrap()
          }
        }
      }

    }

    val machine = new Area {
      val cause = new Area{
        val interrupt = RegInit(False)
        val code = Reg(UInt(commit.rescheduleCauseWidth bits)) init(0)
      }
      val mstatus = new Area{
        val mie, mpie = RegInit(False)
        val mpp = RegInit(U"00")
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
      csr.read     (CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip, 3 -> mip.msip)
      csr.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)

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
    })
    val reschedule = rescheduleUnbuffered.stage()

    val cr = commit.reschedulingPort()
    rescheduleUnbuffered.valid := cr.valid && cr.trap
    rescheduleUnbuffered.cause := cr.cause
    rescheduleUnbuffered.epc   := rob.readAsyncSingle(Global.PC, cr.robId)
    rescheduleUnbuffered.slices := rob.readAsyncSingle(Fetch.INSTRUCTION_SLICE_COUNT, cr.robId)
    rescheduleUnbuffered.tval  := cr.tval

    val dt = decoder.getTrap()
    when(dt.valid) {
      rescheduleUnbuffered.valid := True
      rescheduleUnbuffered.cause := dt.cause
      rescheduleUnbuffered.epc   := dt.epc
      rescheduleUnbuffered.tval  := dt.tval
    }

    assert(!rescheduleUnbuffered.isStall)


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

      if(p.withDebug) when(debug.doHalt){
        valid := True
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
        if(p.withDebug) when(debug.halted && debug.doResume){
          goto(DPC_READ)
        }
      }
      SETUP.whenIsActive{
        when(decoderInterrupt.raised){
          trap.interrupt := True
          trap.code := decoderInterrupt.buffer.code
          trap.targetPrivilege := decoderInterrupt.buffer.targetPrivilege
          goto(TVEC_READ)

          if(p.withDebug) {
            trap.dcause := 3
            when(debug.doHalt){
              goto(DPC_WRITE)
            }
          }
        } otherwise {
          switch(reschedule.cause){
            is(CAUSE_FLUSH){
              goto(FLUSH_CALC)
            }
            is(CAUSE_REDO){
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
                if(p.withUser)       when(debug.dcsr.ebreaku && exception.code === CSR.MCAUSE_ENUM.ECALL_USER){ goto(DPC_WRITE) }
                if(p.withSupervisor) when(debug.dcsr.ebreaks && exception.code === CSR.MCAUSE_ENUM.ECALL_SUPERVISOR){ goto(DPC_WRITE) }
                when(debug.dcsr.ebreakm && exception.code === CSR.MCAUSE_ENUM.ECALL_MACHINE){ goto(DPC_WRITE) }
                when(setup.debugMode){ goto(DPC_WRITE) }
              }
            }
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
          setup.ramWrite.valid   := True
          setup.ramWrite.address := debug.dpc.getAddress()
          setup.ramWrite.data    := S(reschedule.epc, XLEN bits).asBits //TODO PC sign extends ? (DONE)
          debug.running := False
          when(setup.ramWrite.ready){
            goto(DEBUG_ENTER)
          }
        }
        DEBUG_ENTER.whenIsActive{
//          setup.jump.valid := True
//          setup.jump.pc    := p.debugVector
          when(!setup.debugMode) {
            debug.dcsr.cause := trap.dcause
            debug.dcsr.prv := setup.privilege
          }
          setup.privilege  := 3
          setup.debugMode  := True
          debug.halted := True
          debug.bus.halt.served := True
          goto(IDLE)
        }
        DPC_READ.whenIsActive{
          setup.ramRead.valid   := True
          setup.ramRead.address := debug.dpc.getAddress()
          readed := setup.ramRead.data
          debug.halted := False
          when(setup.ramRead.ready){
            goto(RESUME)
          }
        }
        RESUME.whenIsActive{
          setup.jump.valid := True
          setup.jump.pc := U(readed).resized
          setup.debugMode := False
          setup.privilege := debug.dcsr.prv
          debug.running := True
          debug.bus.resume.served := True
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
        goto(IDLE)
      }
      XRET.whenIsActive{
        setup.jump.valid := True
        setup.jump.pc    := U(readed).resized

        setup.privilege  := xret.targetPrivilege
        setup.xretAwayFromMachine clearWhen(xret.targetPrivilege < 3)
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
      fetch.getStage(0).haltIt(rescheduleUnbuffered.valid || !isActive(IDLE))
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

    frontend.release()
    fetch.release()
    rob.release()
  }
}
//TODO access privilege checks