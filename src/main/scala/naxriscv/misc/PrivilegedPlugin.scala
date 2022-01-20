package naxriscv.misc

import naxriscv.{Frontend, Global}
import naxriscv.Global._
import naxriscv.execute.EnvCallPlugin.CAUSE_XRET
import naxriscv.execute.{CsrAccessPlugin, EnvCallPlugin}
import naxriscv.fetch.{FetchPlugin, PcPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.JumpService.Priorities
import naxriscv.interfaces.{CommitService, CsrListFilter, DecoderService, PrivilegedService}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.fsm._
import spinal.lib.pipeline.StageableOffset

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PrivilegedConfig{
  def full = PrivilegedConfig(
    withSupervisor = false,
    withUser       = false,
    withUserTrap   = false,
    vendorId       = 0,
    archId         = 0,
    impId          = 0,
    hartId         = 0
  )
}

case class PrivilegedConfig(withSupervisor : Boolean,
                            withUser: Boolean,
                            withUserTrap: Boolean,
                            vendorId: Int,
                            archId: Int,
                            impId: Int,
                            hartId: Int) {

}



class PrivilegedPlugin(p : PrivilegedConfig) extends Plugin with PrivilegedService{
  override def hasMachinePriv = setup.withMachinePrivilege
  override def hasSupervisorPriv = setup.withSupervisorPrivilege
  override def getPrivilege() = setup.privilege

  override def implementSupervisor = p.withSupervisor
  override def implementUserTrap = p.withUserTrap

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

    val jump = getService[PcPlugin].createJumpInterface(Priorities.COMMIT_TRAP)
    val ramRead  = ram.ramReadPort()
    val ramWrite = ram.ramWritePort()

    val privilege = RegInit(U"11")
    val withMachinePrivilege    = privilege >= U"11"
    val withSupervisorPrivilege = privilege >= U"01"
  }

  val logic = create late new Area{
    val csr = setup.csr
    val ram = setup.ram
    val fetch = setup.fetch
    val rob = setup.rob
    val frontend = setup.frontend
    val decoder = getService[DecoderService]
    val commit = getService[CommitService]


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

      val medeleg = new Area {
        val iam, iaf, ii, lam, laf, sam, saf, eu, es, ipf, lpf, spf = RegInit(False)
        val mapping = mutable.LinkedHashMap(0 -> iam, 1 -> iaf, 2 -> ii, 4 -> lam, 5 -> laf, 6 -> sam, 7 -> saf, 8 -> eu, 9 -> es, 12 -> ipf, 13 -> lpf, 15 -> spf)
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
//      csr.read(CSR.MISA     , p.vendorId) // MRW ISA and extensions

      csr.readWrite(CSR.MCAUSE, XLEN-1 -> cause.interrupt, 0 -> cause.code)
      csr.readWrite(CSR.MSTATUS, 11 -> mstatus.mpp, 7 -> mstatus.mpie, 3 -> mstatus.mie)
      csr.read     (CSR.MIP, 11 -> mip.meip, 7 -> mip.mtip)
      csr.readWrite(CSR.MIP, 3 -> mip.msip)
      csr.readWrite(CSR.MIE, 11 -> mie.meie, 7 -> mie.mtie, 3 -> mie.msie)

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
      for(offset <- List(CSR.MIP, CSR.SIP)){
        csr.readWrite(offset, 5 -> sip.stip, 1 -> sip.ssip)
        csr.read(offset,  9 -> sip.seipOr)
        csr.write(offset,  9 -> sip.seipSoft)
        csr.readToWrite(sip.seipSoft, offset, 9)
      }
      csr.readWrite(CSR.SIP, 3 -> sip.ssip)
      csr.readWrite(CSR.SIE, 11 -> sie.seie, 7 -> sie.stie, 3 -> sie.ssie)

      addInterrupt(sip.stip && sie.stie,    id = 5, privilege = 1, delegators = List(Delegator(machine.mideleg.st, 3)))
      addInterrupt(sip.ssip && sie.ssie,    id = 1, privilege = 1, delegators = List(Delegator(machine.mideleg.ss, 3)))
      addInterrupt(sip.seipOr && sie.seie,  id = 9, privilege = 1, delegators = List(Delegator(machine.mideleg.se, 3)))

      for((id, enable) <- machine.medeleg.mapping) exceptionSpecs += ExceptionSpec(id, List(Delegator(enable, 3)))
    }

    val userTrap = p.withUserTrap generate new Area {
      val tvec    = csr.readWriteRam(CSR.UTVEC)
      val tval    = csr.readWriteRam(CSR.UTVAL)
      val epc     = csr.readWriteRam(CSR.UEPC)
      val scratch = csr.readWriteRam(CSR.USCRATCH)
    }

    csr.release()
    ram.allocationLock.release()



    val rescheduleUnbuffered = Stream(new Bundle{
      val cause      = UInt(commit.rescheduleCauseWidth bits)
      val epc        = PC()
      val tval       = Bits(Global.XLEN bits)
    })
    val reschedule = rescheduleUnbuffered.stage()

    val cr = commit.reschedulingPort()
    rescheduleUnbuffered.valid := cr.valid && cr.trap
    rescheduleUnbuffered.cause := cr.cause
    rescheduleUnbuffered.epc   := rob.readAsyncSingle(Global.PC, cr.robId)
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
    setup.jump.valid := False
    setup.jump.pc.assignDontCare()


    //Process interrupt request, code and privilege
    val interrupt = new Area {
      val valid = False
      val code = UInt(commit.rescheduleCauseWidth bits).assignDontCare()
      val targetPrivilege = UInt(2 bits).assignDontCare()

      val privilegeAllowInterrupts = mutable.LinkedHashMap[Int, Bool]()
      var privilegs: List[Int] = Nil

      privilegs = List(3)
      privilegeAllowInterrupts += 3 -> (machine.mstatus.mie || !setup.withMachinePrivilege)

      if (p.withSupervisor) {
        privilegs = 1 :: privilegs
        ??? // privilegeAllowInterrupts += 1 -> ((sstatus.SIE && !setup.machinePrivilege) || !setup.supervisorPrivilege)
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


    val fsm = new StateMachine{
      val IDLE, SETUP, EPC_WRITE, TVAL_WRITE, EPC_READ, TVEC_READ, XRET = new State()
      setEntry(IDLE)

      val trap = new Area{
        val fire  = False
        val interrupt = Reg(Bool())
        val code      = Reg(UInt(commit.rescheduleCauseWidth bits))
        val targetPrivilege = Reg(UInt(2 bits))
      }

      IDLE.onEntry{
        decoderInterrupt.raised := False
      }
      IDLE.whenIsActive{
        reschedule.ready := True
        when(rescheduleUnbuffered.valid){
          goto(SETUP)
        }
      }
      SETUP.whenIsActive{
        when(decoderInterrupt.raised){
          trap.interrupt := True
          trap.code := decoderInterrupt.buffer.code
          trap.targetPrivilege := decoderInterrupt.buffer.targetPrivilege
          goto(TVEC_READ)
        } otherwise {
          when(reschedule.cause === CAUSE_XRET) {
            goto(EPC_READ)
          } otherwise {
            trap.interrupt := False
            trap.code := exception.code
            trap.targetPrivilege := exception.targetPrivilege
            goto(TVEC_READ)
          }
        }
      }
      EPC_READ.whenIsActive{
        setup.ramRead.valid   := True
        setup.ramRead.address := machine.epc.getAddress()
        readed := setup.ramRead.data
        when(setup.ramRead.ready){
          goto(XRET)
        }
      }
      TVEC_READ.whenIsActive{
        setup.ramRead.valid   := True
        setup.ramRead.address := machine.tvec.getAddress()
        readed := setup.ramRead.data
        when(setup.ramRead.ready){
          goto(TVAL_WRITE)
        }
      }
      TVAL_WRITE.whenIsActive{
        setup.ramWrite.valid   := True
        setup.ramWrite.address := machine.tval.getAddress()
        setup.ramWrite.data    := reschedule.tval
        when(decoderInterrupt.raised){ setup.ramWrite.data    := 0 }
        when(setup.ramWrite.ready){
          goto(EPC_WRITE)
        }
      }
      EPC_WRITE.whenIsActive{
        setup.ramWrite.valid   := True
        setup.ramWrite.address := machine.epc.getAddress()
        setup.ramWrite.data    := B(reschedule.epc)
        setup.jump.pc := U(readed) //TODO mask
        when(setup.ramWrite.ready){
          setup.jump.valid := True
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
              supervisor.sstatus.spie := machine.mstatus.mie
              supervisor.sstatus.spp  := setup.privilege(0, 1 bits)

              supervisor.cause.interrupt := trap.interrupt
              supervisor.cause.code      := trap.code
            }
          }
          goto(IDLE)
        }
      }
      XRET.whenIsActive{
        setup.jump.valid := True
        setup.jump.pc    := U(readed)

        switch(reschedule.tval(1 downto 0)){
          is(3){
            machine.mstatus.mpp := 0
            machine.mstatus.mie := machine.mstatus.mpie
            machine.mstatus.mpie := True
            setup.privilege := machine.mstatus.mpp
          }
          p.withSupervisor generate is(1){
            supervisor.sstatus.spp  := U"0"
            supervisor.sstatus.sie  := supervisor.sstatus.spie
            supervisor.sstatus.spie := True
            setup.privilege         := U"0" @@ supervisor.sstatus.spp
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
      }
    }

    frontend.release()
    fetch.release()
    rob.release()
  }
}
//TODO access privilege checks