package naxriscv.misc

import naxriscv.{Frontend, Global}
import naxriscv.Global._
import naxriscv.execute.{CsrAccessPlugin, EnvCallPlugin}
import naxriscv.fetch.{FetchPlugin, PcPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.JumpService.Priorities
import naxriscv.interfaces.{CommitService, CsrRamFilter, DecoderService, PrivilegedService}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.fsm._
import spinal.lib.pipeline.StageableOffset

object PrivilegedConfig{
  def full = PrivilegedConfig(
    withSupervisor = true,
    withUser       = true,
    withUserTrap   = true
  )
}

case class PrivilegedConfig(withSupervisor : Boolean,
                            withUser : Boolean,
                            withUserTrap : Boolean){

}



class PrivilegedPlugin(p : PrivilegedConfig) extends Plugin with PrivilegedService{
  override def hasMachinePriv = setup.machinePrivilege
  override def hasSupervisorPriv = setup.supervisorPrivilege

  override def implementSupervisor = p.withSupervisor
  override def implementUserTrap = p.withUserTrap

  val setup = create early new Area{
    val csr = getService[CsrAccessPlugin]
    val ram = getService[CsrRamPlugin]
    val fetch = getService[FetchPlugin]
    val frontend = getService[FrontendPlugin]
    val rob = getService[RobPlugin]
    csr.retain()
    ram.retain()
    fetch.retain()
    rob.retain()
    frontend.retain()

    val jump = getService[PcPlugin].createJumpInterface(Priorities.COMMIT_TRAP)
//    val interrupt = getService[CommitService].newSchedulePort(canTrap = true, canJump = false)
    val ramRead  = ram.ramReadPort()
    val ramWrite = ram.ramWritePort()

    val machinePrivilege    = RegInit(True)
    val supervisorPrivilege = implementSupervisor generate RegInit(True)
  }

  val logic = create late new Area{
    val csr = setup.csr
    val ram = setup.ram
    val fetch = setup.fetch
    val rob = setup.rob
    val frontend = setup.frontend
    val commit = getService[CommitService]


    val machine = new Area {
      val tvec    = csr.readWriteRam(CSR.MTVEC)
      val tval    = csr.readWriteRam(CSR.MTVAL)
      val epc     = csr.readWriteRam(CSR.MEPC)
      val scratch = csr.readWriteRam(CSR.MSCRATCH)
      val cause = new Area{
        val interrupt = RegInit(False)
        val code = Reg(UInt(commit.rescheduleCauseWidth bits)) init(0)

        csr.readWrite(CSR.MCAUSE, XLEN-1 -> interrupt, 0 -> code)
      }
    }
    val supervisor = p.withSupervisor generate new Area {
      val tvec    = csr.readWriteRam(CSR.STVEC)
      val tval    = csr.readWriteRam(CSR.STVAL)
      val epc     = csr.readWriteRam(CSR.SEPC)
      val scratch = csr.readWriteRam(CSR.SSCRATCH)
    }
    val userTrap = p.withUserTrap generate new Area {
      val tvec    = csr.readWriteRam(CSR.UTVEC)
      val tval    = csr.readWriteRam(CSR.UTVAL)
      val epc     = csr.readWriteRam(CSR.UEPC)
      val scratch = csr.readWriteRam(CSR.USCRATCH)
    }


    csr.release()
    ram.release()


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

    val de = getService[DecoderService].getException()
    when(de.valid) {
      rescheduleUnbuffered.valid := True
      rescheduleUnbuffered.cause := de.cause
      rescheduleUnbuffered.epc   := de.epc
      rescheduleUnbuffered.tval  := de.tval
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

    val pendingInterrupt = False

    val fsm = new StateMachine{
      val IDLE, SETUP, EPC_WRITE, TVAL_WRITE, EPC_READ, TVEC_READ, XRET = new State()
      setEntry(IDLE)

      val cause = Reg(UInt(commit.rescheduleCauseWidth bits))
      val interrupt = Reg(Bool())

      IDLE.whenIsActive{
        reschedule.ready := True
        when(rescheduleUnbuffered.valid){
          goto(SETUP)
        }
      }
      SETUP.whenIsActive{
        when(reschedule.cause === EnvCallPlugin.CAUSE_XRET) {
          goto(EPC_READ)
        } otherwise{
          goto(TVEC_READ)
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
          machine.cause.interrupt := False
          machine.cause.code      := reschedule.cause //TODO
          goto(IDLE)
        }
      }
      XRET.whenIsActive{
        setup.jump.valid := True
        setup.jump.pc    := U(readed)
        goto(IDLE)
      }
      fetch.getStage(0).haltIt(rescheduleUnbuffered.valid || !isActive(IDLE))
    }


    frontend.release()
    fetch.release()
    rob.release()
  }
}
//TODO access privilege checks