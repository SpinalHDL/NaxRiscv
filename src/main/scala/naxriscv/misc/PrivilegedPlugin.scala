package naxriscv.misc

import naxriscv.Global
import naxriscv.Global._
import naxriscv.execute.CsrAccessPlugin
import naxriscv.fetch.{FetchPlugin, PcPlugin}
import naxriscv.interfaces.JumpService.Priorities
import naxriscv.interfaces.{CommitService, CsrRamFilter, PrivilegedService}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.fsm._

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
    val rob = getService[RobPlugin]
    csr.retain()
    ram.retain()
    fetch.retain()
    rob.retain()

    val jump = getService[PcPlugin].createJumpInterface(Priorities.COMMIT_TRAP)
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
    val commit = getService[CommitService]


    val machine = new Area {
      val tvec    = csr.readWriteRam(CSR.MTVEC)
      val tval    = csr.readWriteRam(CSR.MTVAL)
      val epc     = csr.readWriteRam(CSR.MEPC)
      val scratch = csr.readWriteRam(CSR.MSCRATCH)
      val cause = new Area{
        val interrupt = RegInit(False)
        val code = Reg(UInt(4 bits)) init(0)

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


    val rescheduleUnbuffered = commit.reschedulingPort().toStream
    val reschedule = rescheduleUnbuffered.stage()
    val trapPc = rob.readAsyncSingle(Global.PC, reschedule.robId)
    val trapPcReg = RegNext(trapPc)


    val targetMachine = True

    val tvec = Reg(Bits(Global.XLEN bits))
//    val epc = Reg(Bits(Global.XLEN bits))

    reschedule.ready := False
    setup.ramWrite.valid := False
    setup.ramWrite.address.assignDontCare()
    setup.ramWrite.data.assignDontCare()
    setup.ramRead.valid := False
    setup.ramRead.address.assignDontCare()
    setup.jump.valid := False
    setup.jump.pc    := U(tvec)
    val fsm = new StateMachine{
      val IDLE, SETUP, EPC_WRITE, TVAL_WRITE, EPC_READ, TVEC_READ  = new State()
      setEntry(IDLE)

      IDLE.whenIsActive{
        when(rescheduleUnbuffered.valid && rescheduleUnbuffered.trap){
          goto(SETUP)
        }
      }
      SETUP.whenIsActive{
        goto(TVEC_READ)
      }
      TVEC_READ.whenIsActive{
        setup.ramRead.valid   := True
        setup.ramRead.address := machine.tvec.getAddress()
        tvec := setup.ramRead.data
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
        setup.ramWrite.data    := B(trapPcReg)
        when(setup.ramWrite.ready){
          setup.jump.valid := True
          machine.cause.interrupt := False
          machine.cause.code      := reschedule.cause
          goto(IDLE)
        }
      }
      fetch.getStage(0).haltIt(rescheduleUnbuffered.valid && rescheduleUnbuffered.trap || !isActive(IDLE))
    }


    fetch.release()
    rob.release()
  }
}
