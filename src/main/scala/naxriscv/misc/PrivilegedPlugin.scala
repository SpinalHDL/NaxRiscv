package naxriscv.misc

import naxriscv.execute.CsrAccessPlugin
import naxriscv.interfaces.{CommitService, CsrRamFilter, PrivilegedService}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

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
    csr.retain()
    ram.retain()

    val machinePrivilege    = RegInit(True)
    val supervisorPrivilege = implementSupervisor generate RegInit(True)
  }

  val logic = create late new Area{
    val csr = setup.csr
    val ram = setup.ram
    val commit = getService[CommitService]


    val machine = new Area {
      val tvec    = csr.readWriteRam(CSR.MTVEC)
      val epc     = csr.readWriteRam(CSR.MEPC)
      val scratch = csr.readWriteRam(CSR.MSCRATCH)
      val tval    = csr.readWriteRam(CSR.MTVAL)
    }
    val supervisor = p.withSupervisor generate new Area {
      val tvec    = csr.readWriteRam(CSR.STVEC)
      val epc     = csr.readWriteRam(CSR.SEPC)
      val scratch = csr.readWriteRam(CSR.SSCRATCH)
      val tval    = csr.readWriteRam(CSR.STVAL)
    }
    val userTrap = p.withUserTrap generate new Area {
      val tvec    = csr.readWriteRam(CSR.UTVEC)
      val epc     = csr.readWriteRam(CSR.UEPC)
      val scratch = csr.readWriteRam(CSR.USCRATCH)
      val tval    = csr.readWriteRam(CSR.UTVAL)
    }


    commit.reschedulingPort()






//    val scratch = ram.ramAllocate(2)
//    val scratchMapping = CsrRamFilter(List(CSR.MSCRATCH, CSR.SSCRATCH))
//    csr.readWrite(scratch, scratchMapping)

//    val mscratch = ram.ramAllocate(1)
//    csr.readWrite(mscratch, CSR.MSCRATCH)
//
//
//    val sscratch = ram.ramAllocate(1)
//    csr.readWrite(sscratch, CSR.SSCRATCH)


//    val mscratch = Reg(Bits(32 bits))
//    csr.readWrite(mscratch, CSR.MSCRATCH)
//
//
//    val sscratch = Reg(Bits(32 bits))
//    csr.readWrite(sscratch, CSR.SSCRATCH)


    csr.release()
    ram.release()
  }
}
