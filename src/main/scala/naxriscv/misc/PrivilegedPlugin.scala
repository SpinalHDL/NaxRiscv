package naxriscv.misc

import naxriscv.execute.CsrAccessPlugin
import naxriscv.interfaces.CsrRamFilter
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

class PrivilegedPlugin extends Plugin{
  val setup = create early new Area{
    val csr = getService[CsrAccessPlugin]
    val ram = getService[CsrRamPlugin]
    csr.retain()
    ram.retain()
  }

  val logic = create late new Area{
    val csr = setup.csr
    val ram = setup.ram
//    val scratch = ram.ramAllocate(2)
//    val scratchMapping = CsrRamFilter(List(CSR.MSCRATCH, CSR.SSCRATCH))
//    csr.readWrite(scratch, scratchMapping)

    val mscratch = ram.ramAllocate(1)
    csr.readWrite(mscratch, CSR.MSCRATCH)


    val sscratch = ram.ramAllocate(1)
    csr.readWrite(sscratch, CSR.SSCRATCH)


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
