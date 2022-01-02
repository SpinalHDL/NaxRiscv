package naxriscv.misc

import naxriscv.execute.CsrAccessPlugin
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

class PrivilegedPlugin extends Plugin{
  val setup = create early new Area{
    val csr = getService[CsrAccessPlugin]
    csr.retain()
  }

  val logic = create late new Area{
    val csr = setup.csr

    val mscratch = Reg(Bits(32 bits))
    csr.readWrite(mscratch, CSR.MSCRATCH)


    val sscratch = Reg(Bits(32 bits))
    csr.readWrite(sscratch, CSR.SSCRATCH)


    csr.release()
  }
}
