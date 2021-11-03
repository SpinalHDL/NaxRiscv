package naxriscv.units

import naxriscv.interfaces.Riscv
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

class IntAluPlugin(fuId : Any) extends Plugin{
  val setup = create early new Area{
    val fu = getService[ExecuteUnit](fuId)
    fu.addFunction(Riscv.integer.ADD)
    fu.addFunction(Riscv.integer.ADDI)
  }

  val logic = create late new Area{
/*
      val bitwise = input(ALU_BITWISE_CTRL).mux(
        AluBitwiseCtrlEnum.AND  -> (input(SRC1) & input(SRC2)),
        AluBitwiseCtrlEnum.OR   -> (input(SRC1) | input(SRC2)),
        AluBitwiseCtrlEnum.XOR  -> (input(SRC1) ^ input(SRC2))
      )

      // mux results
      insert(REGFILE_WRITE_DATA) := input(ALU_CTRL).mux(
        AluCtrlEnum.BITWISE  -> bitwise,
        AluCtrlEnum.SLT_SLTU -> input(SRC_LESS).asBits(32 bit),
        AluCtrlEnum.ADD_SUB  -> input(SRC_ADD_SUB)
      )
 */
  }
}
