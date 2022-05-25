package naxriscv.execute.fpu

import naxriscv.ROB
import naxriscv.interfaces.{WakeRegFile, WakeRegFileService, WakeRob, WakeRobService}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

class FpuEmbedded extends Plugin {

  val setup = create early new Area{

  }

  val logic = create late new Area{
    val floatCmd = getService[FpuExecute].setup.floatCmd.setAsDirectionLess
    val floatCompletion = getService[FpuWriteback].setup.floatCompletion.setAsDirectionLess

    val result = floatCmd.stage().stage().stage().toFlow
    floatCompletion.valid := result.valid
    floatCompletion.flags := floatCompletion.flags.getZero
    floatCompletion.robId := result.robId
    floatCompletion.value := result.rs(0)
    floatCompletion.value(52, 11 bits) := B(U(result.rs(0)(52, 11 bits)) + 1)
  }
}
