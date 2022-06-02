package naxriscv.execute.fpu

import naxriscv.{Global, ROB}
import naxriscv.Global._
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


    val core = FpuCore(FpuParameter(
      rvd        = RVD,
      rv64       = XLEN == 64,
      robIdWidth = ROB.ID_WIDTH,
      portCount  = 1
    ))

    val port = core.io.ports(0)
    port.floatCmd << floatCmd
    port.floatCompletion >> floatCompletion
    port.unschedule := getService[FpuWriteback].setup.unschedule

//    val result = floatCmd.stage().stage().stage().toFlow
//    floatCompletion.valid := result.valid
//    floatCompletion.flags := floatCompletion.flags.getZero
//    floatCompletion.robId := result.robId
//    floatCompletion.value := result.rs(0)
//    floatCompletion.value(52, 11 bits) := B(U(result.rs(0)(52, 11 bits)) + 1)
  }
}
