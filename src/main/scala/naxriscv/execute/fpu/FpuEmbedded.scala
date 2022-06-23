package naxriscv.execute.fpu

import naxriscv.{Global, ROB}
import naxriscv.Global._
import naxriscv.interfaces.{WakeRegFile, WakeRegFileService, WakeRob, WakeRobService}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin

class FpuEmbedded extends Plugin {
  val logic = create late new Area{
    val floatCmd = getService[FpuFloatExecute].setup.floatCmd.setAsDirectionLess
    val intCmd = getService[FpuIntegerExecute].setup.intCmd.setAsDirectionLess
    val wb = getService[FpuWriteback]
    val floatCompletion = wb.setup.floatCompletion.setAsDirectionLess
    val integerWriteback = wb.setup.integerWriteback.setAsDirectionLess

    val core = FpuCore(FpuParameter(
      rvd        = RVD,
      rv64       = XLEN.get == 64,
      robIdWidth = ROB.ID_WIDTH,
      portCount  = 1,
      withAdd = true,
      withMul = true
    ))

    val port = core.io.ports(0)
    port.floatCmd << floatCmd
    port.intCmd << intCmd
    port.floatWriteback >> floatCompletion
    port.intWriteback >> integerWriteback
    port.unschedule := getService[FpuWriteback].setup.unschedule
  }
}
