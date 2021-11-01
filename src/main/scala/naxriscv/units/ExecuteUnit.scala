package naxriscv.units

import naxriscv.Frontend
import naxriscv.interfaces._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

class ExecuteUnit(euId : String) extends Plugin with ExecuteUnitService with WakeService {

  override def uniqueIds = List(euId)

  override def hasFixedLatency = ???

  override def getFixedLatency = ???

  override def pushPort() = logic.pushPort

  override def euName() = euId

  override def wakeRobs = Seq(logic.wakePort)

  override def addFunction(enc: Encoding) = {
    val decoder = getService[DecoderService]
    decoder.addFunction(this, enc)
  }

  val setup = create early new Area{
    val rob = getService[RobService]
    val completion = rob.robCompletion()
  }

  val logic = create late new Area{
    val pushPort = Stream(ExecutionUnitPush())
    pushPort.ready := True

    val wakePort = Flow(Frontend.ROB_ID)
    wakePort.valid := False
    wakePort.payload := 0

    setup.completion.valid := False
    setup.completion.id := 0
  }
}
