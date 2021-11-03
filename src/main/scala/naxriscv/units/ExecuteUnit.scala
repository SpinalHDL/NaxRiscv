package naxriscv.units

import naxriscv.{Frontend, ROB}
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

    val rf = getService[RegfileService]
    val rfReadRs1 = rf.newRead(withReady = false)
    val rfReadRs2 = rf.newRead(withReady = false)
    val rfWriteRd = rf.newWrite(withReady = false)
  }

  val logic = create late new Area{
    val rob = getService[RobService]
    val decoder = getService[DecoderService]

    val pushPort = Stream(ExecutionUnitPush())
    val s0 = pushPort.toFlow.stage()

    case class Result() extends Bundle{
      val robId = ROB.ID_TYPE()
      val rd = UInt(log2Up(setup.rf.getPhysicalDepth) bits)
      val value = Bits(32 bits)
      val writeRd = Bool()
    }

    val instruction = rob.readAsyncSingle(Frontend.INSTRUCTION_DECOMPRESSED, pushPort.robId)
    val physRs1 = rob.readAsyncSingle(decoder.PHYS_RS(0), pushPort.robId)
    val physRs2 = rob.readAsyncSingle(decoder.PHYS_RS(1), pushPort.robId)
    val readRs1 = rob.readAsyncSingle(decoder.READ_RS(0), pushPort.robId)
    val readRs2 = rob.readAsyncSingle(decoder.READ_RS(1), pushPort.robId)

    setup.rfReadRs1.valid := pushPort.valid && readRs1
    setup.rfReadRs1.address := physRs1
    setup.rfReadRs2.valid := pushPort.valid && readRs2
    setup.rfReadRs2.address := physRs2



    val s0Result = Result()
    s0Result.robId := s0.robId
    s0Result.writeRd := rob.readAsyncSingle(decoder.WRITE_RD, s0.robId)
    s0Result.rd := rob.readAsyncSingle(decoder.PHYS_RD, s0.robId)
    s0Result.value := B(0xFF00 | s0.robId.resized).resized

    val wake = out(s0.translateWith(s0Result).stage().stage().stage())

    setup.rfWriteRd.valid := wake.writeRd
    setup.rfWriteRd.address := wake.rd
    setup.rfWriteRd.data := wake.value

    val wakePort = Flow(Frontend.ROB_ID)
    wakePort.valid := wake.valid
    wakePort.payload := wake.robId

    setup.completion.valid := wake.valid
    setup.completion.id := wake.robId
  }
}
