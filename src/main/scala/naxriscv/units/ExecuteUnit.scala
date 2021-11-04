package naxriscv.units

import naxriscv.interfaces.Riscv.IMM
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


    val s0 = new Area{
      val input = pushPort.toFlow.stage()

      val rs1 = RegNext(setup.rfReadRs1.data)
      val rs2 = RegNext(setup.rfReadRs2.data)
      val instruction = rob.readAsyncSingle(Frontend.INSTRUCTION_DECOMPRESSED, input.robId)
      val imm = new IMM(instruction)


      val result = Result()
      result.robId := input.robId
      result.writeRd := rob.readAsyncSingle(decoder.WRITE_RD, input.robId)
      result.rd := rob.readAsyncSingle(decoder.PHYS_RD, input.robId)
      when(instruction(5)){
        result.value := B(S(rs1) + S(rs2))
      } otherwise {
        result.value := B(S(rs1) + S(imm.i))
      }


      val output = input.translateWith(result)
    }


    val wake = out(s0.output.stage().stage().stage())

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
