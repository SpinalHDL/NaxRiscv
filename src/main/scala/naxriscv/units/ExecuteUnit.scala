package naxriscv.units

import naxriscv.interfaces.Riscv.IMM
import naxriscv.{Frontend, Global, ROB}
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
    val rf = getService[RegfileService]

    val completion = rob.robCompletion()

    val rfReadRs1 = rf.newRead(withReady = false)
    val rfReadRs2 = rf.newRead(withReady = false)
    val rfWriteRd = rf.newWrite(withReady = false)

    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)
  }

  val logic = create late new Area{
    val rob = getService[RobService]
    val decoder = getService[DecoderService]
    val flush = getService[CommitService].reschedulingPort().valid

    val pushPort = Stream(ExecutionUnitPush())

    val front = new Area{
      val input = pushPort.toFlow.m2sPipe(flush = flush)

      val instruction = rob.readAsyncSingle(Frontend.INSTRUCTION_DECOMPRESSED, input.robId)
      val physRs1 = rob.readAsyncSingle(decoder.PHYS_RS(0), input.robId)
      val physRs2 = rob.readAsyncSingle(decoder.PHYS_RS(1), input.robId)
      val readRs1 = rob.readAsyncSingle(decoder.READ_RS(0), input.robId)
      val readRs2 = rob.readAsyncSingle(decoder.READ_RS(1), input.robId)

      setup.rfReadRs1.valid := input.valid && readRs1
      setup.rfReadRs1.address := physRs1
      setup.rfReadRs2.valid := input.valid && readRs2
      setup.rfReadRs2.address := physRs2

      val output = input
    }



    val s0 = new Area{
      val input = front.output.m2sPipe(flush = flush)

      val rs1 = RegNext(setup.rfReadRs1.data)
      val rs2 = RegNext(setup.rfReadRs2.data)
      val instruction = rob.readAsyncSingle(Frontend.INSTRUCTION_DECOMPRESSED, input.robId)
      val pc = rob.readAsyncSingle(Global.PC, input.robId)
      val imm = new IMM(instruction)


      case class Result() extends Bundle{
        val robId = ROB.ID_TYPE()
        val rd = UInt(log2Up(setup.rf.getPhysicalDepth) bits)
        val value = Bits(32 bits)
        val writeRd = Bool()
        val branch = Bool()
        val pcTarget = Global.PC()
      }

      val result = Result()
      result.robId := input.robId
      result.writeRd := rob.readAsyncSingle(decoder.WRITE_RD, input.robId)
      result.rd := rob.readAsyncSingle(decoder.PHYS_RD, input.robId)
      result.value.assignDontCare()

      result.branch := False
      result.pcTarget := pc + U(imm.s_sext)
      when(instruction(6)){
        result.branch := rs1 === rs2
      } elsewhen(instruction(5)){
        result.value := B(S(rs1) + S(rs2))
      } otherwise {
        result.value := B(S(rs1) + S(imm.i_sext))
      }


      val output = input.translateWith(result)
    }


    val delayed = s0.output.m2sPipe(flush = flush).m2sPipe(flush = flush).m2sPipe(flush = flush)

    setup.rfWriteRd.valid := delayed.valid && delayed.writeRd
    setup.rfWriteRd.robId := delayed.robId
    setup.rfWriteRd.address := delayed.rd
    setup.rfWriteRd.data := delayed.value

    val wakePort = Flow(Frontend.ROB_ID)
    wakePort.valid := delayed.valid
    wakePort.payload := delayed.robId

    setup.completion.valid := delayed.valid
    setup.completion.id := delayed.robId

    setup.reschedule.valid := delayed.valid &&  delayed.branch
    setup.reschedule.trap := False
    setup.reschedule.robId := delayed.robId
    setup.reschedule.cause := 0
    setup.reschedule.tval := 0
    setup.reschedule.pcTarget := delayed.pcTarget
    setup.reschedule.skipCommit := False
  }
}
