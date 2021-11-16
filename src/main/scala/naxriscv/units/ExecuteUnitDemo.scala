package naxriscv.units

import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.riscv.{IMM, Rvi}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

class ExecuteUnitDemo(euId : String, withAdd : Boolean = true) extends Plugin with ExecuteUnitService with WakeService with LockedImpl {
  setName(euId)
  override def uniqueIds = List(euId)

  override def hasFixedLatency = ???

  override def getFixedLatency = ???

  override def pushPort() = logic.pushPort

  override def euName() = euId

  override def wakeRobs = Seq(logic.wakePort)

  def addMicroOp(enc: MicroOp) = {
    getService[DecoderService].addEuOp(this, enc)
  }

  val setup = create early new Area{
    val rob = getService[RobService]
    val rf = getService[RegfileService]

    val completion = rob.robCompletion()

    val rfReadRs1 = rf.newRead(withReady = false)
    val rfReadRs2 = rf.newRead(withReady = false)
    val rfWriteRd = rf.newWrite(withReady = false)

    val reschedule = getService[CommitService].newSchedulePort(canJump = true, canTrap = true)

    if(withAdd)addMicroOp(Rvi.ADD)
    if(withAdd)addMicroOp(Rvi.ADDI)
    addMicroOp(Rvi.BEQ)
  }

  val logic = create late new Area{
    lock.await()

    val rob = getService[RobService]
    val decoder = getService[DecoderService]
    val flush = getService[CommitService].reschedulingPort().valid

    val pushPort = Stream(ExecutionUnitPush())
    val euGroup = decoder.euGroups.find(_.eus.contains(ExecuteUnitDemo.this)).get
    val sf = euGroup.eus.size
    val so = euGroup.eus.indexOf(ExecuteUnitDemo.this)

    case class Front() extends Bundle{
      val robId = ROB.ID_TYPE()
      val instruction = Frontend.MICRO_OP()
      val pc = Global.PC()
      val rs1 = Bits(Global.XLEN bits)
      val rs2 = Bits(Global.XLEN bits)
    }


    val front = new Area{
      val input = pushPort.toFlow.m2sPipe(flush = flush)

      val physRs1 = rob.readAsyncSingle(decoder.PHYS_RS(0), input.robId, sf, so)
      val physRs2 = rob.readAsyncSingle(decoder.PHYS_RS(1), input.robId, sf, so)
      val readRs1 = rob.readAsyncSingle(decoder.READ_RS(0), input.robId, sf, so)
      val readRs2 = rob.readAsyncSingle(decoder.READ_RS(1), input.robId, sf, so)

      setup.rfReadRs1.valid := input.valid && readRs1
      setup.rfReadRs1.address := physRs1
      setup.rfReadRs2.valid := input.valid && readRs2
      setup.rfReadRs2.address := physRs2


      val result = Front()
      result.instruction := rob.readAsyncSingle(Frontend.MICRO_OP, input.robId, sf, so)
      result.pc :=  rob.readAsyncSingle(Global.PC, input.robId, sf, so)
      result.rs1 := setup.rfReadRs1.data
      result.rs2 := setup.rfReadRs2.data
      result.robId := input.robId

      val output = input.translateWith(result)
    }



    val s0 = new Area{
      val input = front.output.m2sPipe(flush = flush)

      val imm = new IMM(input.instruction)


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
      result.writeRd := rob.readAsyncSingle(decoder.WRITE_RD, input.robId, sf, so)
      result.rd := rob.readAsyncSingle(decoder.PHYS_RD, input.robId, sf, so)
      result.value.assignDontCare()

      result.branch := False
      result.pcTarget := input.pc + U(imm.b_sext)
      when(input.instruction(6)){
        result.branch := input.rs1 === input.rs2
      } elsewhen(input.instruction(5)){
        result.value := B(S(input.rs1) + S(input.rs2))
      } otherwise {
        result.value := B(S(input.rs1) + imm.i_sext)
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
