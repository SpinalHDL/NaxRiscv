package naxriscv.units

import naxriscv.Frontend
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core.{U, _}
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

object IntAluPlugin extends AreaObject {
  val AluBitwiseCtrlEnum = new SpinalEnum(binarySequential){
    val XOR, OR, AND = newElement()
  }
  val AluCtrlEnum = new  SpinalEnum(binarySequential){
    val ADD_SUB, SLT_SLTU, BITWISE = newElement()
  }

 val ALU_BITWISE_CTRL = Stageable(AluBitwiseCtrlEnum())
 val ALU_CTRL = Stageable(AluCtrlEnum())

  val SEL = Stageable(Bool())
  val TYPE_I = Stageable(Bool())
}

class IntAluPlugin(euId : String, staticLatency : Boolean = true) extends Plugin with WakeRobService with WakeRegFileService {
  withPrefix(euId)

  import IntAluPlugin._
  val aluStage = 0


  override def wakeRobs = if(!staticLatency) List(logic.process.wake.rob) else Nil
  override def wakeRegFile = if(!staticLatency) List(logic.process.wake.rf) else Nil

  val setup = create early new Area{
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    def add(microOp: MicroOp, decoding : eu.DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.setStaticCompletion(microOp, aluStage)
      if(staticLatency) eu.setStaticWake(microOp, aluStage+1) //TODO should not have +1 (need bypass)
      eu.addDecoding(microOp, decoding)
    }

    val baseline = eu.DecodeList(SEL -> True)
    val immediateActions = baseline ++ eu.DecodeList(TYPE_I -> True)
    val nonImmediateActions = baseline ++ eu.DecodeList(TYPE_I -> False)

    eu.setDecodingDefault(SEL, False)
    add(Rvi.ADD, nonImmediateActions ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
    add(Rvi.ADDI, immediateActions ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(aluStage)
      val decode = getService[DecoderService]

      import stage._

      val src1 = S(eu(IntRegFile, RS1))
      val src2 = TYPE_I ? IMM(Frontend.MICRO_OP).i_sext | S(eu(IntRegFile, RS2))

      val result = src1 + src2

      val wb = eu.newWriteback(IntRegFile, RD, stage)
      wb.valid := SEL
      wb.payload := B(result)

      val wake = !staticLatency generate new Area{
        val fire = isFireing && SEL
        val rob = Flow(WakeRob())
        val rf = Flow(WakeRegFile(decode.PHYS_RD, needBypass = false))

        rob.valid := fire
        rob.robId := ExecutionUnitKeys.ROB_ID

        rf.valid := fire
        rf.physical := decode.PHYS_RD
      }
    }
    eu.release()
  }
}
