package naxriscv.units

import naxriscv.Frontend
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
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
    val src = getService[SrcPlugin](euId)
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    def add(microOp: MicroOp, srcKeys : List[SrcKeys], decoding : eu.DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.setStaticCompletion(microOp, aluStage)
      if(staticLatency) eu.setStaticWake(microOp, aluStage)
      eu.addDecoding(microOp, decoding)
      if(srcKeys.nonEmpty) src.specify(microOp, srcKeys)
    }

    val baseline = eu.DecodeList(SEL -> True)
    val immediateActions = baseline ++ eu.DecodeList(TYPE_I -> True)
    val nonImmediateActions = baseline ++ eu.DecodeList(TYPE_I -> False)

    import SrcKeys._
    eu.setDecodingDefault(SEL, False)
    add(Rvi.ADD,   List(Op.ADD, SRC1.RF, SRC2.RF), nonImmediateActions ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
    add(Rvi.ADDI,  List(Op.ADD, SRC1.RF, SRC2.I ), immediateActions    ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
    add(Rvi.LUI,   List(Op.SRC1, SRC1.U),          nonImmediateActions ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
    add(Rvi.AUIPC, List(Op.ADD, SRC1.U, SRC2.PC),  nonImmediateActions ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(aluStage)
      val decode = getService[DecoderService]

      import stage._
      val ss = SrcStageables
      val result = ss.ADD_SUB

      val wb = eu.newWriteback(IntRegFile, RD, stage, if(staticLatency) 0 else 1)
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
