package naxriscv.units

import naxriscv.{Frontend, Global}
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

    eu.setDecodingDefault(SEL, False)
    val baseline = eu.DecodeList(SEL -> True)

    import SrcKeys._

    val ace = AluCtrlEnum
    val abce = AluBitwiseCtrlEnum

    add(Rvi.ADD , List(Op.ADD   , SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.ADD_SUB ))
    add(Rvi.SUB , List(Op.SUB   , SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.ADD_SUB ))
    add(Rvi.SLT , List(Op.LESS  , SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.SLTU, List(Op.LESS_U, SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.XOR , List(           SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.XOR ))
    add(Rvi.OR  , List(           SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.OR  ))
    add(Rvi.AND , List(           SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.AND ))

    add(Rvi.ADDI , List(Op.ADD   , SRC1.RF, SRC2.I), baseline ++ eu.DecodeList(ALU_CTRL -> ace.ADD_SUB ))
    add(Rvi.SLTI , List(Op.LESS  , SRC1.RF, SRC2.I), baseline ++ eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.SLTIU, List(Op.LESS_U, SRC1.RF, SRC2.I), baseline ++ eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.XORI , List(           SRC1.RF, SRC2.I), baseline ++ eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.XOR ))
    add(Rvi.ORI  , List(           SRC1.RF, SRC2.I), baseline ++ eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.OR  ))
    add(Rvi.ANDI , List(           SRC1.RF, SRC2.I), baseline ++ eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.AND ))

    add(Rvi.LUI,   List(Op.SRC1, SRC1.U)           , baseline ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
    add(Rvi.AUIPC, List(Op.ADD, SRC1.U, SRC2.PC)   , baseline ++ eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(aluStage)
      val decode = getService[DecoderService]

      import stage._
      val ss = SrcStageables

      val bitwise = ALU_BITWISE_CTRL.mux(
        AluBitwiseCtrlEnum.AND  -> (ss.SRC1 & ss.SRC2),
        AluBitwiseCtrlEnum.OR   -> (ss.SRC1 | ss.SRC2),
        AluBitwiseCtrlEnum.XOR  -> (ss.SRC1 ^ ss.SRC2)
      )

      // mux results
      val result = ALU_CTRL.mux(
        AluCtrlEnum.BITWISE  -> bitwise,
        AluCtrlEnum.SLT_SLTU -> S(U(ss.LESS, Global.XLEN bits)),
        AluCtrlEnum.ADD_SUB  -> stage(ss.ADD_SUB)
      )

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
