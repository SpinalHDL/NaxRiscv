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
}

class IntAluPlugin(euId : String, staticLatency : Boolean = true, aluStage : Int = 0) extends ExecutionUnitElementSimple(euId, staticLatency)  {
  import IntAluPlugin._

  override def writeBackAt = aluStage

  override val setup = create early new Setup{
    import SrcKeys._

    val ace = AluCtrlEnum
    val abce = AluBitwiseCtrlEnum

    add(Rvi.ADD , List(Op.ADD   , SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.ADD_SUB ))
    add(Rvi.SUB , List(Op.SUB   , SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.ADD_SUB ))
    add(Rvi.SLT , List(Op.LESS  , SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.SLTU, List(Op.LESS_U, SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.XOR , List(           SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.XOR ))
    add(Rvi.OR  , List(           SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.OR  ))
    add(Rvi.AND , List(           SRC1.RF, SRC2.RF), eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.AND ))

    add(Rvi.ADDI , List(Op.ADD   , SRC1.RF, SRC2.I), eu.DecodeList(ALU_CTRL -> ace.ADD_SUB ))
    add(Rvi.SLTI , List(Op.LESS  , SRC1.RF, SRC2.I), eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.SLTIU, List(Op.LESS_U, SRC1.RF, SRC2.I), eu.DecodeList(ALU_CTRL -> ace.SLT_SLTU))
    add(Rvi.XORI , List(           SRC1.RF, SRC2.I), eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.XOR ))
    add(Rvi.ORI  , List(           SRC1.RF, SRC2.I), eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.OR  ))
    add(Rvi.ANDI , List(           SRC1.RF, SRC2.I), eu.DecodeList(ALU_CTRL -> ace.BITWISE , ALU_BITWISE_CTRL -> abce.AND ))

    add(Rvi.LUI,   List(Op.SRC1  , SRC1.U)         , eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
    add(Rvi.AUIPC, List(Op.ADD   , SRC1.U, SRC2.PC), eu.DecodeList(ALU_CTRL -> AluCtrlEnum.ADD_SUB))
  }

  override val logic = create late new Logic{
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

      val result = ALU_CTRL.mux(
        AluCtrlEnum.BITWISE  -> bitwise,
        AluCtrlEnum.SLT_SLTU -> S(U(ss.LESS, Global.XLEN bits)),
        AluCtrlEnum.ADD_SUB  -> stage(ss.ADD_SUB)
      )

      wb.payload := B(result)
    }
  }
}
