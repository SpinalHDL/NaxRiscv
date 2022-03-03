package naxriscv.execute

import naxriscv.{DecodeList, Frontend, Global}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.core.fiber.Handle
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

object ShiftPlugin extends AreaObject {
  val SIGNED = Stageable(Bool())
  val LEFT = Stageable(Bool())
}

class ShiftPlugin(val euId : String,
                  var staticLatency : Boolean = true,
                  var aluStage : Int = 0) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import ShiftPlugin._

  override def euWritebackAt = aluStage

  override val setup = create early new Setup{
    import SrcKeys._

    add(Rvi.SLL , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> True,  SIGNED -> False))
    add(Rvi.SRL , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRA , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> True))
    add(Rvi.SLLI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> True , SIGNED -> False))
    add(Rvi.SRLI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRAI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> True))
  }

  override val logic = create late new Logic{
    val process = new ExecuteArea(aluStage) {
      import stage._
      val ss = SrcStageables

      if(Global.XLEN.get != 32) SpinalWarning("RV64")
      val amplitude  = ss.SRC2(4 downto 0).asUInt
      val reversed   = Mux[SInt](LEFT, ss.SRC1.reversed, ss.SRC1)
      val shifted = (S((SIGNED & ss.SRC1.msb) ## reversed) >> amplitude).resize(Global.XLEN bits)
      val patched = LEFT ? shifted.reversed | shifted

      wb.payload := B(patched)
    }
  }
}
