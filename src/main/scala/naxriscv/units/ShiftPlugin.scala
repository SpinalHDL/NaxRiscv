package naxriscv.units

import naxriscv.{Frontend, Global}
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

class ShiftPlugin(euId : String, staticLatency : Boolean = true, aluStage : Int = 0) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import ShiftPlugin._

  override def writeBackAt = aluStage

  override val setup = create early new Setup{
    import SrcKeys._

    add(Rvi.SLL , List(SRC1.RF, SRC2.RF), eu.DecodeList(LEFT -> True,  SIGNED -> False))
    add(Rvi.SRL , List(SRC1.RF, SRC2.RF), eu.DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRA , List(SRC1.RF, SRC2.RF), eu.DecodeList(LEFT -> False, SIGNED -> True))
    add(Rvi.SLLI, List(SRC1.RF, SRC2.I ), eu.DecodeList(LEFT -> True , SIGNED -> False))
    add(Rvi.SRLI, List(SRC1.RF, SRC2.I ), eu.DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRAI, List(SRC1.RF, SRC2.I ), eu.DecodeList(LEFT -> False, SIGNED -> True))
  }

  override val logic = create late new Logic{
    val process = new Area {
      val stage = eu.getExecute(aluStage)

      import stage._
      val ss = SrcStageables

      assert(Global.XLEN.get == 32)
      val amplitude  = ss.SRC2(4 downto 0).asUInt
      val reversed   = Mux[SInt](LEFT, ss.SRC1.reversed, ss.SRC1)
      val shifted = (S((SIGNED & ss.SRC1.msb) ## reversed) >> amplitude).resize(Global.XLEN bits)
      val patched = LEFT ? shifted.reversed | shifted

      wb.payload := B(patched)
    }
  }
}
