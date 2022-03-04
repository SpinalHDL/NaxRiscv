package naxriscv.execute

import naxriscv.Global.XLEN
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
  val IS_W = Stageable(Bool())
  val IS_W_RIGHT = Stageable(Bool())
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
    add(Rvi.SRA , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> True ))
    add(Rvi.SLLI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> True , SIGNED -> False))
    add(Rvi.SRLI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRAI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> True ))



    if(XLEN.get == 64){
      for(op <- List(Rvi.SLL, Rvi.SRL, Rvi.SRA, Rvi.SLLI, Rvi.SRLI, Rvi.SRAI)) {
        eu.addDecoding(op, DecodeList(IS_W -> False, IS_W_RIGHT -> False))
      }
      add(Rvi.SLLW , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> True,  SIGNED -> False, IS_W -> True, IS_W_RIGHT -> False))
      add(Rvi.SRLW , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> False, IS_W -> True, IS_W_RIGHT -> True))
      add(Rvi.SRAW , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> True , IS_W -> True, IS_W_RIGHT -> True))
      add(Rvi.SLLIW, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> True , SIGNED -> False, IS_W -> True, IS_W_RIGHT -> False))
      add(Rvi.SRLIW, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> False, IS_W -> True, IS_W_RIGHT -> True))
      add(Rvi.SRAIW, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> True , IS_W -> True, IS_W_RIGHT -> True))
      for(op <- List(Rvi.SLLW, Rvi.SRLW, Rvi.SRAW, Rvi.SLLIW, Rvi.SRLIW, Rvi.SRAIW)) {
        signExtend(op, 31)
      }
    }
  }

  override val logic = create late new Logic{
    val process = new ExecuteArea(aluStage) {
      import stage._
      val ss = SrcStageables

      val amplitude = ss.SRC2(log2Up(XLEN.get)-1 downto 0).asUInt
      val reversed  = Mux[SInt](LEFT, ss.SRC1.reversed, ss.SRC1)
      val shifted   = (S((SIGNED & ss.SRC1.msb) ## reversed) >> amplitude).resize(Global.XLEN bits)
      val patched   = LEFT ? shifted.reversed | shifted

      if(XLEN.get == 64){
        when(IS_W_RIGHT) {
          reversed(63 downto 32) := (default -> (SIGNED & ss.SRC1(31)))
        }
        when(IS_W){
          amplitude(5) := False
        }
      }

      wb.payload := B(patched)
    }
  }
}
