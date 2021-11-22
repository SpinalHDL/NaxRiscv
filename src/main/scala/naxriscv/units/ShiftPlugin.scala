package naxriscv.units

import naxriscv.{Frontend, Global}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

object ShiftPlugin extends AreaObject {
  val SEL = Stageable(Bool())
  val SIGNED = Stageable(Bool())
  val LEFT = Stageable(Bool())
}

class ShiftPlugin(euId : String, staticLatency : Boolean = true) extends Plugin with WakeRobService with WakeRegFileService {
  withPrefix(euId)

  import ShiftPlugin._
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

    add(Rvi.SLL , List(SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(LEFT -> True,  SIGNED -> False))
    add(Rvi.SRL , List(SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRA , List(SRC1.RF, SRC2.RF), baseline ++ eu.DecodeList(LEFT -> False, SIGNED -> True))
    add(Rvi.SLLI, List(SRC1.RF, SRC2.I ), baseline ++ eu.DecodeList(LEFT -> True , SIGNED -> False))
    add(Rvi.SRLI, List(SRC1.RF, SRC2.I ), baseline ++ eu.DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRAI, List(SRC1.RF, SRC2.I ), baseline ++ eu.DecodeList(LEFT -> False, SIGNED -> True))
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val process = new Area {
      val stage = eu.getExecute(aluStage)
      val decode = getService[DecoderService]

      import stage._
      val ss = SrcStageables

      assert(Global.XLEN.get == 32)
      val sel = ss.SRC2(4 downto 0)
      val rotate = ss.SRC1.rotateLeft(U(sel))

      val amplitude  = ss.SRC2(4 downto 0).asUInt
      val reversed   = Mux[SInt](LEFT, ss.SRC1.reversed, ss.SRC1)
      val shifted = (S((SIGNED & ss.SRC1.msb) ## reversed) >> amplitude).resize(Global.XLEN bits)
      val patched = LEFT ? shifted.reversed | shifted

      val wb = eu.newWriteback(IntRegFile, RD, stage, if(staticLatency) 0 else 1)
      wb.valid := SEL
      wb.payload := B(patched)

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
