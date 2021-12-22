package naxriscv.execute

import naxriscv.Global._
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.{DivRadix4, MulSpliter}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable


object DivPlugin extends AreaObject {
  val REM = Stageable(Bool())
  val SIGNED = Stageable(Bool())
}

class DivPlugin(euId : String,
                writebackAt : Int = 0,
                splitWidthA : Int = 16,
                splitWidthB : Int = 16,
                staticLatency : Boolean = true) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import DivPlugin._

  override def writeBackAt = writebackAt

  override val setup = create early new Setup{
    add(Rvi.DIV , List(), eu.DecodeList(REM -> False, SIGNED -> True))
    add(Rvi.DIVU, List(), eu.DecodeList(REM -> False, SIGNED -> False))
    add(Rvi.REM , List(), eu.DecodeList(REM -> True , SIGNED -> True))
    add(Rvi.REMU, List(), eu.DecodeList(REM -> True , SIGNED -> False))
  }

  override val logic = create late new Logic{
    val splits = MulSpliter.splits(XLEN.get() + 1, XLEN.get() + 1, splitWidthA, splitWidthB, true, true)
    val finalWidth = XLEN*2+2
    val sumSplitAt = splits.size/2

    val keys = new AreaRoot{

    }
    import keys._

    val feed = new Area {
      val stage = eu.getExecute(0)
      import stage._

      val rs1 = eu(IntRegFile, RS1)
      val rs2 = eu(IntRegFile, RS2)

      def twoComplement(that : Bits, enable: Bool): UInt = (Mux(enable, ~that, that).asUInt + enable.asUInt)
      val revertA = SIGNED && rs1.msb
      val revertB = SIGNED && rs2.msb
      val divA = twoComplement(rs1, revertA)
      val divB = twoComplement(rs2, revertB)
      val revertResult = (revertA ^ (revertB && !REM)) && !(rs2 === 0 && SIGNED && !REM)
      val div = DivRadix4(width = XLEN.get+2)

      val cmdSent = RegInit(False) setWhen(div.io.cmd.fire) clearWhen(isReady || isFlushed)
      div.io.cmd.valid := isValid && SEL && !cmdSent
      div.io.cmd.a := divA.resized
      div.io.cmd.b := divB.resized

      div.io.flush := isFlushed
      div.io.rsp.ready := isReady
      haltIt(isValid && SEL && !div.io.rsp.valid)

      val selected = REM ? div.io.rsp.remain otherwise div.io.rsp.result
      val result = twoComplement(B(selected), revertResult)
      wb.payload := B(result).resized

    }
  }
}
