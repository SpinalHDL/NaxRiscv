package naxriscv.execute

import naxriscv.{Frontend, Global}
import naxriscv.Global._
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.{MulSpliter, Plugin}
import spinal.core._
import spinal.lib._
import spinal.core.fiber.Handle
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

object MulPlugin extends AreaObject {
  val HIGH = Stageable(Bool())
  val RS1_SIGNED = Stageable(Bool())
  val RS2_SIGNED = Stageable(Bool())
}

class MulPlugin(euId : String,
                writebackAt : Int = 0,
                splitWidthA : Int = 16,
                splitWidthB : Int = 16,
                staticLatency : Boolean = true) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import MulPlugin._

  override def writeBackAt = writebackAt

  override val setup = create early new Setup{
    import SrcKeys._


    add(Rvi.MUL   , List(), eu.DecodeList(HIGH -> False, RS1_SIGNED -> True,  RS2_SIGNED -> True))
    add(Rvi.MULH  , List(), eu.DecodeList(HIGH -> True,  RS1_SIGNED -> True,  RS2_SIGNED -> True))
    add(Rvi.MULHSU, List(), eu.DecodeList(HIGH -> True,  RS1_SIGNED -> True,  RS2_SIGNED -> False))
    add(Rvi.MULHU , List(), eu.DecodeList(HIGH -> True,  RS1_SIGNED -> False, RS2_SIGNED -> False))
  }

  override val logic = create late new Logic{
    val splits = MulSpliter.splits(XLEN.get() + 1, XLEN.get() + 1, splitWidthA, splitWidthB, true, true)
    val finalWidth = XLEN*2+2
    val sumSplitAt = splits.size/2

    val keys = new AreaRoot{
      val MULS = Stageable(Vec(splits.map(e => SInt(e.widthA + e.widthB + 1 bits))))
      val MULS2  = Stageable(Vec(splits.drop(sumSplitAt).map(e => SInt(e.widthA + e.widthB + 1 bits))))
      val MULC2 = Stageable(SInt(finalWidth bits))
      val SUM = Stageable(SInt(finalWidth bits))
    }
    import keys._

    val feed = new Area {
      val stage = eu.getExecute(0)
      import stage._

      val rs1 = eu(IntRegFile, RS1)
      val rs2 = eu(IntRegFile, RS2)
      val mulA = (RS1_SIGNED && rs1.msb) ## (rs1)
      val mulB = (RS2_SIGNED && rs2.msb) ## (rs2)

      def mul(a : Bits, b : Bits, aSigned : Boolean, bSigned : Boolean): SInt = (aSigned, bSigned) match {
        case (false, false) => S((U(a) * U(b)).resize(widthOf(a) + widthOf(b) + 1))
        case (false, true ) => S(False ## a) * S(b)
        case (true , false) => S(a) * S(False ## b)
        case (true , true ) => (S(a) * S(b)).resize(widthOf(a) + widthOf(b) + 1)
      }
      splits.foreach(e => MULS(e.id) := mul(mulA(e.offsetA, e.widthA bits), mulB(e.offsetB, e.widthB bits), e.signedA, e.signedB))
      MULC2 := splits.take(sumSplitAt).map(e => (MULS(e.id) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _).resized
      stage(MULS2) := Vec(MULS.drop(sumSplitAt))
      SUM := MULC2 + splits.drop(sumSplitAt).map(e => (MULS2(e.id-sumSplitAt) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _)

      wb.payload := B(HIGH ? stage(SUM)(XLEN, XLEN bits) otherwise stage(SUM)(0, XLEN bits))
    }
  }
}
