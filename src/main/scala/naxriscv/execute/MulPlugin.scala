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
                srcAt : Int = 0,
                mulAt : Int = 0,
                sum1At : Int = 1,
                sum2At : Int = 2,
                writebackAt : Int = 2,
                splitWidthA : Int = 16,
                splitWidthB : Int = 16,
                staticLatency : Boolean = true) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import MulPlugin._

  override def euWritebackAt = writebackAt

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
      val MUL_SRC1 = Stageable(Bits(XLEN.get+1 bits))
      val MUL_SRC2 = Stageable(Bits(XLEN.get+1 bits))
      val MUL_SLICES1 = Stageable(Vec(splits.map(e => SInt(e.widthA + e.widthB + 1 bits))))
      val MUL_SLICES2  = Stageable(Vec(splits.drop(sumSplitAt).map(e => SInt(e.widthA + e.widthB + 1 bits))))
      val MUL_SUM1 = Stageable(SInt(finalWidth bits))
      val MUL_SUM2 = Stageable(SInt(finalWidth bits))
    }
    import keys._

    val src = new ExecuteArea(srcAt) {
      import stage._

      val rs1 = eu(IntRegFile, RS1)
      val rs2 = eu(IntRegFile, RS2)
      MUL_SRC1 := (RS1_SIGNED && rs1.msb) ## (rs1)
      MUL_SRC2 := (RS2_SIGNED && rs2.msb) ## (rs2)
    }

    val mul = new ExecuteArea(mulAt) {
      import stage._
      splits.foreach(e => MUL_SLICES1(e.id) := mulSs(MUL_SRC1(e.offsetA, e.widthA bits), MUL_SRC2(e.offsetB, e.widthB bits), e.signedA, e.signedB))
    }

    val sum1 = new ExecuteArea(sum1At) {
      import stage._
      MUL_SUM1 := splits.take(sumSplitAt).map(e => (MUL_SLICES1(e.id) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _).resized
      stage(MUL_SLICES2) := Vec(MUL_SLICES1.drop(sumSplitAt))
    }

    val sum2 = new ExecuteArea(sum2At) {
      import stage._
      MUL_SUM2 := MUL_SUM1 + splits.drop(sumSplitAt).map(e => (MUL_SLICES2(e.id - sumSplitAt) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _)
    }

    val writeback = new ExecuteArea(writebackAt) {
      import stage._
      wb.payload := B(HIGH ? stage(MUL_SUM2)(XLEN, XLEN bits) otherwise stage(MUL_SUM2)(0, XLEN bits))
    }
  }


  def mulSs(a: Bits, b: Bits, aSigned: Boolean, bSigned: Boolean): SInt = (aSigned, bSigned) match {
    case (false, false) => S((U(a) * U(b)).resize(widthOf(a) + widthOf(b) + 1))
    case (false, true) => S(False ## a) * S(b)
    case (true, false) => S(a) * S(False ## b)
    case (true, true) => (S(a) * S(b)).resize(widthOf(a) + widthOf(b) + 1)
  }
}
