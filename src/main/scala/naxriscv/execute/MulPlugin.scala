// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute

import naxriscv.{DecodeList, Frontend, Global}
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

class MulPlugin(val euId : String,
                var srcAt : Int = 0,
                var mulAt : Int = 0,
                var sum1At : Int = 0,
                var sum2At : Int = 1,
                var sum3At : Int = 2,
                var writebackAt : Int = 2,
                var splitWidthA : Int = 17,
                var splitWidthB : Int = 17,
                var sum1WidthMax : Int = -1,
                var sum2WidthMax : Int = -1,
                var staticLatency : Boolean = true) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import MulPlugin._

  override def euWritebackAt = writebackAt

  override val setup = create early new Setup{
    import SrcKeys._

    if(sum1WidthMax == -1) sum1WidthMax = XLEN.get/2
    if(sum2WidthMax == -1) sum2WidthMax = XLEN.get*4/3

    getServiceOption[PrivilegedService].foreach(_.addMisa('M'))

    add(Rvi.MUL   , List(), DecodeList(HIGH -> False, RS1_SIGNED -> True,  RS2_SIGNED -> True))
    add(Rvi.MULH  , List(), DecodeList(HIGH -> True,  RS1_SIGNED -> True,  RS2_SIGNED -> True))
    add(Rvi.MULHSU, List(), DecodeList(HIGH -> True,  RS1_SIGNED -> True,  RS2_SIGNED -> False))
    add(Rvi.MULHU , List(), DecodeList(HIGH -> True,  RS1_SIGNED -> False, RS2_SIGNED -> False))

    if(XLEN.get == 64){
      add(Rvi.MULW   , List(), DecodeList(HIGH -> False, RS1_SIGNED -> True,  RS2_SIGNED -> True))
      for(op <- List(Rvi.MULW)){
        signExtend(op, 31)
      }
    }
  }

  override val logic = create late new Logic{
    val splits = MulSpliter.splits(XLEN.get + 1, XLEN.get + 1, splitWidthA, splitWidthB, true, true)
    val finalWidth = XLEN*2+2
    val sum1Takes = splits.takeWhile(e => e.offsetC + e.widthC <= sum1WidthMax).size
    val sum2Takes = splits.takeWhile(e => e.offsetC + e.widthC <= sum2WidthMax).size-sum1Takes
    val sum3Takes = splits.size - sum1Takes - sum2Takes

    val keys = new AreaRoot{
      val MUL_SRC1 = Stageable(Bits(XLEN.get+1 bits))
      val MUL_SRC2 = Stageable(Bits(XLEN.get+1 bits))
      val MUL_SLICES1 = Stageable(Vec(splits.map(e => SInt(e.widthA + e.widthB + 1 bits))))
      val MUL_SLICES2  = Stageable(Vec(splits.drop(sum1Takes).map(e => SInt(e.widthA + e.widthB + 1 bits))))
      val MUL_SUM1 = Stageable(SInt(finalWidth bits))
      val MUL_SUM2 = Stageable(SInt(finalWidth bits))
      val MUL_SUM3 = Stageable(SInt(finalWidth bits))
      val MUL_SLICES3_REDUCED = Stageable(SInt(finalWidth bits))
    }
    import keys._

    val src = new ExecuteArea(srcAt) {
      import stage._

      val rs1 = eu(IntRegFile, RS1)
      val rs2 = eu(IntRegFile, RS2)
      MUL_SRC1 := (RS1_SIGNED && rs1.msb) ## (rs1)
      MUL_SRC2 := (RS2_SIGNED && rs2.msb) ## (rs2)

      KeepAttribute(stage(MUL_SRC1))
      KeepAttribute(stage(MUL_SRC2))
    }

    val mul = new ExecuteArea(mulAt) {
      import stage._
      splits.foreach(e => MUL_SLICES1(e.id) := mulSs(MUL_SRC1(e.offsetA, e.widthA bits), MUL_SRC2(e.offsetB, e.widthB bits), e.signedA, e.signedB))
    }

    val sum1 = new ExecuteArea(sum1At) {
      import stage._
      MUL_SUM1 := (if(sum1Takes != 0) splits.take(sum1Takes).map(e => (MUL_SLICES1(e.id) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _).resized else S(0))
      stage(MUL_SLICES2) := Vec(MUL_SLICES1.drop(sum1Takes))

      KeepAttribute(stage(MUL_SLICES2))
    }

    val sum2 = new ExecuteArea(sum2At) {
      import stage._
      MUL_SUM2 := (stage(MUL_SUM1) +: splits.drop(sum1Takes).take(sum2Takes).map(e => (MUL_SLICES2(e.id - sum1Takes) << e.offsetC).resize(finalWidth))).reduceBalancedTree(_ + _)

      if(sum3Takes != 0) {
        MUL_SLICES3_REDUCED := splits.drop(sum1Takes + sum2Takes).map(e => (MUL_SLICES2(e.id - sum1Takes) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _)
      }
    }

    val sum3 = new ExecuteArea(sum3At) {
      import stage._
      MUL_SUM3 := (if(sum3Takes != 0) MUL_SUM2 + MUL_SLICES3_REDUCED else stage(MUL_SUM2))
    }


    val writeback = new ExecuteArea(writebackAt) {
      import stage._
      wb.payload := B(HIGH ? stage(MUL_SUM3)(XLEN, XLEN bits) otherwise stage(MUL_SUM3)(0, XLEN bits))
    }
  }


  def mulSs(a: Bits, b: Bits, aSigned: Boolean, bSigned: Boolean): SInt = (aSigned, bSigned) match {
    case (false, false) => S((U(a) * U(b)).resize(widthOf(a) + widthOf(b) + 1))
    case (false, true) => S(False ## a) * S(b)
    case (true, false) => S(a) * S(False ## b)
    case (true, true) => (S(a) * S(b)).resize(widthOf(a) + widthOf(b) + 1)
  }
}
