package naxriscv.execute.fpu

import naxriscv.utilities.MulSpliter
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

class PipelinedMul(rsA : Stageable[UInt],
                   rsB : Stageable[UInt],
                   splitWidthA : Int,
                   splitWidthB : Int,
                   sum1WidthMax : Int,
                   sum2WidthMax : Int,
                   mulStage : Stage,
                   sum1Stage : Stage,
                   sum2Stage : Stage,
                   sum3Stage : Stage) extends Area{
  val splits = MulSpliter.splits(widthOf(rsA), widthOf(rsB), splitWidthA, splitWidthB, false, false)
  val finalWidth = widthOf(rsA) + widthOf(rsB)
  val sum1Takes = splits.takeWhile(e => e.offsetC + e.widthC <= sum1WidthMax).size
  val sum2Takes = splits.takeWhile(e => e.offsetC + e.widthC <= sum2WidthMax).size-sum1Takes
  val sum3Takes = splits.size - sum1Takes - sum2Takes



  val keys = new AreaRoot{
    val MUL_SLICES1 = Stageable(Vec(splits.map(e => UInt(e.widthA + e.widthB bits))))
    val MUL_SLICES2  = Stageable(Vec(splits.drop(sum1Takes).map(e => UInt(e.widthA + e.widthB bits))))
    val MUL_SUM1 = Stageable(UInt(finalWidth bits))
    val MUL_SUM2 = Stageable(UInt(finalWidth bits))
    val MUL_SUM3 = Stageable(UInt(finalWidth bits))
    val MUL_SLICES3_REDUCED = Stageable(UInt(finalWidth bits))
  }
  import keys._

  val mul = new Area{
    import mulStage._
    splits.foreach(e => MUL_SLICES1(e.id) := (rsA(e.offsetA, e.widthA bits) * rsB(e.offsetB, e.widthB bits)))
    KeepAttribute(mulStage(rsA))
    KeepAttribute(mulStage(rsB))
  }

  val sum1 = new Area {
    import sum1Stage._
    MUL_SUM1 := (if(sum1Takes != 0) splits.take(sum1Takes).map(e => (MUL_SLICES1(e.id) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _).resized else U(0))
    sum1Stage(MUL_SLICES2) := Vec(MUL_SLICES1.drop(sum1Takes))

    KeepAttribute(sum1Stage(MUL_SLICES2))
  }

  val sum2 = new Area {
    import sum2Stage._
    MUL_SUM2 := (sum2Stage(MUL_SUM1) +: splits.drop(sum1Takes).take(sum2Takes).map(e => (MUL_SLICES2(e.id - sum1Takes) << e.offsetC).resize(finalWidth))).reduceBalancedTree(_ + _)

    if(sum3Takes != 0) {
      MUL_SLICES3_REDUCED := splits.drop(sum1Takes + sum2Takes).map(e => (MUL_SLICES2(e.id - sum1Takes) << e.offsetC).resize(finalWidth)).reduceBalancedTree(_ + _)
    }
  }

  val sum3 = new Area {
    import sum3Stage._
    MUL_SUM3 := (if(sum3Takes != 0) MUL_SUM2 + MUL_SLICES3_REDUCED else sum3Stage(MUL_SUM2))
  }
}


