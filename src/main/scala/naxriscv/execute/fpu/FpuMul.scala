package naxriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

class FpuMul(pipeline : Pipeline,
             rs1 : Stageable[FloatUnpacked],
             rs2 : Stageable[FloatUnpacked],
             splitWidthA : Int,
             splitWidthB : Int,
             sum1WidthMax : Int,
             sum2WidthMax : Int,
             mulStage : Stage,
             sum1Stage : Stage,
             sum2Stage : Stage,
             sum3Stage : Stage,
             resultStage : Stage) extends Area{

  val mul = new Area {
    import mulStage._
    val M1 = insert(rs1.mantissa.raw.asUInt)
    val M2 = insert(rs2.mantissa.raw.asUInt)
  }

//  val sum1 = new Stage(DIRECT())
  val sum2 = new Area{
    import sum2Stage._
    val EXP = insert(rs1.exponent +^ rs2.exponent)
    val SIGN = insert(rs1.sign ^ rs2.sign)
  }

  val spliter = new PipelinedMul(
    rsA          = mul.M1,
    rsB          = mul.M2,
    splitWidthA  = splitWidthA,
    splitWidthB  = splitWidthB,
    sum1WidthMax = sum1WidthMax,
    sum2WidthMax = sum2WidthMax,
    mulStage     = mulStage,
    sum1Stage    = sum1Stage,
    sum2Stage    = sum2Stage,
    sum3Stage    = sum3Stage
  )

  val result = new Area{
    import resultStage._
    val MANTISSA = insert(AFix(spliter.keys.MUL_RESULT) >> -mulStage(rs1).factorExp - mulStage(rs2).factorExp)
//    val VALUE = Stageable(FloatUnpacked(
//      exponentWidth =
//      factorMax =
//      factorExp =
//    ))
  }
}


