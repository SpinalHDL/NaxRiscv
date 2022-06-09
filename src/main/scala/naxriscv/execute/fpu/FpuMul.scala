package naxriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

class FpuMul(pipeline : Pipeline,
             rs1 : Stageable[FloatUnpacked],
             rs2 : Stageable[FloatUnpacked],
             resultMantissaWidth : Int,
             splitWidthA : Int,
             splitWidthB : Int,
             sum1WidthMax : Int,
             sum2WidthMax : Int,
             mulStage : Stage,
             sum1Stage : Stage,
             sum2Stage : Stage,
             sum3Stage : Stage,
             normStage : Stage,
             resultStage : Stage) extends Area{

  val mul = new Area {
    import mulStage._
    val M1 = insert(U"1" @@ rs1.mantissa.raw.asUInt)
    val M2 = insert(U"1" @@ rs2.mantissa.raw.asUInt)
  }

//  val sum1 = new Stage(DIRECT())
  val sum2 = new Area{
    import sum2Stage._
    val EXP_ADD = insert(rs1.exponent + rs2.exponent)
    val SIGN = insert(rs1.sign ^ rs2.sign)
    val FORCE_ZERO = insert(rs1.isZero || rs2.isZero)
    val FORCE_OVERFLOW = insert(rs1.isInfinity || rs2.isInfinity)
    val INFINITY_NAN = insert(((rs1.isInfinity || rs2.isInfinity) && (rs1.isZero || rs2.isZero)))
    val FORCE_NAN = insert(rs1.isNan || rs2.isNan || INFINITY_NAN)
  }

  import sum2._


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

  val norm = new Area{
    import normStage._
    val needShift = spliter.keys.MUL_RESULT.msb
    val EXP = insert(EXP_ADD + AFix(U(needShift)))
    val MAN = insert(AFix(U(needShift ? spliter.keys.MUL_RESULT.dropHigh(1) | (spliter.keys.MUL_RESULT.dropHigh(2) << 1)), 1-widthOf(spliter.keys.MUL_RESULT) exp))
  }
  import norm._


  val result  = new Area{
    import resultStage._
    val RESULT = Stageable(FloatUnpacked(
      exponentMax = mulStage(rs1).exponentMax + mulStage(rs2).exponentMax + 1,
      exponentMin = mulStage(rs1).exponentMin + mulStage(rs2).exponentMin,
      mantissaWidth = widthOf(MAN)
    ))

    RESULT.sign := SIGN
    RESULT.exponent := EXP
    RESULT.mantissa := MAN
    RESULT.mode := FloatMode.NORMAL
    RESULT.quiet := True

    val NV = insert(False)

    when(FORCE_NAN) {
      RESULT.setNanQuiet
      NV setWhen(INFINITY_NAN || rs1.isNanSignaling || rs2.isNanSignaling)
    }.elsewhen(FORCE_OVERFLOW) {
      RESULT.setInfinity
    }.elsewhen(FORCE_ZERO) {
      RESULT.setZero
    } /*elsewhen(forceUnderflow) { //TODO
      output.exponent := underflowExp.resized
    }*/
  }
}


