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
    val EXP = insert(rs1.exponent + rs2.exponent)
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

  val result  = new Area{
    import resultStage._
    val RESULT = Stageable(FloatUnpacked(
      exponentMax = mulStage(rs1).exponentMax + mulStage(rs2).exponentMax,
      exponentMin = mulStage(rs1).exponentMin + mulStage(rs2).exponentMin,
      factorMax   = mulStage(rs1).factorMax   * mulStage(rs2).factorMax,
      factorExp   = mulStage(rs1).factorExp   + mulStage(rs2).factorExp
    ))

    RESULT.sign := SIGN
    RESULT.exponent := sum2.EXP
    RESULT.mantissa := (AFix(spliter.keys.MUL_RESULT) >> -mulStage(rs1).factorExp - mulStage(rs2).factorExp).truncated()
    RESULT.mode := FloatMode.NORMAL
    RESULT.quiet := True

    val NV = False

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


