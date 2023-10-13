// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute.fpu

import spinal.core.{widthOf, _}
import spinal.lib.Shift
import spinal.lib.pipeline._
import spinal.lib._

class FpuAdd(pipeline : Pipeline,
             rs1 : Stageable[FloatUnpacked],
             rs2 : Stageable[FloatUnpacked],
             roundDown : Stageable[Bool],
             preShiftStage : Stage,
             shifterStage : Stage,
             mathStage : Stage,
             normStage : Stage,
             resultStage : Stage) extends Area{

  val preShift = new Area {
    import preShiftStage._
    val exp21             = insert(rs2.exponent - rs1.exponent)
    val exp12             = insert(rs1.exponent - rs2.exponent)
    val expDifAbs         = insert(Mux[AFix](exp21.raw.msb, exp12, exp21).asUInt().dropHigh(1).asUInt)
    val rs1ExponentBigger = insert((exp21.isNegative() || rs2.isZero) && !rs1.isZero)
    val rs1ExponentEqual  = insert(rs1.exponent === rs2.exponent)
    val rs1MantissaBigger = insert(rs1.mantissa > rs2.mantissa)
    val absRs1Bigger      = insert(((rs1ExponentBigger || rs1ExponentEqual && rs1MantissaBigger) && !rs1.isZero || rs1.isInfinity) && !rs2.isInfinity)
    val needSub           = insert(rs1.sign ^ rs2.sign)
    val passThrough       = insert(rs1.isZero || rs2.isZero)
    val expDifAbsSat      = insert(expDifAbs.sat(widthOf(expDifAbs) - log2Up((rs1.mantissa.bitWidth max rs2.mantissa.bitWidth))).orMask(passThrough))
  }
  import preShift._

  val shifter = new Area {
    import shifterStage._

    //Assume the shifter can totaly clear things
    //Note that rs1ExponentBigger can be replaced by absRs1Bigger bellow to avoid xsigned two complement in math block at expense of combinatorial path
    val xySign             = insert(absRs1Bigger ? rs1.sign | rs2.sign)
    val xMantissa          = insert((absRs1Bigger ? rs1.mantissa | rs2.mantissa) + AFix(1))
    val yMantissaUnshifted = insert((absRs1Bigger ? rs2.mantissa | rs1.mantissa) +  AFix(1))
    val shifter            = insert(Shift.rightWithScrap(yMantissaUnshifted.raw ## False ## False, expDifAbsSat))
    val yMantissa          = insert(AFix(shifter.dropLow(1).asUInt, yMantissaUnshifted.exp-1 exp))

    val xyExponent = insert(absRs1Bigger ? rs1.exponent | rs2.exponent)
  }
  import shifter._

  val math = new Area{
    import mathStage._
    val roundingScrap = insert(shifter.shifter.lsb && !passThrough)
    when(!rs1.isNormal || !rs2.isNormal){ roundingScrap := False }
    val ySigned    = insert(yMantissa.negate(needSub, plusOneEnable = !roundingScrap))
    val xyMantissa = insert((xMantissa + ySigned).asAlwaysPositive())
  }
  import math._

  val norm = new Area{
    import normStage._
    val shiftOh        = insert(OHMasking.firstV2(xyMantissa.raw.reversed)) //The OhMasking.first can be processed in parallel to the xyMantissa carry chaine
    val shift          = insert(OHToUInt(shiftOh))
    val forceInfinity  = insert((rs1.isInfinity || rs2.isInfinity))
    val forceZero      = insert(xyMantissa.isZero() || (rs1.isZero && rs2.isZero))
    val infinityNan    = insert(rs1.isInfinity && rs2.isInfinity && (rs1.sign ^ rs2.sign))
    val forceNan       = insert(rs1.isNan || rs2.isNan || infinityNan)
    val xyMantissaZero = insert(xyMantissa.isZero())
  }
  import norm._

  val result  = new Area{
    import resultStage._

    val exponent = insert(xyExponent - AFix(shift) + AFix(1))
    val mantissa = insert(xyMantissa |<< AFix(shift))

    val RESULT = Stageable(FloatUnpacked(
      exponentMax = exponent.maxRaw.toInt,
      exponentMin = exponent.minRaw.toInt,
      mantissaWidth = mantissa.bitWidth
    ))

    RESULT.sign         := xySign
    RESULT.mantissa.raw := (mantissa.raw ## roundingScrap).resized
    RESULT.exponent     := exponent.resized
    RESULT.quiet        := False
    RESULT.setNormal


    val NV = insert(infinityNan || rs1.isNanSignaling || rs2.isNanSignaling)

    when(forceNan) {
      RESULT.setNanQuiet
    }.elsewhen (forceInfinity) {
      RESULT.setInfinity
    }.elsewhen (forceZero) {
      RESULT.setZero
      when(xyMantissaZero || rs1.isZero && rs2.isZero) {
        RESULT.sign := rs1.sign && rs2.sign
      }
      when((rs1.sign || rs2.sign) && roundDown) {
        RESULT.sign := True
      }
    }
  }
}


