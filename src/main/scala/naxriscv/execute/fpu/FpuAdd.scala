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
//    val exp21 = insert(rs2.exponent - rs1.exponent)
//    val exp12 = insert(rs1.exponent - rs2.exponent)
//    val expDifAbs = insert(Mux[AFix](exp21.raw.msb, exp12, exp21).asUInt().dropHigh(1).asUInt)
//    val expDifAbsSat = insert(expDifAbs.sat(widthOf(expDifAbs) - log2Up((rs1.mantissa.bitWidth max rs2.mantissa.bitWidth)*2)))
//    val rs1ExponentBigger = insert((exp21.raw.msb || rs2.isZero) && !rs1.isZero)
//    val doSub = rs1.sign ^ rs2.sign
//    val exp = Mux[AFix](rs1ExponentBigger, rs1.exponent, rs2.exponent)
//    val mantissaExpMin = rs1.mantissa.exp min rs2.mantissa.exp

    val exp21 = insert(rs2.exponent - rs1.exponent)
    val exp12 = insert(rs1.exponent - rs2.exponent)
    val expDifAbs = insert(Mux[AFix](exp21.raw.msb, exp12, exp21).asUInt().dropHigh(1).asUInt)
    val rs1ExponentBigger = insert((exp21.isNegative() || rs2.isZero) && !rs1.isZero)
    val rs1ExponentEqual = insert(rs1.exponent === rs2.exponent)
    val rs1MantissaBigger = insert(rs1.mantissa > rs2.mantissa)
    val absRs1Bigger = insert(((rs1ExponentBigger || rs1ExponentEqual && rs1MantissaBigger) && !rs1.isZero || rs1.isInfinity) && !rs2.isInfinity)
    val needSub = insert(rs1.sign ^ rs2.sign)
  }
  import preShift._

  val shifter = new Area {
    import shifterStage._
    val expDifAbsSat = insert(expDifAbs.sat(widthOf(expDifAbs) - log2Up((rs1.mantissa.bitWidth max rs2.mantissa.bitWidth))))
//    val shiftOverflow = (shiftBy >= p.internalMantissaSize+1+addExtraBits)
//    val passThrough = shiftOverflow || (input.rs1.isZero) || (input.rs2.isZero)


    //Note that rs1ExponentBigger can be replaced by absRs1Bigger bellow to avoid xsigned two complement in math block at expense of combinatorial path
    val xySign = insert(absRs1Bigger ? rs1.sign | rs2.sign)
    val xMantissa = insert((absRs1Bigger ? rs1.mantissa | rs2.mantissa) + AFix(1))
    val yMantissaUnshifted = insert((absRs1Bigger ? rs2.mantissa | rs1.mantissa) +  AFix(1))
    val shifter = Shift.rightWithScrap(yMantissaUnshifted.raw ## False, expDifAbsSat)
    val yMantissa = insert(AFix(shifter.dropLow(1).asUInt, yMantissaUnshifted.exp exp))
    val roundingScrap = shifter.lsb

//    when(passThrough) { yMantissa := 0 }
//    when(shiftOverflow) { roundingScrap := True }
    when(!rs1.isNormal || !rs2.isNormal){ roundingScrap := False }
    val xyExponent = insert(absRs1Bigger ? rs1.exponent | rs2.exponent)


//    val rsManExp = rs2.mantissa.exp min rs1.mantissa.exp
//    val manType = AFix.holding(List(rs1.mantissa, rs2.mantissa))
//    val manRs1 = insert(rs1.mantissa.toAFix(manType))
//    val manRs2 = insert(rs2.mantissa.toAFix(manType))
//    val man1   = insert(Mux[AFix](rs1ExponentBigger, manRs1, manRs2))
//    val man2PreShift   = insert(Mux[AFix](rs1ExponentBigger, manRs2, manRs1))
//    val man2 = insert(man2PreShift.resize(manRs1.exp-manRs1.bitWidth exp) >>| AFix(expDifAbsSat, 0 exp))
//    val sumMax = (rs1.factorMax << (rs1.factorExp - man2.exp)) + (rs2.factorMax << (rs2.factorExp - man2.exp))
  }
  import shifter._

  val math = new Area{
    import mathStage._

    val ySigned = insert(yMantissa.negate(needSub, plusOneEnable = !roundingScrap))
    val xyMantissa = insert((xMantissa + ySigned).asAlwaysPositive())
    val shiftOh = insert(OHMasking.firstV2(xyMantissa.raw.reversed)) //The OhMasking.first can be processed in parallel to the xyMantissa carry chaine
    val shift   = insert(OHToUInt(shiftOh))

//    val man2Negate = insert(man2.negate(doSub))
//    val sum12 = insert(man1 + man2Negate)
//    val sum21 = insert(man2 - man1)
//    val sumIsPos = insert(sum21.raw.msb)
////    val sum = insert(AFix(U(sum12.raw.asUInt.dropHigh(1)), exp = sum12.exp)) //Mux[AFix](sumIsPos, sum12, sum12) //TODO manage sub
//    val sum = Stageable(new AFix(maxValue = sumMax, minValue = 0, exp = sum12.exp))
//    val pick21 = insert(doSub && !sum21.raw.msb)
//    when(pick21) {
//      sum := AFix(U(sum21.raw.asUInt.dropHigh(1)), maxValue = sumMax, 0 exp) >> -sum21.exp
//    } otherwise {
//      sum := AFix(U(sum12.raw.asUInt.dropHigh(1)), maxValue = sumMax, 0 exp) >> -sum12.exp
//    }
  }
  import math._

  val norm = new Area{
    import normStage._
    val mantissa = insert(xyMantissa |<< AFix(shift))
    val exponent = insert(xyExponent - AFix(shift) + AFix(1))
    val forceInfinity = insert((rs1.isInfinity || rs2.isInfinity))
    val forceZero = insert(xyMantissa.isZero() || (rs1.isZero && rs2.isZero))
    val infinityNan = insert( (rs1.isInfinity && rs2.isInfinity && (rs1.sign ^ rs2.sign)))
    val forceNan = insert(rs1.isNan || rs2.isNan || infinityNan)
    val xyMantissaZero = insert(xyMantissa.isZero())
  }
  import norm._

  val result  = new Area{
    import resultStage._

    val RESULT = Stageable(FloatUnpacked(
      exponentMax = exponent.maxValue.toInt,
      exponentMin = exponent.minValue.toInt,
      mantissaWidth = mantissa.bitWidth
    ))
//
//    RESULT.sign := ((pick21 ^ !rs1ExponentBigger) ? rs2.sign | rs1.sign)
//    RESULT.exponent := exp
//    RESULT.mantissa := sum
//    RESULT.mode := FloatMode.NORMAL
//    RESULT.quiet := True

    RESULT.sign := xySign
    RESULT.mantissa.raw := (mantissa.raw ## roundingScrap).resized
    RESULT.exponent := exponent.resized
    RESULT.setNormal
    RESULT.quiet := False


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


