package naxriscv.execute.fpu

import spinal.core.{widthOf, _}
import spinal.lib.Shift
import spinal.lib.pipeline._

class FpuAdd(pipeline : Pipeline,
             rs1 : Stageable[FloatUnpacked],
             rs2 : Stageable[FloatUnpacked],
             preShiftStage : Stage,
             shifterStage : Stage,
             mathStage : Stage,
             resultStage : Stage) extends Area{

  val preShift = new Area {
    import preShiftStage._
    val exp21 = insert(rs2.exponent - rs1.exponent)
    val exp12 = insert(rs1.exponent - rs2.exponent)
    val expDifAbs = insert(Mux[AFix](exp21.raw.msb, exp12, exp21).asUInt())
    val rs1ExponentBigger = insert((exp21.raw.msb || rs2.isZero) && !rs1.isZero)
    val doSub = rs1.sign ^ rs2.sign
    val exp = Mux[AFix](rs1ExponentBigger, rs1.exponent, rs2.exponent)

  }
  import preShift._

  val shifter = new Area {
    import shifterStage._
    val rsManExp = rs2.mantissa.exp.value min rs1.mantissa.exp.value
    val manType = AFix.holding(List(rs1.mantissa, rs2.mantissa))
    val manRs1 = insert(rs1.mantissa.toAFix(manType))
    val manRs2 = insert(rs2.mantissa.toAFix(manType))
    val man12CommonShift = widthOf(manRs1.raw)
    val man1   = insert(Mux[AFix](rs1ExponentBigger, manRs1, manRs2))
    val man2PreShift   = insert(Mux[AFix](rs1ExponentBigger, manRs2, manRs1))
//    val man2Shifter = man2PreShift.raw << 1 + widthOf(manRs1.raw), expDifAbs) //TODO expDifAbs resized
    val man2 = insert(man2PreShift >> AFix(expDifAbs, maxValue = man12CommonShift))  //TODO expDifAbs resized

  }
  import shifter._

  val math = new Area{
    import mathStage._
    val man2Negate = insert(man2.negate(doSub))
    val sum12 = insert(man1 + man2Negate)
    val sum21 = insert(man2 - man1)
    val sumIsPos = insert(sum21.raw.msb)
    val sum = insert(AFix(U(sum12.raw.asUInt.dropHigh(1)), exp = sum12.exp)) //Mux[AFix](sumIsPos, sum12, sum12) //TODO manage sub
  }
  import math._

  val result  = new Area{
    import resultStage._
    import resultStage._
    val RESULT = Stageable(FloatUnpacked(
      exponentMax = exp.maxValue.toInt,
      exponentMin = exp.minValue.toInt,
      factorMax   = sum.maxValue,
      factorExp   = sum.exp.value
    ))

    RESULT.sign := False //TODO
    RESULT.exponent := exp
    RESULT.mantissa := sum
    RESULT.mode := FloatMode.NORMAL
    RESULT.quiet := True
    val xx = out(RESULT)
  }
}


