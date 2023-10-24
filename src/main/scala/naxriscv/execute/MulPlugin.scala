// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute

import naxriscv.{DecodeList, Frontend, Global}
import naxriscv.Global._
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.{AdderAggregator, MulSpliter, Plugin}
import spinal.core._
import spinal.lib._
import spinal.core.fiber.Handle
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

import scala.collection.mutable

object MulPlugin extends AreaObject {
  val HIGH = Stageable(Bool())
  val RS1_SIGNED = Stageable(Bool())
  val RS2_SIGNED = Stageable(Bool())
}

/**
 *
 * @param sumsSpec Specify in how many stages the multiply chunks are added together.
 *                 For instance, List((17, 2),(44, 8), (100, 100)) mean that it is done on 3 steps (2 stages)
 *                 first argument of each step specify how deep can be each adder
 *                 seconde argument of each step specify how many inputs each adder can have.
 * @param bufferedHigh When activated, will buffer in a register the XLEN MSB of the result to improve timings
 *                     at the cost of 1 cycle lost (only when reading the XLEN MSB)
 *                     Activate itself by default if XLEN == 64
 */
class MulPlugin(val euId : String,
                var srcAt : Int = 0,
                var mulAt: Int = 0,
                var sumAt: Int = 1,
                var sumsSpec: List[(Int, Int)] = List((44, 8), (1000, 1000)),
                var writebackAt : Int = 2,
                var splitWidthA : Int = 17,
                var splitWidthB : Int = 17,
                var staticLatency : Boolean = true,
                var bufferedHigh : Option[Boolean] = None) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import MulPlugin._

  override def euWritebackAt = writebackAt

  override val setup = create early new Setup{
    import SrcKeys._

    getServiceOption[PrivilegedService].foreach(_.addMisa('M'))

    if(bufferedHigh == None) bufferedHigh = Some(XLEN >= 64)
    if(bufferedHigh.get) {
      eu.setDecodingDefault(HIGH, False)
      assert(!staticLatency)
    }

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

    val finalWidth = XLEN*2

    val keys = new AreaRoot{
      val MUL_SRC1 = Stageable(Bits(XLEN.get+1 bits))
      val MUL_SRC2 = Stageable(Bits(XLEN.get+1 bits))
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

    // Generate all the partial multiplications
    val mul = new ExecuteArea(mulAt) {
      import stage._
      // MulSpliter.splits Will generate a data model of all partial multiplications
      val splits = MulSpliter.splits(XLEN.get + 1, XLEN.get + 1, splitWidthA, splitWidthB, true, true)
      // Generate the partial multiplications from the splits data model
      val VALUES = splits.map(s => insert(s.toMulU(MUL_SRC1, MUL_SRC2, finalWidth)))
      VALUES.foreach(e => KeepAttribute(stage(e)))
    }

    // sourcesSpec will track the partial sum positions
    var sourcesSpec = mul.splits.map(s => AdderAggregator.Source(s, finalWidth)).toList
    // sourceToSignal will allow to retrieve the hardware signal from a sourcesSpec element
    val sourceToSignal = mutable.LinkedHashMap[AdderAggregator.Source, Stageable[UInt]]()
    for((s, m) <- (sourcesSpec, mul.VALUES).zipped) sourceToSignal(s) = m

    val steps = for(stepId <- sumsSpec.indices) yield new ExecuteArea(sumAt + stepId) {
      val (stepWidth, stepLanes) = sumsSpec(stepId)
      // Generate the specification for ever adders of the current step
      val addersSpec = AdderAggregator(sourcesSpec, stepWidth, stepLanes)
      // Generate the hardware corresponding to every addersSpec
      val adders = addersSpec.map(_.craft(sourceToSignal.mapValues(stage(_)))).map(stage.insert(_))

      // Setup the iteration variables for the next step
      sourcesSpec = addersSpec.map(_.toSource()).toList
      for ((s, m) <- (sourcesSpec, adders).zipped) sourceToSignal(s) = m
//      println(addersSpec.mkString("\n"))
//      println("------------")
    }

    val writeback = new ExecuteArea(writebackAt) {
      import stage._
      assert(sourcesSpec.size == 1)
      val result = stage(sourceToSignal(sourcesSpec.head))
      val buffer = bufferedHigh.get generate new Area{
        val valid = RegNext(False) init (False) setWhen (isValid && !isReady && !isRemoved)
        val data = RegNext(result(XLEN, XLEN bits))
        haltIt(HIGH && !valid)
      }
      wb.payload := B(HIGH ? (if(bufferedHigh.get) buffer.data else result(XLEN, XLEN bits)) otherwise result(0, XLEN bits))
    }
  }
}
