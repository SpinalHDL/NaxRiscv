package naxriscv.prediction

import naxriscv.Fetch.SLICE_COUNT
import naxriscv.Global.PC_WIDTH
import spinal.core._
import spinal.lib.pipeline.Stageable

object Prediction extends AreaObject{
  val ALIGNED_BRANCH_VALID = Stageable(Bool())
  val ALIGNED_BRANCH_PC_NEXT = Stageable(UInt(PC_WIDTH bits))

  val WORD_BRANCH_VALID = Stageable(Bool())
  val WORD_BRANCH_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits))
  val WORD_BRANCH_PC_NEXT = Stageable(UInt(PC_WIDTH bits))
}