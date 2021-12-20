package naxriscv.prediction

import naxriscv.Fetch.SLICE_COUNT
import naxriscv.Global.PC_WIDTH
import naxriscv.utilities.Service
import spinal.core._
import spinal.lib.pipeline.Stageable

object Prediction extends AreaObject{
  //Used by fetch based instruction to inform the aligner plugin about predictions done
  val WORD_BRANCH_VALID = Stageable(Bool())
  val WORD_BRANCH_SLICE = Stageable(UInt(log2Up(SLICE_COUNT) bits))
  val WORD_BRANCH_PC_NEXT = Stageable(UInt(PC_WIDTH bits))

  //Used by decoder based prediction to know the fetch based prediction modified the flow of future instructions
  val ALIGNED_BRANCH_VALID = Stageable(Bool())
  val ALIGNED_BRANCH_PC_NEXT = Stageable(UInt(PC_WIDTH bits))

  //Set by fetch prediction to propose conditional branch prediction, could be used by another fetch level prediction, or later on in decode to correct things
  val CONDITIONAL_TAKE_IT = Stageable(Bits(SLICE_COUNT bits))
}


trait HistoryUser extends Service{
  def historyWidthUsed : Int
}