package naxriscv

import naxriscv.pipeline.Stageable
import spinal.core._


object ROB extends AreaObject{
  def ROWS = Frontend.DECODE_COUNT
  val SIZE = ScopeProperty[Int]
  def ID_WIDTH = log2Up(SIZE.get)
  val ID_TYPE = HardType(UInt(ID_WIDTH bits))
  def lineRange = ID_WIDTH-1 downto log2Up(ROWS)
}

object Global extends AreaObject {
  val PHYSICAL_WIDTH = ScopeProperty[Int]
  def VIRTUAL_WIDTH = PHYSICAL_WIDTH.get //for now

  val PC = Stageable(UInt(Global.VIRTUAL_WIDTH bits))

  val COMMIT_COUNT = ScopeProperty[Int]


  val TRAP_CAUSE_WIDTH = 4 //TODO
  val XLEN = ScopeProperty[Int]
}


object Frontend extends AreaObject {
  val RVC = ScopeProperty[Boolean]
  val FETCH_DATA_WIDTH = ScopeProperty[Int]
  val INSTRUCTION_WIDTH = ScopeProperty[Int]
  val BRANCH_HISTORY_WIDTH = ScopeProperty[Int]
  val DECODE_COUNT = ScopeProperty[Int]
  def FETCH_COUNT = DECODE_COUNT.get
  def DISPATCH_COUNT = DECODE_COUNT.get

  def SLICE_WIDTH = if(RVC) 16 else 32
  def SLICE_BYTES = if(RVC) 2 else 4
  def SLICE_COUNT = FETCH_DATA_WIDTH/SLICE_WIDTH

  val WORD = Stageable(Bits(FETCH_DATA_WIDTH bits))
  val MASK = Stageable(Bits(FETCH_DATA_WIDTH/SLICE_WIDTH bits))

  val DISPATCH_MASK = Stageable(Bool())

  val MASK_ALIGNED = Stageable(Bool())
  val INSTRUCTION_ALIGNED = Stageable(Bits(INSTRUCTION_WIDTH bits))
  val INSTRUCTION_DECOMPRESSED = Stageable(Bits(INSTRUCTION_WIDTH bits))
  val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))


  val FETCH_PC_PHYSICAL  = Stageable(UInt(Global.VIRTUAL_WIDTH bits))
  val FETCH_PC_VIRTUAL   = Stageable(UInt(Global.VIRTUAL_WIDTH bits))

  val ROB_ID = Stageable(ROB.ID_TYPE)
}

