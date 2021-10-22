package naxriscv.frontend

import spinal.core._
import naxriscv.Global
import naxriscv.pipeline._


object Frontend extends Area{
  this.setName("FETCH")

  val RVC = ScopeProperty[Boolean]
  val FETCH_DATA_WIDTH = ScopeProperty[Int]
  val INSTRUCTION_WIDTH = ScopeProperty[Int]
  val BRANCH_HISTORY_WIDTH = ScopeProperty[Int]
  val DECODE_COUNT = ScopeProperty[Int]
  def FETCH_COUNT = DECODE_COUNT.get

  def SLICE_WIDTH = if(RVC) 16 else 32
  def SLICE_BYTES = if(RVC) 2 else 4
  def SLICE_COUNT = FETCH_DATA_WIDTH/SLICE_WIDTH

  val WORD = Stageable(Bits(FETCH_DATA_WIDTH bits))
  val MASK = Stageable(Bits(FETCH_DATA_WIDTH/SLICE_WIDTH bits))


  val INSTRUCTION_ALIGNED = Stageable(Vec.fill(DECODE_COUNT)(Bits(INSTRUCTION_WIDTH bits)))
  val INSTRUCTION_DECOMPRESSED = Stageable(Vec.fill(DECODE_COUNT)(Bits(INSTRUCTION_WIDTH bits)))
  val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))


  val FETCH_PC_PHYSICAL   = Stageable(UInt(Global.VIRTUAL_WIDTH bits))
  val FETCH_PC_VIRTUAL   = Stageable(UInt(Global.VIRTUAL_WIDTH bits))

  val USE_RS1, USE_RS2 = Stageable(Bool())
}

