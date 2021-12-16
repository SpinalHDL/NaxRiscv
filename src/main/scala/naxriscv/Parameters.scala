package naxriscv

import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.pipeline.Stageable
import naxriscv.utilities._

import scala.collection.mutable


object ROB extends AreaObject{
  def COLS = Frontend.DISPATCH_COUNT
  def LINES = SIZE/COLS
  val SIZE = NaxParameter[Int]
  def ID_WIDTH = log2Up(SIZE.get)
  val ID = Stageable(UInt(ID_WIDTH bits))
  def lineRange = ID_WIDTH-1 downto log2Up(COLS)
}

object Global extends AreaObject {
//  val PHYSICAL_WIDTH = NaxParameter[Int]
//  def VIRTUAL_WIDTH = PHYSICAL_WIDTH.get //for now

//  val PHYSICAL_ADDRESS = Stageable(UInt(PHYSICAL_WIDTH bits))
//  val VIRTUAL_ADDRESS = Stageable(UInt(VIRTUAL_WIDTH bits))
//  val PAGE_OFFSET = Stageable(UInt(12 bits))

//  val wordWidth = Global.XLEN.get
//  val wordBytes = wordWidth/8
//  val pageOffsetRange = 11 downto log2Up(wordBytes)
//  val pageNumberRange = Global.XLEN.get downto 12
//  val pageOffsetWidth = pageOffsetRange.size
//  val pageNumberWidth = pageNumberRange.size


  val COMMIT_COUNT = NaxParameter[Int]


  val TRAP_CAUSE_WIDTH = 4 //TODO
  val XLEN = NaxParameter[Int]
}

object Fetch extends AreaObject{
  val RVC = NaxParameter[Boolean]
  val FETCH_DATA_WIDTH = NaxParameter[Int]
  def SLICE_WIDTH = if(RVC) 16 else 32
  def SLICE_BYTES = if(RVC) 2 else 4
  def SLICE_COUNT = FETCH_DATA_WIDTH/SLICE_WIDTH
  val INSTRUCTION_SLICE_COUNT = Stageable(UInt(if(RVC) 1 bits else 0 bits)) // minus one => RVC => 0, normal => 1

  def SLICE_RANGE_LOW = if (RVC) 1 else 2
  def SLICE_RANGE = (SLICE_RANGE_LOW + log2Up(SLICE_COUNT) - 1) downto SLICE_RANGE_LOW

  val WORD = Stageable(Bits(FETCH_DATA_WIDTH bits))
  val INSTRUCTION_WIDTH = NaxParameter[Int]
}


object Frontend extends AreaObject {
  val BRANCH_HISTORY_WIDTH = NaxParameter[Int]
  val DECODE_COUNT = NaxParameter[Int]
  def FETCH_COUNT = DECODE_COUNT.get
  def DISPATCH_COUNT = DECODE_COUNT.get

  val DISPATCH_MASK = Stageable(Bool())

  val MASK_ALIGNED = Stageable(Bool())
  val INSTRUCTION_ALIGNED = Stageable(Bits(Fetch.INSTRUCTION_WIDTH bits))
  val INSTRUCTION_DECOMPRESSED = Stageable(Bits(Fetch.INSTRUCTION_WIDTH bits))
  val MICRO_OP = Stageable(Bits(Fetch.INSTRUCTION_WIDTH bits))
  val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))
}


