package naxriscv

import naxriscv.Fetch.INSTRUCTION_SLICE_COUNT
import naxriscv.Global.{PC}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.pipeline.Stageable
import naxriscv.utilities._
import spinal.core.fiber.Handle

import scala.collection.mutable


object ROB extends AreaObject{
  def COLS = Frontend.DISPATCH_COUNT
  def LINES = SIZE/COLS
  val SIZE = NaxParameter[Int]
  def ID_WIDTH = log2Up(SIZE.get)
  val ID = Stageable(UInt(ID_WIDTH bits))
  val MSB = Stageable(UInt(1 bits) //Extra ID bit which allow to figure out ordering by a substractor
  def lineRange = ID_WIDTH-1 downto log2Up(COLS)
}

object Global extends AreaRoot {
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


//  val TRAP_CAUSE_WIDTH = NaxParameter[Handle[Int]]
  val XLEN = NaxParameter[Int]
  val RVC = NaxParameter[Boolean]
  val RVF = NaxParameter[Boolean].setDefault(false)
  val RVD = NaxParameter[Boolean].setDefault(false)
  val RV_DEBUG = NaxParameter[Boolean].setDefault(false)
  def FLEN : Int = RVD.get.toInt*64 max RVF.get.toInt*32
  def LSLEN : Int = XLEN.get max FLEN

  val PC_WIDTH = NaxParameter[Int]
  val PC = Stageable(UInt(PC_WIDTH bits))
  val PC_TRANSLATED = Stageable(UInt(PHYSICAL_WIDTH bits))


  val PHYSICAL_WIDTH = NaxParameter[Int]
  val VIRTUAL_WIDTH  = NaxParameter[Int]
  val VIRTUAL_EXT_WIDTH  = NaxParameter[Int]

  val TVAL_WIDTH = NaxParameter[Int]
}

object Fetch extends AreaObject{
  val FETCH_DATA_WIDTH = NaxParameter[Int]
  def SLICE_WIDTH = if(Global.RVC) 16 else 32
  def SLICE_BYTES = if(Global.RVC) 2 else 4
  def SLICE_COUNT = FETCH_DATA_WIDTH/SLICE_WIDTH
  val INSTRUCTION_SLICE_COUNT = Stageable(UInt(if(Global.RVC) 1 bits else 0 bits)) // minus one => RVC => 0, normal => 1

  def SLICE_RANGE_LOW = if (Global.RVC) 1 else 2
  def SLICE_RANGE = (SLICE_RANGE_LOW + log2Up(SLICE_COUNT) - 1) downto SLICE_RANGE_LOW

  val WORD = Stageable(Bits(FETCH_DATA_WIDTH bits))
  val WORD_FAULT = Stageable(Bool())
  val WORD_FAULT_PAGE = Stageable(Bool())
  val INSTRUCTION_WIDTH = NaxParameter[Int]

  val FETCH_PC  = Stageable(PC)
  val FETCH_PC_INC  = Stageable(PC)
}


object Frontend extends AreaObject {
  val DECODE_COUNT = NaxParameter[Int]
  def FETCH_COUNT = DECODE_COUNT.get
  def DISPATCH_COUNT = DECODE_COUNT.get

  val DECODED_MASK = Stageable(Bool())
  val DISPATCH_MASK = Stageable(Bool())

  val MASK_ALIGNED = Stageable(Bool())
  val INSTRUCTION_ALIGNED = Stageable(Bits(Fetch.INSTRUCTION_WIDTH bits))
  val INSTRUCTION_DECOMPRESSED = Stageable(Bits(Fetch.INSTRUCTION_WIDTH bits))
  val INSTRUCTION_ILLEGAL = Stageable(Bool())
  val MICRO_OP = Stageable(Bits(Fetch.INSTRUCTION_WIDTH bits))

  val FETCH_FAULT = Stageable(Bool())
  val FETCH_FAULT_PAGE = Stageable(Bool())
  val FETCH_FAULT_SLICE = Stageable(INSTRUCTION_SLICE_COUNT)
}


