package naxriscv.riscv

import naxriscv.Global
import naxriscv.interfaces._
import spinal.core.{AreaObject, MaskedLiteral}

object IntRegFile extends RegfileSpec with AreaObject {
  override def sizeArch = 32
  override def width = Global.XLEN
  override def x0AlwaysZero = true
  override def getName() = "integer"

  def TypeR(key : MaskedLiteral) = SingleDecoding(
    key = key,
    resources = List(RS1, RS2, RD).map(this -> _)
  )
  def TypeI(key : MaskedLiteral) = SingleDecoding(
    key = key,
    resources = List(RS1, RD).map(this -> _)
  )
  def TypeB(key : MaskedLiteral) = SingleDecoding(
    key = key,
    resources = List(RS1, RS2).map(this -> _) :+ PC_READ :+ INSTRUCTION_SIZE
  )
}


