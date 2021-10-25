package naxriscv

import naxriscv.frontend.Frontend
import spinal.core.ScopeProperty
import naxriscv.pipeline.Stageable
import spinal.core._

object Global  extends Area{
  setName("Global")
  val PHYSICAL_WIDTH = ScopeProperty[Int]
  def VIRTUAL_WIDTH = PHYSICAL_WIDTH.get //for now

  val PC   = Stageable(UInt(Global.VIRTUAL_WIDTH bits))

  def ROB_ROWS = Frontend.DECODE_COUNT
  val ROB_SIZE = ScopeProperty[Int]
  def ROB_ID_WIDTH = log2Up(ROB_SIZE.get)
  val TRAP_CAUSE_WIDTH = 4 //TODO
  val XLEN = ScopeProperty[Int]
}

