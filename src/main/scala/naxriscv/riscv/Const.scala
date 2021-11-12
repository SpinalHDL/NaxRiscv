package naxriscv.riscv
import spinal.core._

object Const {
  def funct7Range = 31 downto 25
  def rdRange = 11 downto 7
  def funct3Range = 14 downto 12
  def rs2Range = 24 downto 20
  def rs1Range = 19 downto 15
  def rs3Range = 31 downto 27
  def csrRange = 31 downto 20
  def rsRange(id : Int) = List(rs1Range, rs2Range,rs3Range)(id)
}

case class IMM(instruction  : Bits) extends Area{
  // immediates
  def i = instruction(31 downto 20)
  def h = instruction(31 downto 24)
  def s = instruction(31 downto 25) ## instruction(11 downto 7)
  def b = instruction(31) ## instruction(7) ## instruction(30 downto 25) ## instruction(11 downto 8)
  def u = instruction(31 downto 12) ## U"x000"
  def j = instruction(31) ## instruction(19 downto 12) ## instruction(20) ## instruction(30 downto 21)
  def z = instruction(19 downto 15)

  // sign-extend immediates
  def i_sext = B((19 downto 0) -> i(11)) ## i
  def h_sext = B((23 downto 0) -> h(7))  ## h
  def s_sext = B((19 downto 0) -> s(11)) ## s
  def b_sext = B((18 downto 0) -> b(11)) ## b ## False
  def j_sext = B((10 downto 0) -> j(19)) ## j ## False
}

