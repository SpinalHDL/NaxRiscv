package naxriscv.riscv

import naxriscv.riscv.IntRegFile.TypeU
import spinal.core._

object Rvi{
  import IntRegFile._

  val ADD   = TypeR(M"0000000----------000-----0110011")
  val ADDI  = TypeI(M"-----------------000-----0010011")
  def LUI   = TypeU(M"-------------------------0110111")
  def AUIPC = TypeU(M"-------------------------0010111")


  val MUL  = TypeR(M"0000001----------000-----0110011")
  def BEQ  =  TypeB(M"-----------------000-----1100011")
  def BNE  =  TypeB(M"-----------------001-----1100011")
  def BLT  =  TypeB(M"-----------------100-----1100011")
  def BGE  =  TypeB(M"-----------------101-----1100011")
  def BLTU =  TypeB(M"-----------------110-----1100011")
  def BGEU =  TypeB(M"-----------------111-----1100011")
//  def JALR =  M"-----------------000-----1100111"
//  def JAL  =  M"-------------------------1101111"
}