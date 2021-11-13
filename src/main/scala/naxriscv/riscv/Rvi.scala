package naxriscv.riscv

import spinal.core._

object Rvi{
  import IntRegFile.{TypeR, TypeI, TypeB}

  val ADD  = TypeR(M"0000000----------000-----0110011")
  val ADDI = TypeI(M"-----------------000-----0010011")
  val MUL  = TypeR(M"0000001----------000-----0110011")
  def BEQ =  TypeB(M"-----------------000-----1100011")
}