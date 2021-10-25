package naxriscv.sandbox.syntax

import naxriscv.interfaces.RegfileSpec
import spinal.core._

//class Spec {
//
//  val intRegfile = new RegfileConfig {
//    override def size = 32
//    override def width = ???
//    override def x0AlwaysZero = true
//  }
//
//  val INT_RS1 = new RfRead{
//    def decode(i : Bits) = i(15 downto 10)
//    def rf = intRegfile
//  }
//  val INT_RD = new RfWrite{
//    def decode(i : Bits) = i(9 downto 5)
//    def rf = intRegfile
//  }
//
//  def TypeR(key : MaskedLiteral): Unit = Instruction(
//    key = key,
//    ressources = List(INT_RS1, INT_RS2, INT_RD),
//  )
//
//  val ADD = TypeR(M"010101010000")
//
//  class FuAluPlugin{
//    val setup = create early new Area {
//      decoder.add(
//        fu = this,
//        instruction = ADD
//      )
//    }
//  }
//
//  backend.addFu(new FuAluPlugin)
//}
//



//backend = List(
//ALU_OP -> ADD
//)