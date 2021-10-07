package vooxriscv.engine

import spinal.core._
import spinal.lib._

object DecoderWidth extends ScopeProperty[Int]
object InstructionWidth extends ScopeProperty[Int]{
  def craft() = Bits(this.get bits)
}


//case class DecoderParameter(decoderWidth : Int,
//                            instructionWidth : Int)  {
//
//}

case class DecompressedInstruction() extends Bundle {
  val valid = Bool()
  val instruction = InstructionWidth.craft()
}


case class DecoderIo() extends Bundle {
  val instructions = slave Stream(Vec.fill(DecoderWidth)(DecompressedInstruction()))
}

case class Decoder() extends Component {

}
