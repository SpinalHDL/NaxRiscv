package naxriscv.sandbox

import spinal.core._
import spinal.lib.blackbox.xilinx.s7.FDRE
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

object Stats extends App{
  val population = 16
  val spaceSize = 256
//  val population = 23
//  val spaceSize = 365

  var prob = 1.0
  for(i <- 0 until population){
    prob *= 1.0*(spaceSize-i)/spaceSize
  }
  println(1.0*(spaceSize-1)/spaceSize)
  println(prob)


  val loads = 16
  val stores = 16
  println(Math.pow(1-1.0/spaceSize, stores))
  println(Math.pow(Math.pow(1-1.0/spaceSize, stores),loads))
}


object SyntTest extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(new Component {
    val inputs = in Bits(5 bits)
    val r0 = out(RegNext(inputs.orR))
    val r1 = out(RegNext(inputs.andR))
  }))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}



object SyntTest2 extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(new Component {
//    val rUp, event, col0, col1 = in Bool()
//    val r0 = out(Reg(Bool))
//    val r1 = out(Reg(Bool))
//
//    when(event){
//      r0 := False
//      r1 := False
//    } otherwise {
//      r0 := rUp && col0 || r0
//      r1 := r0 && col1 || r1
//    }

    val  event = in Bool()
    val logic = for(i <- 0 until 50) yield {
      val rUp, col0, col1 = in Bool()

      val r0, r1 = FDRE()
      r0.R := event
      r1.R := event
      r0.CE := True
      r1.CE := True

      val o0 = r0.Q.toIo()
      val o1 = r1.Q.toIo()

      r0.D := rUp && col0 || r0.Q
      r1.D := r0.Q && col1 || r1.Q
    }
  }))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}

//LUT6_2 #(
//.INIT(64’h0000000000000000) // Specify LUT Contents
//) LUT6_2_inst (
//.O6(O6), // 1-bit LUT6 output
//.O5(O5), // 1-bit lower LUT5 output
//.I0(I0), // 1-bit LUT input
//.I1(I1), // 1-bit LUT input
//.I2(I2), // 1-bit LUT input
//.I3(I3), // 1-bit LUT input
//.I4(I4), // 1-bit LUT input
//.I5(I5) // 1-bit LUT input (fast MUX select only available to O6 output)
//);



object SyntTest3 extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Component {
    val LSLEN = 64
    val sizeMax = log2Up(LSLEN/8)
    val rspSize = in UInt(2 bits)
    val rspShifted = in Bits(64 bits)
    val rspUnsigned = in Bool()
    val rspFormated = out(rspSize.muxListDc((0 to sizeMax).map{i =>
      val off = (1 << i) * 8
      i -> B((LSLEN - 1 downto off) -> (rspShifted(off-1) && !rspUnsigned), (off-1 downto 0) -> rspShifted(off-1 downto 0))
    }))
  })))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}


object RamPorts extends App{
  def xor(w : Int, r : Int) = w*(w-1)+r*w

  for(w <- List(2,3,4); r <- List(2,4,6)){
    println(s"w=$w r=$r XOR ${xor(w,r)}")
  }

}