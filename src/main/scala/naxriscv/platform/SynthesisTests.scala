package naxriscv.platform

import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench._

object SynthesisTests extends App{
  //Demo
  def gen(width : Int) = new Rtl {
    override def getName(): String = s"gen$width"
    override def getRtlPath(): String = s"gen$width.v"

    SpinalVerilog({
      new Component {
//        val en = in Bool()
//        val din = in Bool()
//        val addr = in UInt(6 bits)
//        val dout = out Bool()

//        val reg = Reg(Bits(64 bits))
//        when(en){
//          reg := (reg ## din).resized
//        }
//        dout := reg(addr)

//        val en = in Bool()
//        val din = in Bits(2 bits)
//        val addr = in UInt(6 bits)
//        val dout = out Bool()
//
//        val reg = Vec.fill(2)(Reg(Bits(32 bits)))
//        when(en){
//          reg(0) := (reg(0) ## din(0)).resized
//          reg(1) := (reg(1) ## din(1)).resized
//        }
//        dout := (addr.lsb ? reg(0)(addr >> 1) |  reg(1)(addr >> 1))


//        val sel = in UInt(log2Up(width) bits)
//        val din = in Bits(width bits)
//        val dout = out(din(sel))

//        val sel = in UInt(log2Up(width*2) bits)
//        val din = in Bits(width bits)
//        val dout = out((din << width) >> sel)

        val sel = in UInt(log2Up(width) bits)
        val din = in Bits(width bits)
        val dout = out((din) >> sel)

      }.setDefinitionName(getRtlPath().split("\\.").head)
    })
  }



  val rtls = List(gen(106))

  val targets = XilinxStdTargets().take(2)
  Bench(rtls, targets)

}