package naxriscv.utilities


import naxriscv.Global
import spinal.core._
import spinal.lib._

case class DivCmd(width : Int) extends Bundle{
  val a,b = UInt(width bits)
}

case class DivRsp(width : Int) extends Bundle{
  val result = UInt(width+1 + 2 bits)
  val remain = UInt(width+1 bits)
}

case class DivRadix4(val width : Int) extends Component {
  assert(width % 2 == 0)
  val io = new Bundle{
    val flush = in Bool()
    val cmd = slave Stream(DivCmd(width))
    val rsp = master Stream(DivRsp(width))
  }

  val iterations = width/2
  val counter = Reg(UInt(log2Up(iterations) bits))
  val busy = RegInit(False) clearWhen(io.rsp.fire)
  val done = RegInit(False) setWhen(busy && counter === iterations-1) clearWhen(io.rsp.fire)

  val shifter = Reg(UInt(width bits))
  val numerator = Reg(UInt(width bits))
  val result = Reg(UInt(width bits))

  val div1, div3 = Reg(UInt(width+2 bits))
  val div2 = div1 |<< 1

  val shifted = shifter @@ numerator.takeHigh(2).asUInt
  val sub1 = shifted -^ div1
  val sub2 = shifted -^ div2
  val sub3 = shifted -^ div3

  io.rsp.valid := done
  io.rsp.result := result.resized
  io.rsp.remain := shifter.resized
  io.cmd.ready := !busy

  when(!done){
    counter := counter + 1
    val sel = CombInit(shifted)
    result := result |<< 2
    when(!sub1.msb){
      sel := sub1.resized
      result(1 downto 0) := 1
    }
    when(!sub2.msb){
      sel := sub2.resized
      result(1 downto 0) := 2
    }
    when(!sub3.msb){
      sel := sub3.resized
      result(1 downto 0) := 3
    }
    shifter := sel.resized
    numerator := numerator |<< 2
  }

  val sliceCount = 3
  val shiftWidth = width/(sliceCount+1)
  val slices = io.cmd.a.subdivideIn(sliceCount+1 slices).tail
  val slicesZero = slices.map(_ === 0)
  val shiftSel = B((0 until sliceCount).map(i => slicesZero.drop(i).andR))
  val sel = OHToUInt(OHMasking.firstV2(True ## shiftSel))
  when(!busy){
    busy   := io.cmd.valid
    div1   := io.cmd.b.resized
    div3   := io.cmd.b +^ (io.cmd.b << 1)
    result := (default -> (io.cmd.b === 0))
    switch(sel) {
      for (i <- sliceCount downto 0) is(i) {
        val shift = width - ((i + 1) * shiftWidth)
        counter := shift / 2
        shifter := U(io.cmd.a.takeHigh(shift)).resized
        numerator := io.cmd.a |<< shift
      }
    }
  }

  when(io.flush){
    done := False
    busy := False
  }
}


object DivRadix4Tester extends App{
  import spinal.core.sim._
  SimConfig.withWave.compile(new DivRadix4(16)).doSim(seed = 52){dut =>
    dut.clockDomain.forkStimulus(10)
    dut.io.cmd.valid #= false
    dut.io.rsp.ready #= true
    dut.clockDomain.waitSampling()
    for(i <- 0 until 1000) {
      dut.io.cmd.valid #= true
      val a = dut.io.cmd.a.randomizedInt()
      val b = dut.io.cmd.b.randomizedInt()
      dut.io.cmd.a #= a
      dut.io.cmd.b #= b
      dut.clockDomain.waitSampling()
      dut.io.cmd.valid #= false
      dut.clockDomain.waitSamplingWhere(dut.io.rsp.valid.toBoolean)
      assert(dut.io.rsp.result.toInt == a / b)
      assert(dut.io.rsp.remain.toInt == a % b)
      dut.clockDomain.waitSampling()
    }
    simSuccess()
  }
}