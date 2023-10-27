package naxriscv.platform.asic
import spinal.core._
import spinal.core.internals.{MemTopology, PhaseContext, PhaseMemBlackBoxingWithPolicy, PhaseNetlist}
import spinal.lib._



class test_a extends Component{
//  val io = new Bundle {
//    val a = in UInt (8 bits)
//    val x = out Bool()
//  }
//  val reduced = io.a.orR
//  val reg = RegNext(reduced)
//  val delayed = Delay(reg, 100)
//  io.x := reg

  val io = new Bundle{
    val push = slave Stream(UInt(32 bits))
    val pop = master Stream(UInt(32 bits))
  }

  val fifo = StreamFifo(UInt(32 bits), 1024)
  io.push <> fifo.io.push
  io.pop <> fifo.io.pop

  setDefinitionName("top")
}

object TestAGen extends App{
  SpinalSky130()(new test_a)
}
