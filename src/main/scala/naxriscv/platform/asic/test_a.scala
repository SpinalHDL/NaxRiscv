package naxriscv.platform.asic
import spinal.core._
import spinal.core.internals.{MemTopology, PhaseContext, PhaseMemBlackBoxingWithPolicy, PhaseNetlist}
import spinal.lib._

object SpinalSky130{
  object blackboxPolicy extends MemBlackboxingPolicy {
    override def translationInterest(topology: MemTopology): Boolean = topology.readsSync.nonEmpty || topology.readWriteSync.nonEmpty
    override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = {}
  }
  def apply() = {
    val c = SpinalConfig(mode = Verilog)
    c.addStandardMemBlackboxing(blackboxPolicy)
    c.phasesInserters += { phases =>
      val i = phases.lastIndexWhere(_.isInstanceOf[PhaseMemBlackBoxingWithPolicy])
      phases.insert(i+1, new PhaseNetlist {
        override def impl(pc: PhaseContext): Unit = {
          pc.walkComponents {
            case bb: Ram_1w_1rs => bb.parent.rework {
              val skyMaskWidth = 4
              val skyDataWidth = 32
              bb.setDefinitionName("sky130_sram_4kbyte_1r1w_32x1024_8_nocheck")
              bb.genericElements.clear()
              bb.io.wr.clk.setName("clk0")
              bb.io.wr.addr.setName("addr0")
              bb.io.wr.data.setName("din0")

              bb.io.wr.en.setName("csb0")
              val wrEn = bb.io.wr.en.getSingleDriver.get
              bb.io.wr.en.removeAssignments() := !wrEn

              skyMaskWidth match {
                case v if v > 0 => {
                  bb.io.wr.mask.setName("wmask0")
                  bb.io.wr.mask.setWidth(skyMaskWidth)

                  bb.wrMaskEnable match {
                    case false => bb.io.wr.mask.removeAssignments().setAll()
                  }
                }
              }

              bb.io.rd.clk.setName("clk1")
              bb.io.rd.addr.setName("addr1")
              bb.io.rd.data.setName("dout1")
              bb.io.rd.en.setName("csb1")
              val rdEn = bb.io.rd.en.getSingleDriver.get
              bb.io.rd.en.removeAssignments() := !rdEn

            }
            case _ =>
          }
        }
      })
    }

    c
  }
}

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
