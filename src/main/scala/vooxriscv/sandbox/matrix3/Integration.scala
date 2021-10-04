package vooxriscv.sandbox.matrix3

import spinal.core._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer
class Integration extends Component{
  val sp = SlotSelectorParameter(
    slotCount   = 32,
    eventCount  = 32,
    selWidth    = 2,
    schedules   = List.tabulate(2)(i => ScheduleParameter(32, i))
  )
  val wp = WaitTableParameter(
    slotCount    = 32,
    wayCount     = 2,
    eventCount   = 32,
    selCount = 2
  )

  val waitTable = new WaitTable(wp, SlotSelectorContext(sp))
  val slotSelector = new SlotSelector(sp)
  (slotSelector.io.slots, waitTable.io.pop).zipped.foreach{(s, m) =>
    s.arbitrationFrom(m)
    s.sel := m.sel
    s.selPriority := m.selPriority
    s.context := m.context
  }
  waitTable.io.events := slotSelector.io.schedules.map(_.event).zipWithIndex.map{case (v, i) => if(true) v else RegNext(v)}.reduce(_ | _) //Assume always ready

  val push = waitTable.io.push.toIo
  val schedules = slotSelector.io.schedules.toIo
  val priority = slotSelector.io.priority.toIo
  val priority2 = waitTable.io.priority.toIo

  schedules.map(_.ready.setAsDirectionLess() := True)

}

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Integration)))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}

/*
first based
Artix 7 -> 145 Mhz 505 LUT 1646 FF
Artix 7 -> 256 Mhz 537 LUT 1646 FF

age based
Artix 7 -> 117 Mhz 620 LUT 1742 FF
Artix 7 -> 202 Mhz 677 LUT 1742 FF

age based opt
Artix 7 -> 125 Mhz 645 LUT 1932 FF
Artix 7 -> 220 Mhz 723 LUT 1932 FF

Id mux
Artix 7 -> 93 Mhz 653 LUT 1932 FF
Artix 7 -> 154 Mhz 690 LUT 1932 FF

 */