package naxriscv.sandbox.cam2

import spinal.core._
import spinal.lib._
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
    triggerCount = 2,
    eventPorts   = 2,
    eventCount   = 32
  )

  val waitTable = new WaitTable(wp, SlotSelectorContext(sp))
  val slotSelector = new SlotSelector(sp)
  slotSelector.io.slots <> waitTable.io.pop
  waitTable.io.events <> RegNext(Vec(slotSelector.io.schedules.map(_.event))) //Assume always ready TODO remove reg

  val push = waitTable.io.push.toIo
  val squedules = slotSelector.io.schedules.toIo
  val priority = slotSelector.io.priority.toIo

  squedules.map(_.ready.setAsDirectionLess() := True)

}

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Integration)))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}

/*
Artix 7 -> 161 Mhz 564 LUT 700 FF
Artix 7 -> 238 Mhz 612 LUT 700 FF
 */