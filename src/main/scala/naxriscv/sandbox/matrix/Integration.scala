// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.sandbox.matrix

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
    eventCount   = 32
  )

  val waitTable = new WaitTable(wp, SlotSelectorContext(sp))
  val slotSelector = new SlotSelector(sp)
  slotSelector.io.slots <> waitTable.io.pop
  waitTable.io.events := slotSelector.io.schedules.map(_.event).reduce(_ | _) //Assume always ready

  val push = waitTable.io.push.toIo
  val squedules = slotSelector.io.schedules.toIo

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
Artix 7 -> 110 Mhz 753 LUT 808 FF
Artix 7 -> 145 Mhz 843 LUT 808 FF

Artix 7 -> 121 Mhz 357 LUT 488 FF
Artix 7 -> 169 Mhz 394 LUT 488 FF

Artix 7 -> 150 Mhz 665 LUT 812 FF
Artix 7 -> 209 Mhz 740 LUT 812 FF
 */