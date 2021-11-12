package naxriscv.frontend

import spinal.core._
import spinal.lib.NoData
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer
class IssueQueueTop(wp : IssueQueueParameter) extends Component{
  val IssueQueue = new IssueQueue(wp, new NoData)

  IssueQueue.io.events := IssueQueue.io.schedules.map(_.event).zipWithIndex.map{case (v, i) => if(true) v else RegNext(v)}.reduce(_ | _) //Assume always ready

  val push = IssueQueue.io.push.toIo
  val schedules = IssueQueue.io.schedules.toIo

  schedules.map(_.ready.setAsDirectionLess() := True)
}

object IssueQueueTopSynthBench extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new IssueQueueTop(
    IssueQueueParameter(
      slotCount    = 32,
      wayCount     = 2,
      selCount = 2,
      schedules = List.tabulate(2)(i => ScheduleParameter(1, 0, i))
    )
  ).setDefinitionName("miaou32"))))
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new IssueQueueTop(
    IssueQueueParameter(
      slotCount    = 48,
      wayCount     = 2,
      selCount = 2,
      schedules = List.tabulate(2)(i => ScheduleParameter(1, 0, i))
    )
  ).setDefinitionName("miaou48"))))
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new IssueQueueTop(
    IssueQueueParameter(
      slotCount    = 64,
      wayCount     = 2,
      selCount = 2,
      schedules = List.tabulate(2)(i => ScheduleParameter(1, 0, i))
    )
  ).setDefinitionName("miaou64"))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}


/*
INFO: [Common 17-206] Exiting Vivado at Sat Oct 30 06:43:28 2021...
miaou32 ->
Artix 7 -> 128 Mhz 357 LUT 1018 FF
Artix 7 -> 200 Mhz 451 LUT 1018 FF
miaou48 ->
Artix 7 -> 117 Mhz 617 LUT 1898 FF
Artix 7 -> 164 Mhz 751 LUT 1898 FF
miaou64 ->
Artix 7 -> 115 Mhz 962 LUT 3034 FF
Artix 7 -> 155 Mhz 1136 LUT 3034 FF
 */