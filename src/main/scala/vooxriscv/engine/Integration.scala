package vooxriscv.engine

import spinal.core._
import spinal.lib.NoData
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer
class Integration(wp : IssueQueueParameter) extends Component{
  val IssueQueue = new IssueQueue(wp, new NoData)

  IssueQueue.io.events := IssueQueue.io.schedules.map(_.event).zipWithIndex.map{case (v, i) => if(true) v else RegNext(v)}.reduce(_ | _) //Assume always ready

  val push = IssueQueue.io.push.toIo
  val schedules = IssueQueue.io.schedules.toIo

  schedules.map(_.ready.setAsDirectionLess() := True)
}

object IntegrationSynthBench extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Integration(
    IssueQueueParameter(
      slotCount    = 32,
      wayCount     = 2,
      robEntries = 64,
      selCount = 2,
      schedules = List.tabulate(2)(i => ScheduleParameter(32, i))
    )
  ).setDefinitionName("miaou32"))))
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Integration(
    IssueQueueParameter(
      slotCount    = 48,
      wayCount     = 2,
      robEntries = 64,
      selCount = 2,
      schedules = List.tabulate(2)(i => ScheduleParameter(48, i))
    )
  ).setDefinitionName("miaou48"))))
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new Integration(
    IssueQueueParameter(
      slotCount    = 64,
      wayCount     = 2,
      robEntries = 64,
      selCount = 2,
      schedules = List.tabulate(2)(i => ScheduleParameter(64, i))
    )
  ).setDefinitionName("miaou64"))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}
