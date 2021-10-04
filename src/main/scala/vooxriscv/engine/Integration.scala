package vooxriscv.engine

import spinal.core._
import spinal.lib.NoData
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer
class Integration extends Component{

  val wp = WaitTableParameter(
    slotCount    = 32,
    wayCount     = 2,
    robEntries = 64,
    selCount = 2,
    schedules = List.tabulate(2)(i => ScheduleParameter(32, i))
  )

  val waitTable = new WaitTable(wp, new NoData)

  waitTable.io.events := waitTable.io.schedules.map(_.event).zipWithIndex.map{case (v, i) => if(true) v else RegNext(v)}.reduce(_ | _) //Assume always ready

  val push = waitTable.io.push.toIo
  val schedules = waitTable.io.schedules.toIo
//  val event = waitTable.io.events.toIo
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
