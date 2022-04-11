package naxriscv.frontend

import spinal.core._
import spinal.lib._

case class IssueQueueParameter(slotCount : Int,
                               wayCount : Int,
                               selCount : Int,
                               schedules : Seq[ScheduleParameter]){
  assert(slotCount % wayCount == 0)
  val lineCount = slotCount/wayCount
  val eventType = HardType(Bits(slotCount bits))
}

case class IssueQueuePush[T <: Data](p : IssueQueueParameter, contextType : HardType[T]) extends Bundle{
  val slots = Vec.fill(p.wayCount)(IssueQueuePushSlot(p, contextType))
}

case class IssueQueuePushSlot[T <: Data](p : IssueQueueParameter, contextType : HardType[T]) extends Bundle{
  val event = p.eventType()
  val sel = Bits(p.selCount bits)
  val context = contextType()
}

case class Schedule(p : ScheduleParameter, slotCount : Int) extends Bundle{
  val event = Bits(slotCount/p.eventFactor bits)
}

case class ScheduleParameter(eventFactor : Int,
                             eventOffset : Int,
                             selId : Int){
//  val eventType = HardType(Bits(eventCount bits))
}

case class IssueQueueIo[T <: Data](p : IssueQueueParameter, slotContextType : HardType[T]) extends Bundle{
  val clear = in Bool()
  val events = KeepAttribute(in(p.eventType()))
  val push = slave Stream(IssueQueuePush(p, slotContextType))
  val schedules = Vec(p.schedules.map(sp => master Stream(Schedule(sp, p.slotCount))))
  val contexts = out(Vec.fill(p.slotCount)(slotContextType()))
  val usage = out Bits(p.slotCount bits)
}



class IssueQueue[T <: Data](val p : IssueQueueParameter, slotContextType : HardType[T]) extends Component{
  val io = IssueQueueIo(p, slotContextType)

  val running = RegNext(True) init(False)
  val clear = io.clear || !running

  val lines = for(line <- 0 until p.lineCount) yield new Area{
    val ways = for(way <- 0 until p.wayCount) yield new Area{
      val fire = Bool()
      val priority = line*p.wayCount+way
      val sel = Reg(Bits(p.selCount bits)) //Not zero when the given slot is valid
      val selComb = CombInit(sel)
      val triggers = Reg(Bits(priority + 1 bits)) //The msb bit is use ensure the slot is keept until it did the wakeup
      val ready = triggers(priority - 1 downto 0) === 0

      sel := selComb
      when(fire){ selComb := 0 }

      val context = Reg(slotContextType())
      io.contexts(line*p.wayCount + way) := context

      io.usage(priority) := sel =/= 0 || triggers.msb
    }
  }


  val compaction = new Area {
    val moveIt = io.push.fire

    when(moveIt) {
      //Compact most of the table
      for (lineId <- 0 to p.lineCount - 2) {
        for (way <- 0 until p.wayCount) {
          val wSrc = lines(lineId + 1).ways(way)
          val wDst = lines(lineId).ways(way)
          wDst.context := wSrc.context
          wDst.triggers := wSrc.triggers >> p.wayCount
          wDst.sel := wSrc.selComb
        }
      }

      //Push in the table
      for (way <- 0 until p.wayCount) {
        val wSrc = io.push.slots(way)
        val wDst = lines.last.ways(way)
        wDst.context := wSrc.context
        wDst.triggers := wSrc.event.resized
        wDst.sel := wSrc.sel
      }
    }
  }

  val event = new Area{
    val moved = !compaction.moveIt ? io.events | (io.events |>> p.wayCount)
    val value = clear ? B(p.slotCount bits, default -> true) | moved //TODO each entry could track if it is a active one or not, this would relax the timings
  }

  //Apply io events
  for(eventId <- 0 until p.slotCount) {
    when(event.value(eventId)){
      lines.foreach(_.ways.filter(_.priority >= eventId).foreach(_.triggers(eventId) := False))
    }
  }

  val slots = lines.map(_.ways).flatten
  val selector = for((schedule, scheduleId) <- io.schedules.zipWithIndex) yield new Area{
    val sp = p.schedules(scheduleId)
    val slotsValid = B((sp.eventOffset until p.slotCount by sp.eventFactor).map(i => slots(i).ready && slots(i).sel(sp.selId)))
    val selOh = OHMasking.firstV2(slotsValid, firstOrder =  LutInputs.get)

    schedule.valid := slotsValid.orR && running
    schedule.event := selOh
    val ready = schedule.ready
  }

  for((slot, slotIdx) <- slots.zipWithIndex){
    slot.fire := selector.filter(s => slotIdx % s.sp.eventFactor == s.sp.eventOffset).map(s => s.ready && s.selOh(slotIdx/s.sp.eventFactor)).orR
  }

  when(clear){
    lines.foreach(_.ways.foreach(_.sel := 0))
  }

  //Pessimistic ready
  val line0Ready = lines(0).ways.map(s => !s.triggers.msb && s.sel === 0).andR
  val line1Ready = lines(1).ways.map(s => !s.triggers.msb && s.sel === 0).andR
  val readyReg = RegInit(False)
  readyReg := compaction.moveIt ? line1Ready | line0Ready
  io.push.ready := readyReg

//  io.push.ready := lines.head.ways.map(s => !s.triggers.msb && s.sel === 0).andR

}
