package naxriscv.engine

import spinal.core._
import spinal.lib._
import naxriscv.sandbox.matrix3.ScheduleParameter

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
  val events = KeepAttribute(in(p.eventType()))
  val push = slave Stream(IssueQueuePush(p, slotContextType))
  val schedules = Vec(p.schedules.map(sp => master Stream(Schedule(sp, p.slotCount))))
}



class IssueQueue[T <: Data](val p : IssueQueueParameter, slotContextType : HardType[T]) extends Component{
  val io = IssueQueueIo(p, slotContextType)

  case class IssueQueuePop() extends Bundle{
    val sel = Bits(p.selCount bits)
    val context = slotContextType()
  }

 //TODO event shift while compacting
  val lines = for(line <- 0 until p.lineCount) yield new Area{
    val ways = for(way <- 0 until p.wayCount) yield new Area{
      val fire = Bool()
      val priority = line*p.wayCount+way
      val sel = Reg(Bits(p.selCount bits))
      val selComb = CombInit(sel)
      val context = Reg(slotContextType())
      val triggers = Reg(Bits(priority + 1 bits)) //The msb bit is use to encore if the instruction will generate event
      val ready = triggers(priority - 1 downto 0) === 0

      when(fire){ selComb := 0 }
      sel := selComb
    }
  }



  io.push.ready := lines.head.ways.map(!_.triggers.msb).andR

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

  //Apply io events
  for(eventId <- 0 to p.slotCount-p.wayCount) {
    when(compaction.moveIt ?  (if(eventId >= p.slotCount - p.wayCount) False else io.events(eventId+p.wayCount)) | io.events(eventId)){
      lines.foreach(_.ways.filter(_.priority > eventId).foreach(_.triggers(eventId) := False))
    }
  }

  val slots = lines.map(_.ways).flatten
  val selector = for((schedule, scheduleId) <- io.schedules.zipWithIndex) yield new Area{
    val sp = p.schedules(scheduleId)
    val slotsValid = (sp.eventOffset until p.slotCount by sp.eventFactor).map(i => slots(i).ready && slots(i).sel(sp.selId)).asBits()
    val selOh = OHMasking.firstV2(slotsValid, firstOrder =  LutInputs.get/2)

    schedule.valid := slotsValid.orR
    schedule.event := selOh
  }

  for((slot, slotIdx) <- slots.zipWithIndex){
    slot.fire := selector.filter(s => slotIdx % s.sp.eventFactor == s.sp.eventOffset).map(s => s.selOh(slotIdx/s.sp.eventFactor)).orR
  }
}
