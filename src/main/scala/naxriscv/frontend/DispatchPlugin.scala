package naxriscv.frontend

import naxriscv.Frontend.DISPATCH_MASK
import naxriscv.{Frontend, ROB}
import naxriscv.interfaces.{CommitService, DecoderService, IssueService, RobWait, WakeService}
import naxriscv.utilities.{Plugin, Service}
import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable.ArrayBuffer



class DispatchPlugin(slotCount : Int) extends Plugin with IssueService{
  val robWaits = ArrayBuffer[RobWait]()
  override def newRobWait() = {
    val e = RobWait()
    robWaits += e
    e
  }

  val lock = Lock()
  override def retain() = lock.retain()
  override def release() = lock.release()

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }


  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val commit = getService[CommitService]
    lock.await()

    val decoder = getService[DecoderService]
    val groups = decoder.euGroups

    val stage = frontend.pipeline.allocated
    import stage._

    val eus =  groups.flatMap(_.eus)
    val eusSel = groups.map(g => g.eus.map(_ -> g.sel)).flatten

    val queue = new IssueQueue(
      p = IssueQueueParameter(
        slotCount  = slotCount,
        wayCount   = Frontend.DISPATCH_COUNT,
        selCount   = groups.size,
        schedules  = groups.map(g => g.eus.map(eu =>
          new ScheduleParameter(
            eventFactor = g.eus.size,
            eventOffset = g.eus.indexOf(eu),
            selId       = groups.indexOf(g)
          )
        )).flatten
      ),
      slotContextType = NoData
    )

    val ptr = new Area{
      val next = Reg(ROB.ID_TYPE)  init(ROB.SIZE-slotCount + Frontend.DISPATCH_COUNT)
      val current = Reg(ROB.ID_TYPE)  init(ROB.SIZE-slotCount)
      when(queue.io.push.fire){
        next := next + Frontend.DISPATCH_COUNT
        current := current + Frontend.DISPATCH_COUNT
      }
      val rescheduling = commit.reschedulingPort
      queue.io.clear := rescheduling.valid
      when(rescheduling.valid){
        next := rescheduling.nextRob - slotCount + Frontend.DISPATCH_COUNT
        current := rescheduling.nextRob - slotCount
      }
    }


    val wake = new Area {
      def g2l(robId : UInt) = (robId - ptr.current).resize(log2Up(slotCount))
      val sources = getServicesOf[WakeService]
      val ids = sources.flatMap(_.wakeRobs)
      val offseted = ids.map(f => f.translateWith(g2l(f.payload)))
      val masks = offseted.map(o => B(slotCount bits, default -> o.valid) & UIntToOh(o.payload))
      queue.io.events := masks.reduceBalancedTree(_ | _)
    }

    val push = new Area{
      def g2l(robId : UInt) = (robId - ptr.next).resize(log2Up(slotCount))
      queue.io.push.valid := isFireing
      stage.haltIt(!queue.io.push.ready) //Assume ready at 0 when not full
      val slots = for(slotId <- 0 until Frontend.DISPATCH_COUNT) yield new Area{
        val slot = queue.io.push.slots(slotId)
        val self   = ((decoder.WRITE_RD, slotId) && (DISPATCH_MASK, slotId)) ? B(BigInt(1) << slotCount-Frontend.DISPATCH_COUNT+slotId, slotCount bits) | B(0)
        val events = robWaits.map(o => B(slotCount bits, default -> stage(o.ENABLE, slotId)) & UIntToOh(g2l(stage(o.ID, slotId))))
        slot.event := (self +: events).reduceBalancedTree(_ | _)
        slot.sel   := (DISPATCH_MASK, slotId) ? groups.map(g => stage(g.sel, slotId)).asBits() | B(0)
      }
    }

    val pop = for((port, portId) <- queue.io.schedules.zipWithIndex) yield new Area{
      def l2g(robId : UInt) = robId.resize(log2Up(ROB.SIZE))  + ptr.current

      val eu = eus(portId)
      val mapping = queue.p.schedules(portId)
      val eventFull = (0 until slotCount).map(i => if(i % mapping.eventFactor == mapping.eventOffset) port.event(i/mapping.eventFactor) else False).asBits()
      val robId = l2g(OHToUInt(eventFull))

      val euPort = eu.pushPort()
      euPort.arbitrationFrom(port)
      euPort.robId := robId
    }

//    val events = KeepAttribute(in(p.eventType()))
//    val push = slave Stream(IssueQueuePush(p, slotContextType))
//    val schedules = Vec(p.schedules.map(sp => master Stream(Schedule(sp, p.slotCount))))

    frontend.release()
  }
}
