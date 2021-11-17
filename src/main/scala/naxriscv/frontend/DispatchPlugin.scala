package naxriscv.frontend

import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK, MICRO_OP}
import naxriscv.{Frontend, ROB}
import naxriscv.interfaces.{CommitService, DecoderService, IssueService, LockedImpl, MicroOp, RobWait, WakeService}
import naxriscv.utilities.{Plugin, Service}
import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stageable}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



class DispatchPlugin(slotCount : Int = 0,
                     uintAt : Int = 0,
                     robIdAt : Int = 1,
                     euAt : Int = 1) extends Plugin with IssueService with LockedImpl{
  val robWaits = ArrayBuffer[RobWait]()
  override def newRobDependency() = robWaits.addRet(RobWait())


  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val commit = getService[CommitService]
    lock.await()

    val decoder = getService[DecoderService]
    val groups = decoder.euGroups

    val stage = frontend.pipeline.dispatch
    import stage._

    val eus =  groups.flatMap(_.eus)
    val eusSel = groups.map(g => g.eus.map(_ -> g.sel)).flatten

    val globalStaticLatencies = new Area{
      val raw = eus.flatMap(_.staticLatencies())
      val perLatency = raw.groupByLinked(_.latency).mapValues(_.map(_.microOp))
      val perMicroOp = raw.groupByLinked(_.microOp)
      for((op, spec) <- perMicroOp){
        assert(spec.size <= 1, s"The microOp ${op} has multiple static latencies")
      }

      val latencies = perLatency.keys
      val allTerms = decoder.euGroups.flatMap(_.microOps).distinctLinked.map(e => Masked(e.key))
      val latenciesStageable = mutable.LinkedHashMap[Int, Stageable[Bool]]()
      for(latency <- perLatency.keys.toList.sorted) {
        val key = Stageable(Bool()).setName(s"LATENCY_$latency")
        latenciesStageable(latency) = key

        val trueTerms = perLatency(latency).map(op => Masked(op.key))
        val falseTerms = allTerms -- trueTerms
        val stage = frontend.pipeline.decoded
        for(slotId <- 0 until DISPATCH_COUNT) {
          stage(key, slotId) := Symplify.apply(stage(MICRO_OP, slotId), trueTerms, falseTerms)
        }
      }
      val toBits = latenciesStageable.keys.toList.sorted.zipWithIndex.toMap
      val fromBits = toBits.map(e => e._2 -> e._1).toMap
    }

    case class Context() extends Bundle{
      val staticWake = Bits(globalStaticLatencies.latencies.size bits)
    }
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
      slotContextType = Context()
    )
    val queueStaticWakeTransposed = (0 until globalStaticLatencies.latencies.size).map(i => queue.io.contexts.map(_.staticWake(i)).asBits())

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
        for(latency <- globalStaticLatencies.latencies){
          val bitId = globalStaticLatencies.toBits(latency)
          slot.context.staticWake(bitId) := (globalStaticLatencies.latenciesStageable(latency), slotId)
        }
      }
    }


    val pop = for((port, portId) <- queue.io.schedules.zipWithIndex) yield new Pipeline{
      val stagesList = List.fill(euAt+1)(newStage())
      for((m,s) <- (stagesList.dropRight(1), stagesList.drop(1)).zipped){
        connect(m, s)(M2S())
      }


      val eu = eus(portId)
      val mapping = queue.p.schedules(portId)

      val keys = new Area{
        setName("")
        val OH = Stageable(cloneOf(port.event))
        val UINT = Stageable(UInt(log2Up(slotCount) bits))
        val ROB_ID = Stageable(ROB.ID_TYPE)
        val OFFSET = Stageable(cloneOf(ptr.current))
      }

      val entryStage = stagesList(0)
      val uintStage = stagesList(uintAt)
      val robIdStage = stagesList(robIdAt)
      val euStage = stagesList(euAt)

      entryStage.valid := port.valid
      entryStage(keys.OH) := port.event
      entryStage(keys.OFFSET) := ptr.current
      port.ready := entryStage.isReady

      val eventFull = (0 until slotCount).map(i => if(i % mapping.eventFactor == mapping.eventOffset) uintStage(keys.OH)(i/mapping.eventFactor) else False).asBits()
      uintStage(keys.UINT) :=  OHToUInt(eventFull)
      robIdStage(keys.ROB_ID) := robIdStage(keys.UINT).resize(log2Up(ROB.SIZE)) + robIdStage(keys.OFFSET)

      val euPort = eu.pushPort()
      euPort.valid := euStage.isValid
      euPort.robId := euStage(keys.ROB_ID)
      if(euPort.withReady) euStage.haltIt(!euPort.ready)

      val flush = getService[CommitService].reschedulingPort().valid
      euStage.flushIt(flush, root = false)

      val statics = eu.staticLatencies()
      val perLatency = statics.groupByLinked(_.latency).mapValues(_.map(_.microOp))
      def self = this
      val wake = for((latency, microOps) <- perLatency) yield new Area{
        setCompositeName(self, s"wake_L$latency")
        val lat = latency
        val mask = port.fire ? eventFull | B(0)
      }

      this.build()
    }

    val wake = new Area {
      val dynamic = new Area {
        def g2l(robId: UInt) = (robId - ptr.current).resize(log2Up(slotCount))
        val sources = getServicesOf[WakeService]
        val ids = sources.flatMap(_.wakeRobs)
        val offseted = ids.map(f => f.translateWith(g2l(f.payload)))
        val masks = offseted.map(o => B(slotCount bits, default -> o.valid) & UIntToOh(o.payload))
      }
      val statics = for(latency <- globalStaticLatencies.latencies) yield new Area{
        val popMasks = pop.flatMap(_.wake.filter(_.lat == latency).map(_.mask))
        val popMask = popMasks.reduceBalancedTree(_ | _) & queueStaticWakeTransposed(globalStaticLatencies.toBits(latency))
        val history = History(popMask, 0 to latency-1) //TODO miss shifts
        val mask = history(latency - 1)
      }
      queue.io.events := (dynamic.masks ++ statics.map(_.mask)).reduceBalancedTree(_ | _) //Todo squezing first the dynamic one with some KEEP attribut may help synthesis timings for statics
    }
    frontend.release()
  }
}
