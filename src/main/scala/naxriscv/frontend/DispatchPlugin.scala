package naxriscv.frontend

import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK, MICRO_OP}
import naxriscv.backend.RobPlugin
import naxriscv.{Frontend, ROB}
import naxriscv.interfaces.{CommitService, DecoderService, InitCycles, IssueService, LockedImpl, MicroOp, RobWait, WakeRegFile, WakeRegFileService, WakeRobService, WakeWithBypassService}
import naxriscv.utilities.{Plugin, Service}
import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stageable}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



class DispatchPlugin(slotCount : Int = 0,
                     uintAt : Int = 0,
                     staticHitAt : Int = 0,
                     robIdAt : Int = 1,
                     physRdAt : Int = 1,
                     euAt : Int = 1,
                     readContextLayer0Factor : Int = LutInputs.get/2) extends Plugin with IssueService with LockedImpl with WakeRegFileService with InitCycles{
  val robWaits = ArrayBuffer[RobWait]()
  override def newRobDependency() = robWaits.addRet(RobWait())
  override def wakeRegFile = logic.pop.flatMap(_.wake.map(_.bypassed))

  override def initCycles = if(logic.globalStaticLatencies.latencies.isEmpty) 0 else logic.globalStaticLatencies.latencies.max + 4 //For sanity in some test configs withs absurd static latencies

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
    getService[RobPlugin].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val commit = getService[CommitService]
    val rob = getService[RobPlugin]
    lock.await()

    val decoder = getService[DecoderService]
    val groups = decoder.euGroups

    val stage = frontend.pipeline.dispatch
    import stage._

    val eus =  groups.flatMap(_.eus)
    val eusSel = groups.map(g => g.eus.map(_ -> g.sel)).flatten

    val rescheduling = commit.reschedulingPort

    val globalStaticLatencies = new Area{
      val raw = eus.flatMap(_.staticLatencies())
      val perLatency = raw.groupByLinked(_.latency).mapValues(_.map(_.microOp))
      val perMicroOp = raw.groupByLinked(_.microOp).mapValues(_.map(_.latency))
      for((op, latencies) <- perMicroOp){
        assert(latencies.distinct.size <= 1, s"The microOp ${op} has multiple static latencies")
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
      val physRd = decoder.PHYS_RD()
      val robId = ROB.ID() //Only for debug so far
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
    val queueStaticWakeTransposed = Vec.tabulate(globalStaticLatencies.latencies.size)(i => queue.io.contexts.map(_.staticWake(i)).asBits())
    val queueStaticWakeTransposedHistory = History(queueStaticWakeTransposed, 0 to staticHitAt)

    val ptr = new Area{
      val next = Reg(ROB.ID)  init(ROB.SIZE-slotCount + Frontend.DISPATCH_COUNT)
      val current = Reg(ROB.ID)  init(ROB.SIZE-slotCount)
      when(queue.io.push.fire){
        next := next + Frontend.DISPATCH_COUNT
        current := current + Frontend.DISPATCH_COUNT
      }

      queue.io.clear := rescheduling.valid
      when(rescheduling.valid){
        next := rescheduling.robIdNext - slotCount + Frontend.DISPATCH_COUNT
        current := rescheduling.robIdNext - slotCount
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
        slot.context.physRd := stage(decoder.PHYS_RD, slotId)
        slot.context.robId := stage(ROB.ID) | slotId
        for(latency <- globalStaticLatencies.latencies){
          val bitId = globalStaticLatencies.toBits(latency)
          slot.context.staticWake(bitId) := (decoder.WRITE_RD, slotId) && (globalStaticLatencies.latenciesStageable(latency), slotId)
        }
      }
    }


    val pop = for((port, portId) <- queue.io.schedules.zipWithIndex) yield new Pipeline{
      val eu = eus(portId)
      val mapping = queue.p.schedules(portId)

      val statics = eu.staticLatencies()
      val perLatency = statics.groupByLinked(_.latency).mapValues(_.map(_.microOp))

      val stageCount = (perLatency.keys.toSeq.map(_ + 1) :+ euAt).max + 1
      val stagesList = List.fill(stageCount)(newStage())
      for((m,s) <- (stagesList.dropRight(1), stagesList.drop(1)).zipped){
        connect(m, s)(M2S())
      }

      val keys = new Area{
        setName("")
        val OH = Stageable(Bits(slotCount bits))
        val UINT = Stageable(UInt(log2Up(slotCount) bits))
        val ROB_ID = Stageable(ROB.ID)
        val OFFSET = Stageable(cloneOf(ptr.current))
      }

      val entryStage = stagesList(0)
      val uintStage = stagesList(uintAt)
      val staticHitStage = stagesList(staticHitAt)
      val robIdStage = stagesList(robIdAt)
      val physRdStage = stagesList(physRdAt)
      val euStage = stagesList(euAt)

      def eventFull(v : Bits) = (0 until slotCount).map(i => if(i % mapping.eventFactor == mapping.eventOffset) v(i/mapping.eventFactor) else False).asBits()
      def filter[T](v : Seq[T]) = (for(i <- 0 until slotCount; if i % mapping.eventFactor == mapping.eventOffset) yield v(i))
      def readContext[T <: Data](key : Stageable[T], latency : Int)(extract : Context => T): Unit ={
        val factor = readContextLayer0Factor
        val inputs = filter(queue.io.contexts).map(extract(_).asBits)
        val oh = port.event
        latency match {
          case 0 => entryStage(key) := MuxOH.or(oh, inputs).as(key)
          case _ => {
            val layer0 = new Area{
              val ohGroups = oh.subdivideIn(factor bits, strict = false).toList
              val inputsGroups = inputs.grouped(factor).toList
              val groups = Vec((ohGroups, inputsGroups).zipped.map(MuxOH.or(_, _)))
              val key0 = Stageable(groups).setCompositeName(DispatchPlugin.this, s"${eu.euName}_readContext_${key.getName}")
              entryStage(key0) := groups
            }

            val layer1 = new Area{
              val stage = stagesList(1)
              stage(key) := stage(layer0.key0).reduceBalancedTree(_ | _).as(key)
            }
          }
        }
      }

      val portEventFull = eventFull(port.event)
      entryStage.valid := port.valid
      entryStage(keys.OH) := portEventFull
      entryStage(keys.OFFSET) := ptr.current
      port.ready := entryStage.isReady

      uintStage(keys.UINT) :=  OHToUInt(uintStage(keys.OH))
      robIdStage(keys.ROB_ID) := robIdStage(keys.UINT).resize(log2Up(ROB.SIZE)) + robIdStage(keys.OFFSET)

      val staticsFilter = for((latency, key) <- globalStaticLatencies.latenciesStageable){
        staticHitStage(key) := (staticHitStage(keys.OH) & queueStaticWakeTransposedHistory(staticHitAt)(globalStaticLatencies.toBits(latency))).orR
      }

      readContext(decoder.PHYS_RD, physRdAt)(_.physRd)

      val euPort = eu.pushPort()
      euPort.valid := euStage.isValid
      euPort.robId := euStage(keys.ROB_ID)
      euPort.physRd := euStage(decoder.PHYS_RD)
      if(euPort.withReady) euStage.haltIt(!euPort.ready)

      stagesList.last.flushIt(rescheduling.valid, root = false)


      def self = this
      val wake = for((latency, microOps) <- perLatency) yield new Area{
        setCompositeName(self, s"wake_L$latency")
        val lat = latency
        val mask = port.fire ? portEventFull | B(0)

        val bypassed = Flow(WakeRegFile(decoder.PHYS_RD, needBypass = true))
        val stage = stagesList(latency+1)
        bypassed.valid := stage.valid && stage(globalStaticLatencies.latenciesStageable(latency))
        bypassed.physical := stage(decoder.PHYS_RD)
      }

      this.build()
    }

    val wake = new Area {
      val dynamic = new Area {
        def g2l(robId: UInt) = (robId - ptr.current).resize(log2Up(slotCount))
        val sources = getServicesOf[WakeRobService]
        val ids = sources.flatMap(_.wakeRobs)
        val offseted = ids.map(f => f.translateWith(g2l(f.robId)))
        val masks = offseted.map(o => B(slotCount bits, default -> o.valid) & UIntToOh(o.payload))
      }
      val statics = for(latency <- globalStaticLatencies.latencies) yield new Area{
        val popMasks = pop.flatMap(_.wake.filter(_.lat == latency).map(_.mask))
        val popMask = popMasks.reduceBalancedTree(_ | _) & queueStaticWakeTransposed(globalStaticLatencies.toBits(latency))
        val history = Vec.fill(latency+1)(cloneOf(popMask))
        history(0) := popMask
        for(i <- 1 until latency+1){
          history(i).setAsReg()
          history(i) := (queue.io.push.valid ? (history(i-1) |>> DISPATCH_COUNT) | history(i-1)) //Queue compression
        }
        val mask = history(latency)
      }
      queue.io.events := (dynamic.masks ++ statics.map(_.mask)).reduceBalancedTree(_ | _) //Todo squezing first the dynamic one with some KEEP attribut may help synthesis timings for statics
    }
    frontend.release()
    rob.release()
  }
}
