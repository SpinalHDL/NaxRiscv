package naxriscv.frontend

import naxriscv.{Frontend, Global}
import naxriscv.fetch.FetchPlugin.FETCH_ID
import naxriscv.interfaces.{AddressTranslationService, CommitService, LockedImpl}
import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.pipeline.Connection._
import spinal.lib.pipeline._
import naxriscv.utilities.Plugin
import spinal.lib.pipeline.Pipeline

trait FetchPipelineRequirements{
  def stagesCountMin : Int
}

//class FrontendElementPlugin extends Plugin{
//  val setup = create early new Area{
//    val frontend = framework.getService(classOf[FrontendPlugin])
//    frontend.retain()
//    frontend.pipeline.connect(frontend.pipeline.stages.last, frontend.pipeline.aligned)(new Logic)
//  }
//
//  val logic = create late new Area{
//
//    setup.frontend.release()
//  }
//}

class FrontendPlugin() extends Plugin with LockedImpl{
  val pipeline = create early new Pipeline{
    val stagesCount = framework.getServices.map{
      case s : FetchPipelineRequirements => s.stagesCountMin
      case _ => 0
    }.max

    val aligned = newStage()
    val decompressed = newStage()
    val decoded = newStage()
    val serialized = newStage()
    val allocated = newStage()
    val dispatch = newStage()

    import spinal.lib.pipeline.Connection._
    connect(serialized, allocated)(DIRECT())
    connect(allocated, dispatch)(M2S())

    for(slotId <- 0 until Frontend.DECODE_COUNT) {
      Verilator.public(decoded(FETCH_ID, slotId))
      Verilator.public(allocated(FETCH_ID, slotId))
      Verilator.public(decoded.isFireing)
      Verilator.public(allocated.isFireing)
    }
    val isBusy = Bool()
    val isBusyAfterDecode = Bool()
  }
  pipeline.setCompositeName(this)

  val builder = create late new Area{
    val commit = getService[CommitService]
    pipeline.dispatch.flushIt(commit.reschedulingPort(onCommit = false).valid)
    pipeline.allocated.haltIt(commit.hasPendingRescheduling())

    lock.await()
    pipeline.isBusy := (pipeline.stagesSet - pipeline.aligned).map(_.isValid).toList.orR
    pipeline.isBusyAfterDecode := List(pipeline.serialized, pipeline.allocated, pipeline.dispatch).map(_.isValid).toList.orR
    pipeline.build()
  }

  def getPipeline() = pipeline.get
  def getFollowing(m : Stage, latency : Int) : Stage = pipeline.getFollowing(m, latency)
  def isBusy() = pipeline.isBusy
  def isBusyAfterDecode() = pipeline.isBusyAfterDecode
}
