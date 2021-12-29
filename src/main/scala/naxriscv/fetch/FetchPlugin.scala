package naxriscv.fetch

import naxriscv.Global
import naxriscv.Global._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{AddressTranslationService, CommitService, LockedImpl}
import spinal.core._
import spinal.core.fiber._
import spinal.lib.pipeline.Connection._
import spinal.lib.pipeline._
import naxriscv.utilities.Plugin
import spinal.lib.pipeline.Pipeline

trait FetchPipelineRequirements{
  def stagesCountMin : Int
}

object FetchPlugin extends AreaRoot {
  val FETCH_ID = Stageable(UInt(12 bits))
}

class FetchPlugin() extends Plugin with LockedImpl {
  import FetchPlugin._

  val pipeline = create early new Pipeline{
    getService[AlignerPlugin].addFirstWordContext(FETCH_ID)
    getService[FrontendPlugin].retain()

    val stagesCount = framework.getServices.map{
      case s : FetchPipelineRequirements => s.stagesCountMin
      case _ => 0
    }.max
    val stages = Array.fill(stagesCount)(newStage())

    import spinal.lib.pipeline.Connection._
    for((m, s) <- (stages.dropRight(1), stages.tail).zipped){
      connect(m, s)(M2S(flushPreserveInput = m == stages.head))
    }
  }
  pipeline.setCompositeName(this)

  val fetchId = create late new Area{
    val frontend = getService[FrontendPlugin]
    val stage = pipeline.stages(1)
    import stage._

    FETCH_ID.setAsReg() init(0)
    FETCH_ID := FETCH_ID + U(pipeline.stages(1).isFireing)

    Verilator.public(stage(FETCH_ID))
    Verilator.public(isFirstCycle)

    frontend.release()
  }

  val builder = create late new Area{
    lock.await()

    val whitebox = new AreaRoot{
      val fetchLastFire = Verilator.public(CombInit(pipeline.stages.last.isFireing))
      val fetchLastId = Verilator.public(CombInit(pipeline.stages.last(FETCH_ID)))
    }

    pipeline.build()
  }

  def getStage(id : Int) = pipeline.stages(id)
  def getLastStage = pipeline.stages.last
  def getPipeline() = pipeline.get
}
