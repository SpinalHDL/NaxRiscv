package naxriscv.fetch

import naxriscv.Global
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


class FetchPlugin() extends Plugin with LockedImpl {
  val keys = create early new AreaRoot{
    val spec = getService[AddressTranslationService]
    val FETCH_PC_POST_TRANSLATION   = Stageable(UInt(spec.postWidth bits))
    val FETCH_PC_PRE_TRANSLATION  = Stageable(UInt(spec.preWidth bits))
    val FETCH_PC_NEXT  = Stageable(UInt(spec.preWidth bits))
  }

  val pipeline = create early new Pipeline{
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

  val builder = create late new Area{
    lock.await()
    pipeline.build()
  }

  def getStage(id : Int) = pipeline.stages(id)
  def getLastStage = pipeline.stages.last
  def getPipeline() = pipeline.get
}
