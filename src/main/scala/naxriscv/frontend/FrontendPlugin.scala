package naxriscv.frontend

import spinal.core._
import spinal.core.fiber._
import naxriscv.pipeline.Connection._
import naxriscv.pipeline._
import naxriscv.utilities.Plugin

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

class FrontendPlugin() extends Plugin {
  val lock = Lock()

  val pipeline = create early new Pipeline{
    val stagesCount = framework.getServices.map{
      case s : FetchPipelineRequirements => s.stagesCountMin
      case _ => 0
    }.max
    val fetches = Array.fill(stagesCount)(newStage())
    val aligned = newStage()
    val decompressed = newStage()
    val decoded = newStage()
    val renamed = newStage()

    import Connection._
    for((m, s) <- (fetches.dropRight(1), fetches.tail).zipped){
      connect(m, s)(M2S(flushPreserveInput = m == fetches.head))
    }

    connect(decoded, renamed)(M2S())
  }
  pipeline.setCompositeName(this)

  val builder = create late new Area{
    lock.await()
    pipeline.build()
  }

  def getStage(id : Int) = pipeline.fetches(id)
  def getPipeline() = pipeline.get

  def retain() = lock.retain()
  def release() = lock.release()


}
