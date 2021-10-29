package naxriscv.frontend

import naxriscv.ROB
import naxriscv.interfaces.{IssueService, RobWait}
import naxriscv.pipeline.Stageable
import naxriscv.utilities.{Plugin, Service}
import spinal.core._
import spinal.core.fiber.Lock

import scala.collection.mutable.ArrayBuffer



class IssuePlugin extends Plugin with IssueService{
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
    lock.await()
    println(robWaits)

    frontend.release()
  }
}
