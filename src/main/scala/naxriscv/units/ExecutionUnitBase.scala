package naxriscv.units

import naxriscv.{Frontend, ROB}
import naxriscv.interfaces._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.{Pipeline, Stageable}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExecutionUnitKeys{
  val ROB_ID = Stageable(ROB.ID_TYPE)
}

class ExecutionUnitBase(euId : String) extends Plugin with ExecuteUnitService with WakeService with LockedImpl{

  override def uniqueIds = List(euId)

  override def hasFixedLatency = ???

  override def getFixedLatency = ???

  override def pushPort() = pipeline.pushPort

  override def euName() = euId

  override def wakeRobs = Nil//Seq(logic.wakePort)

  val microOps = ArrayBuffer[MicroOp]()


  override def addMicroOp(microOp: MicroOp) = {
    getService[DecoderService].addEuOp(this, microOp)
    microOps += microOp
  }

  val setup = create early new Area{
    val rob = getService[RobService]
    val completion = rob.robCompletion()
    getServicesOf[RegfileService].foreach(_.retain())
  }

  val pipeline = create late new Pipeline{
    val executeSages = 2
    val push, withRobId, withContext, withRegfile = newStage()
//    val execute = List.fill(executeSages)(newStage())


    lock.await()
    import ExecutionUnitKeys._
    val withReady = false

    val rob = getService[RobService]
    val decoder = getService[DecoderService]
    val flush = getService[CommitService].reschedulingPort().valid

    val pushPort = Stream(ExecutionUnitPush()) //TODO manage ready
    push.valid := pushPort.valid
    push(ROB_ID) := pushPort.robId




    val euGroup = decoder.euGroups.find(_.eus.contains(ExecutionUnitBase.this)).get
    val sf = euGroup.eus.size
    val so = euGroup.eus.indexOf(ExecutionUnitBase.this)





    //Figure out which register file are in use
    val ressources = mutable.LinkedHashSet[Resource]()
    for(e <- microOps; r <- e.resources) ressources += r

    //Allocate all the ressources
    val rfReads = mutable.LinkedHashMap[RfResource, RegFileRead]()
    ressources.foreach {
      case r : RfResource if r.access.isInstanceOf[RfRead] => {
        rfReads(r) = getService[RegfileService](r.rf).newRead(withReady)
      }
      case r : RfResource if r.access.isInstanceOf[RfWrite] => {

      }
    }




    getServicesOf[RegfileService].foreach(_.retain())
    this.build()
  }

}
