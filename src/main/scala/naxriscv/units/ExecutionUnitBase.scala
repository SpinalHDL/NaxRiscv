package naxriscv.units

import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces.{MicroOp, _}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.pipeline.Connection.{DIRECT, M2S}
import spinal.lib.pipeline.{Pipeline, Stage, Stageable}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExecutionUnitKeys extends AreaObject {
  val ROB_ID = Stageable(ROB.ID_TYPE)
}

class ExecutionUnitBase(euId : String,
                        robIdStage : Int = 1,
                        contextStage : Int = 1,
                        rfReadStage : Int = 1,
                        decodeStage : Int = 1,
                        executeStage : Int = 2) extends Plugin with ExecuteUnitService with WakeService with LockedImpl{
  withPrefix(euId)

  override def uniqueIds = List(euId)
  override def hasFixedLatency = ???
  override def getFixedLatencies = ???
  override def pushPort() = pipeline.push.port
  override def euName() = euId
  override def wakeRobs = Nil//Seq(logic.wakePort)
  override def staticLatencies() = {
    lock.await()
    staticLatenciesStorage
  }

  val idToexecuteStages = mutable.LinkedHashMap[Int, Stage]()
  val rfStageables = mutable.LinkedHashMap[RfResource, Stageable[Bits]]()

  def apply(rf : RegfileSpec, access : RfAccess) = getStageable(rf -> access)
  def apply(r : RfResource) = getStageable(r)
  def getStageable(r : RfResource) : Stageable[Bits] = {
    rfStageables.getOrElseUpdate(r, Stageable(Bits(r.rf.width bits)).setCompositeName(this, s"${r.rf.getName()}_${r.access.getName()}"))
  }
  def getExecute(id : Int) : Stage = idToexecuteStages.getOrElseUpdate(id, new Stage().setCompositeName(pipeline, s"execute_$id"))

  case class WriteBackKey(rf : RegfileSpec, access : RfAccess, stage : Stage)
  val writeBacksSpec = mutable.LinkedHashMap[WriteBackKey, ArrayBuffer[Flow[Bits]]]()
  def newWriteback(rf : RegfileSpec, access : RfAccess, stage : Stage) : Flow[Bits] = {
    val ret = Flow(Bits(rf.width bits))
    writeBacksSpec.getOrElseUpdate(WriteBackKey(rf, access, stage), ArrayBuffer[Flow[Bits]]()) += ret
    ret
  }

  val microOps = ArrayBuffer[MicroOp]()

  class StageCompletionSpec(stage : Int){
    val sel = Stageable(Bool).setCompositeName(ExecutionUnitBase.this, s"completion_SEL_E$stage")
    val microOps = ArrayBuffer[MicroOp]()
    setDecodingDefault(sel, False)
  }
  val stagesCompletions = mutable.LinkedHashMap[Int, StageCompletionSpec]()
  def setStaticCompletion(microOp: MicroOp, completionStage : Int): Unit ={
    val completion = stagesCompletions.getOrElseUpdate(completionStage, new StageCompletionSpec(completionStage))
    completion.microOps += microOp
    addDecoding(microOp, DecodeList(completion.sel -> True))
  }


  val staticLatenciesStorage = ArrayBuffer[StaticLatency]()
  def setStaticWake(microOp: MicroOp, latency : Int): Unit ={
    staticLatenciesStorage += StaticLatency(microOp, latency)
  }

  override def addMicroOp(microOp: MicroOp) = {
    getService[DecoderService].addEuOp(this, microOp)
    microOps += microOp
  }
  def addMicroOp(microOp: MicroOp, completionStage : Int) = {
    getService[DecoderService].addEuOp(this, microOp)
    microOps += microOp
    setStaticCompletion(microOp,  completionStage)
  }
  def add(microOp: MicroOp) ={
    addMicroOp(microOp)
    new {
      def completionStage(stage : Int) : this.type = {
        setStaticCompletion(microOp,  stage)
        this
      }
    }
  }

  val decodingSpecs = mutable.LinkedHashMap[Stageable[_ <: BaseType], DecodingSpec[Stageable[_ <: BaseType]]]()
  def getDecodingSpec(key : Stageable[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
  def setDecodingDefault(key : Stageable[_ <: BaseType], value : BaseType) : Unit = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  def DecodeList(e : (Stageable[_ <: BaseType],Any)*) = List(e :_*)
  type DecodeListType = Seq[(Stageable[_ <: BaseType],Any)]
  def addDecoding(microOp: MicroOp, values : Seq[(Stageable[_ <: BaseType],Any)]) : Unit = {
    val op = Masked(microOp.key)
    for((key, value) <- values) {
      getDecodingSpec(key).addNeeds(op, Masked(value))
    }
  }

  val setup = create early new Area{
    getService[DecoderService].retain()
    getServicesOf[RegfileService].foreach(_.retain())
    getService[RobService].retain()
  }

  val pipeline = create late new Pipeline{
    // Define stages
    val fetch = List.fill(executeStage + 1)(newStage())
    for((m,s) <- (fetch.dropRight(1), fetch.drop(1)).zipped){
      connect(m, s)(M2S())
    }

    lock.await()
    getService[DecoderService].release()

    //Create and connect execute stages
    val executeStageCount = idToexecuteStages.map(_._1).max + 1
    val executeStages = ArrayBuffer[Stage]()
    for(i <- 0 until executeStageCount) {
      val s = getExecute(i)
      this.addStage(s)
      if(i == 0){
        connect(fetch.last, s)(DIRECT())
      } else {
        connect(getExecute(i-1), s)(M2S())
      }
      executeStages += s
    }

    import ExecutionUnitKeys._
    val withReady = false

    val rob = getService[RobService]
    val decoder = getService[DecoderService]
    val flush = getService[CommitService].reschedulingPort().valid
    val euGroup = decoder.euGroups.find(_.eus.contains(ExecutionUnitBase.this)).get
    val sf = euGroup.eus.size
    val so = euGroup.eus.indexOf(ExecutionUnitBase.this)

    executeStages.last.flushIt(flush, root = false)

    //Figure out which register file are in use
    val ressources = mutable.LinkedHashSet[Resource]()
    for(e <- microOps; r <- e.resources) ressources += r

    //Allocate all the resources
    val rfReadPorts = mutable.LinkedHashMap[RfResource, RegFileRead]()
    val rfReads = mutable.LinkedHashSet[RfRead]()
    var implementRd = false
    ressources.foreach {
      case r : RfResource if r.access.isInstanceOf[RfRead] => {
        val port = getService[RegfileService](r.rf).newRead(withReady)
        val name = s"${r.rf.getName()}_${r.access.getName()}"
        port.setCompositeName(this, s"rfReads_${name}")
        rfReadPorts(r) = port
        rfReads += r.access.asInstanceOf[RfRead]
      }
      case r : RfResource if r.access.isInstanceOf[RfWrite] => implementRd = true
      case PC_READ =>
      case INSTRUCTION_SIZE =>
    }

    // Implement the fetch pipeline
    val push = new Area{
      val stage = fetch(0)
      val port = Stream(ExecutionUnitPush())
      stage.valid := port.valid
      stage(ROB_ID) := port.robId
      port.ready := stage.isReady
    }

    val robId = new Area{
      val stage = fetch(robIdStage)
    }

    val context = new Area{
      val stage = fetch(contextStage)
      import stage._

      def read[T <: Data](key : Stageable[T]) = rob.readAsyncSingle(key, ROB_ID, sf, so)
      def readAndInsert[T <: Data](key : Stageable[T]) = stage(key) := read(key)

      readAndInsert(Frontend.MICRO_OP)
      for(e <- rfReads){
        readAndInsert(decoder.PHYS_RS(e))
        readAndInsert(decoder.READ_RS(e))
      }
      if(implementRd){
        readAndInsert(decoder.PHYS_RD)
        readAndInsert(decoder.WRITE_RD)
      }
      if(ressources.contains(PC_READ)){
        readAndInsert(Global.PC)
      }
      if(ressources.contains(INSTRUCTION_SIZE)){
        readAndInsert(Frontend.INSTRUCTION_SLICE_COUNT)
      }
    }

    val decoding = new Area{
      val stage = fetch(decodeStage)
      import stage._

      val coverAll = microOps.map(e => Masked(e.key))
      for((key, spec) <- decodingSpecs){
        key.assignFromBits(spec.build(Frontend.MICRO_OP, coverAll))
      }
    }

    val readRf = new Area{
      val stage = fetch(rfReadStage)
      import stage._

      for((rf, port) <- rfReadPorts){
        val id = rf.access.asInstanceOf[RfRead]
        port.valid := decoder.READ_RS(id)
        port.address := decoder.PHYS_RS(id)
        getStageable(rf) := port.data
      }
    }

    val writeBack = for((spec, writes) <- writeBacksSpec) yield new Area{
      val rfService = getService[RegfileService](spec.rf)
      val port = rfService.newWrite(withReady)
      port.valid := spec.stage.isFireing && spec.stage(decoder.WRITE_RD)
      port.robId := spec.stage(ROB_ID)
      port.address := spec.stage(decoder.PHYS_RD)
      port.data := MuxOH.or(writes.map(_.valid), writes.map(_.payload))
    }

    val completion = for((stageId, spec) <- stagesCompletions) yield new Area{
      val stage = executeStages(stageId)
      val port = rob.robCompletion()
      port.valid := stage.isFireing && stage(spec.sel)
      port.id := stage(ROB_ID)
    }


    getServicesOf[RegfileService].foreach(_.release())
    rob.release()
    this.build()
    assert(!(fetch(0).internals.arbitration.propagateReady && staticLatenciesStorage.nonEmpty), s"ExecutionUnit id=${euId} has static latencies but its pipeline can be halted")
  }

}
