package naxriscv.execute

import naxriscv.{DecodeList, Fetch, Frontend, Global, ROB}
import naxriscv.interfaces.{MicroOp, _}
import naxriscv.lsu.LsuPlugin
import naxriscv.utilities.{Plugin, WithRfWriteSharedSpec}
import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import spinal.lib.pipeline.Connection.{DIRECT, M2S}
import spinal.lib.pipeline.{Pipeline, Stage, Stageable}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import naxriscv.Global._

class ExecutionUnitBase(val euId : String,
                        var writebackCountMax : Int = 1,
                        var readPhysRsFromQueue : Boolean = false,
                        var contextAt : Int = 0,
                        var rfReadAt : Int = 0,
                        var decodeAt : Int = 0,
                        var executeAt : Int = 1) extends Plugin with ExecuteUnitService with WakeRobService with WakeRegFileService with LockedImpl with WithRfWriteSharedSpec{
  withPrefix(euId)

  override def hasFixedLatency = ???
  override def getFixedLatencies = ???
  override def pushPort() = pipeline.push.port
  override def euName() = euId
  override def wakeRobs = pipeline.wakeRobs.logic.map(_.rob).toSeq
  override def wakeRegFile = pipeline.wakeRf.logic.map(_.rf).toSeq

  case class WakeRobsSelSpec(stageId : Int, sel : Bool)
  val wakeRobsSelSpecs = ArrayBuffer[WakeRobsSelSpec]()
  def newWakeRobsSelAt(stageId : Int) : Bool = {
    wakeRobsSelSpecs.addRet(WakeRobsSelSpec(stageId, Bool())).sel
  }

  case class WakeRegFileSelSpec(stageId : Int, sel : Bool)
  val wakeRegFileSelSpecs = ArrayBuffer[WakeRegFileSelSpec]()
  def newWakeRegFileSelAt(stageId : Int) = {
    wakeRegFileSelSpecs.addRet(WakeRegFileSelSpec(stageId, Bool())).sel
  }

  override def staticLatencies() = {
    lock.await()
    staticLatenciesStorage
  }

  val idToexecuteStages = mutable.LinkedHashMap[Int, Stage]()
  val rfStageables = mutable.LinkedHashMap[RfResource, Stageable[Bits]]()
  val robStageable = mutable.LinkedHashSet[Stageable[_ <: Data]]()

  def apply(rf : RegfileSpec, access : RfAccess) = getStageable(rf -> access)
  def apply(r : RfResource) = getStageable(r)
  def getStageable(r : RfResource) : Stageable[Bits] = {
    rfStageables.getOrElseUpdate(r, Stageable(Bits(r.rf.width bits)).setName(s"${r.rf.getName()}_${r.access.getName()}"))
  }
  def getExecute(stageId : Int) : Stage = {
    if(stageId >= 0){
      idToexecuteStages.getOrElseUpdate(stageId, new Stage().setCompositeName(pipeline, s"execute_$stageId"))
    } else {
      setup.fetch.reverse(-stageId)
    }
  }
  def addRobStageable(s : Stageable[_ <: Data]) = robStageable += s

  case class WriteBackKey(rf : RegfileSpec, access : RfAccess, stage : Stage)
  case class WriteBackSpec(){
    var latency = Int.MaxValue
    val ports = ArrayBuffer[Flow[Bits]]()
  }
  val writeBacksSpec = mutable.LinkedHashMap[WriteBackKey, WriteBackSpec]()
  def newWriteback(rf : RegfileSpec, access : RfAccess, stage : Stage, latency : Int) : Flow[Bits] = {
    val ret = Flow(Bits(rf.width bits))
    val spec = writeBacksSpec.getOrElseUpdate(WriteBackKey(rf, access, stage), WriteBackSpec())
    spec.ports += ret
    spec.latency = spec.latency min latency
    ret
  }

  val microOps = ArrayBuffer[MicroOp]()

  class StageCompletionSpec(stage : Int){
    val sel = Stageable(Bool).setName(s"completion_SEL_E$stage")
    val microOps = ArrayBuffer[MicroOp]()
    setDecodingDefault(sel, False)
  }
  val stagesCompletions = mutable.LinkedHashMap[Int, StageCompletionSpec]()
  def setCompletion(microOp: MicroOp, stageId : Int): Unit ={
    val completion = stagesCompletions.getOrElseUpdate(stageId, new StageCompletionSpec(stageId))
    completion.microOps += microOp
    addDecoding(microOp, DecodeList(completion.sel -> True))
  }


  val staticLatenciesStorage = ArrayBuffer[StaticLatency]()
  def setStaticWake(microOp: MicroOp, stageId : Int): Unit ={
    staticLatenciesStorage += StaticLatency(microOp, stageId)
  }

  override def addMicroOp(microOp: MicroOp) = {
    getService[DecoderService].addEuOp(this, microOp)
    microOps += microOp
  }
  def addMicroOp(microOp: MicroOp, completionStage : Int) = {
    getService[DecoderService].addEuOp(this, microOp)
    microOps += microOp
    setCompletion(microOp,  completionStage)
  }
  def add(microOp: MicroOp) ={
    addMicroOp(microOp)
    new {
      def completionStage(stage : Int) : this.type = {
        setCompletion(microOp,  stage)
        this
      }
    }
  }

  val decodingSpecs = mutable.LinkedHashMap[Stageable[_ <: BaseType], DecodingSpec[_ <: BaseType]]()
  def getDecodingSpec(key : Stageable[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
  def setDecodingDefault(key : Stageable[_ <: BaseType], value : BaseType) : Unit = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  def addDecoding(microOp: MicroOp, values : Seq[(Stageable[_ <: BaseType],Any)]) : Unit = {
    val op = Masked(microOp.key)
    for((key, value) <- values) {
      getDecodingSpec(key).addNeeds(op, Masked(value))
    }
  }

  def addDecoding(microOp: MicroOp, key : Stageable[_ <: BaseType], value : Any) : Unit = {
    val op = Masked(microOp.key)
    getDecodingSpec(key).addNeeds(op, Masked(value))
  }

  val setup = create early new Area{
    getService[DecoderService].retain()
    getServicesOf[RegfileService].foreach(_.retain())
    getService[RobService].retain()
    val fetch = List.fill(executeAt + 1)(new Stage())
  }

  val pipeline = create late new Pipeline{
    // Define stages
    setup.fetch.foreach{ f =>
      addStage(f)
      f.setRefOwner(this)
    }
    val fetch = setup.fetch
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

    val rob = getService[RobService]
    val decoder = getService[DecoderService]
    val flush = getService[CommitService].reschedulingPort(onCommit = true).valid
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
        val port = findService[RegfileService](_.rfSpec == r.rf).newRead(false)
        val name = s"${r.rf.getName()}_${r.access.getName()}"
        port.setCompositeName(this, s"rfReads_${name}")
        rfReadPorts(r) = port
        rfReads += r.access.asInstanceOf[RfRead]
      }
      case r : RfResource if r.access.isInstanceOf[RfWrite] => implementRd = true
      case PC_READ =>
      case INSTRUCTION_SIZE =>
      case _ =>
    }

    // Implement the fetch pipeline
    val push = new Area{
      val stage = fetch(0)
      val contextKeys = ArrayBuffer[Stageable[_ <: Data]]()
      if(readPhysRsFromQueue) for(e <- rfReads){
        contextKeys += decoder.PHYS_RS(e)
      }
      val port = ExecutionUnitPush(
        withReady = staticLatenciesStorage.isEmpty,
        physRdType = decoder.PHYS_RD,
        regfileRdType = decoder.REGFILE_RD,
        contextKeys = contextKeys
      )
      stage.valid := port.valid
      stage(ROB.ID) := port.robId
      if(implementRd) {
        stage(decoder.PHYS_RD) := port.physRd
        stage(decoder.REGFILE_RD) := port.regfileRd
      }
      if(port.withReady) port.ready := stage.isReady

      if (readPhysRsFromQueue) for(e <- rfReads) {
        val key = decoder.PHYS_RS(e)
        stage(key) := port.getContext(key)
      }
    }

    val context = new Area{
      val stage = fetch(contextAt)
      import stage._

      def read[T <: Data](key : Stageable[T]) = rob.readAsyncSingle(key, ROB.ID, sf, so)

      val inserted = mutable.LinkedHashSet[Stageable[_ <: Data]]()
      def readAndInsert[T <: Data](key : Stageable[T]) : Unit = {
        if(inserted.contains(key)) return
        inserted += key
        stage(key) := read(key)
      }

      readAndInsert(Frontend.MICRO_OP)
      for(e <- rfReads){
        if(!readPhysRsFromQueue) readAndInsert(decoder.PHYS_RS(e))
        readAndInsert(decoder.READ_RS(e))
      }
      if(implementRd){
        readAndInsert(decoder.WRITE_RD)
      }
      if(ressources.contains(PC_READ)){
        readAndInsert(PC)
      }
      if(ressources.contains(INSTRUCTION_SIZE)){
        readAndInsert(Fetch.INSTRUCTION_SLICE_COUNT)
      }
      for(s <- robStageable){
        readAndInsert(s)
      }
//      if(ressources.contains(SQ) || ressources.contains(LQ)){
//        readAndInsert(getService[LsuQueuePlugin].keys.LSU_ID)
//      }
    }

    val decoding = new Area{
      val stage = fetch(decodeAt)
      import stage._

      val coverAll = microOps.map(e => Masked(e.key))
      for((key, spec) <- decodingSpecs){
        key.assignFromBits(spec.build(Frontend.MICRO_OP, coverAll).asBits)
      }
    }

    val readRf = new Area{
      val stage = fetch(rfReadAt)
      import stage._

      for((rf, port) <- rfReadPorts){
        val id = rf.access.asInstanceOf[RfRead]
        port.valid := decoder.READ_RS(id)
        port.address := decoder.PHYS_RS(id)
        getStageable(rf) := port.data
      }
    }

    assert(writeBacksSpec.size <= writebackCountMax, s"$euId writeback count exceeded (${writeBacksSpec.size}) the limit set by the user (writebackCoutnMax=$writebackCountMax). At ${writeBacksSpec.map(_._1.stage).mkString(" ")}")
    val writeBack = for((key, spec) <- writeBacksSpec) yield new Area{
      val rfService = findService[RegfileService](_.rfSpec == key.rf)
      val sharing = getRfWriteSharing(key.rf)
      val write = rfService.newWrite(sharing.withReady, spec.latency, sharing.key, sharing.priority)
      write.valid := key.stage.isValid && key.stage(decoder.WRITE_RD) && spec.ports.map(_.valid).orR
      write.robId := key.stage(ROB.ID)
      write.address := key.stage(decoder.PHYS_RD)
      write.data := MuxOH.mux(spec.ports.map(_.valid), spec.ports.map(_.payload))
      if(write.withReady){
        key.stage.haltWhen(write.valid && !write.ready)
      }
    }

    val completion = for((stageId, spec) <- stagesCompletions) yield new Area{
      val stage = executeStages(stageId)
      val port = rob.newRobCompletion()
      port.valid := stage.isFireing && stage(spec.sel)
      port.id := stage(ROB.ID)
    }

    val wakeRobs = new Area{
      val grouped = wakeRobsSelSpecs.groupByLinked(_.stageId)
      val logic = for((stageId, group) <- grouped) yield new Area{
        val stage = executeStages(stageId)
        val fire = stage.isFireing && group.map(_.sel).orR
        val rob = Flow(WakeRob())

        rob.valid := fire && stage(decoder.WRITE_RD)
        rob.robId := stage(ROB.ID)
      }
    }

    val wakeRf = new Area{
      val grouped = wakeRegFileSelSpecs.groupByLinked(_.stageId)
      val logic = for((stageId, group) <- grouped) yield new Area{
        val stage = executeStages(stageId)
        val fire = stage.isFireing && group.map(_.sel).orR
        val rf = Flow(WakeRegFile(decoder.REGFILE_RD, decoder.PHYS_RD, needBypass = false))

        rf.valid := fire && stage(decoder.WRITE_RD)
        rf.regfile := stage(decoder.REGFILE_RD)
        rf.physical := stage(decoder.PHYS_RD)
      }
    }


    getServicesOf[RegfileService].foreach(_.release())
    rob.release()
    this.build()
    assert(!(fetch(0).internals.arbitration.propagateReady && staticLatenciesStorage.nonEmpty), s"ExecutionUnit id=${euId} has static latencies but its pipeline can be halted")
  }

}
