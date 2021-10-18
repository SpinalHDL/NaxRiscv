package vooxriscv.pipeline

import spinal.core._
import spinal.lib._
import vooxriscv.utilities.Misc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Stageable{
  def apply[T <: Data](gen : => T) = new Stageable(gen)
}

class Stageable[T <: Data](gen : => T) extends HardType(gen) with Nameable {

}

class Stage extends Nameable {
  val internals = new {
    val input = new {
      val valid = Bool()
      var ready : Bool = null
    }

    val output = new Area {
      val valid = Bool()
      var ready : Bool = null
    }

//    val will = new {
//      var haltAsk, removeAsk, flushAsk = false
//      var haltBe, removeBe, flushBe = false
//    }

    val arbitration = new {
      var isRemoved : Bool = null
      var isFlushed : Bool = null
      var isFlushingNext : Bool = null
      var isHalted : Bool = null
      var isHaltedByOthers : Bool = null
    }

    val request = new {
      val halts = ArrayBuffer[Bool]()
      val removes = ArrayBuffer[Bool]()

    }



    val stageableToData = mutable.LinkedHashMap[Stageable[Data], Data]()
    val stageableOverloadedToData = mutable.LinkedHashMap[Stageable[Data], Data]()

    def outputOf(key : Stageable[Data]) = stageableOverloadedToData.get(key) match {
      case Some(x) => x
      case None => stageableToData(key)
    }
  }

  implicit def stageablePiped[T <: Data](stageable: Stageable[T]) = Stage.this(stageable)
  def haltIt() : Unit = haltIt(ConditionalContext.isTrue)
  def haltIt(cond : Bool) : Unit = {
//    internals.will.haltAsk = true
//    internals.will.haltBe = true
    internals.request.halts += cond
  }
  def flushIt() : Unit = ???
  def removeIt(): Unit = ???
  def isValid: Bool = internals.input.valid
  def isFireing: Bool = isValid && isReady
  def isRemoved : Bool = {
    if(internals.arbitration.isRemoved == null) internals.arbitration.isRemoved = Misc.outsideCondScope(Bool())
    internals.arbitration.isRemoved
  }
  def isFlushed : Bool = {
    if(internals.arbitration.isFlushed == null) internals.arbitration.isFlushed = Misc.outsideCondScope(Bool())
    internals.arbitration.isFlushed
  }
  def isReady : Bool = {
    if(internals.input.ready == null) internals.input.ready = Misc.outsideCondScope(Bool())
    internals.input.ready
  }

  def valid = internals.input.valid






  def apply[T <: Data](key : Stageable[T]) : T = {
    internals.stageableToData.getOrElseUpdate(key.asInstanceOf[Stageable[Data]],Misc.outsideCondScope{
//      val input,inputDefault = key()
//      inputsDefault(key.asInstanceOf[Stageable[Data]]) = inputDefault
//      input := inputDefault
//      input.setPartialName(this, key.getName())

      val v = key()

      v
    }).asInstanceOf[T]
  }

  def overloaded[T <: Data](key : Stageable[T]) : T = {
    internals.stageableOverloadedToData.getOrElseUpdate(key.asInstanceOf[Stageable[Data]], Misc.outsideCondScope {
      //      val input,inputDefault = key()
      //      inputsDefault(key.asInstanceOf[Stageable[Data]]) = inputDefault
      //      input := inputDefault
      //      input.setPartialName(this, key.getName())

      val v = CombInit(this.apply(key))

      v
    }).asInstanceOf[T]
  }


//  def <<(that : Stage) = {
//
//    that
//  }
//
//  def <-<(that : Stage) = {
//    that
//  }
//
////  def >>(that : Stage) = {
////    that
////  }
//  def >>(that : Stage) = new {
//    def apply(x : Int) = {
//
//    }
//  }
//
//  def >->(that : Stage) = {
//    that
//  }
}

case class ConnectionPoint(valid : Bool, ready : Bool, payload : Vec[Data]) extends Nameable
trait ConnectionLogic extends OverridedEqualsHashCode{
  def on(m : ConnectionPoint,
         s : ConnectionPoint,
         flush : Bool) : Area // Remove => one element, flush =>

  def latency : Int = ???
  def tokenCapacity : Int = ???
}

object Connection{
  case class DIRECT() extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool) = new Area {
      if(m.ready != null) m.ready   := s.ready
      s.valid   := m.valid
      s.payload := m.payload
    }

    override def latency = 0
    override def tokenCapacity = 0
  }

  case class M2S(collapse : Boolean = true,
                 holdPayload : Boolean = false,
                 flushPreserveInput : Boolean = false) extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool) = new Area{

      val (rValid, rData) = m.ready match {
        case null => (
            RegNext(m.valid) init(False),
            RegNext(m.payload)
        )
        case r =>   (
          RegNextWhen(m.valid, r) init(False),
          RegNextWhen(m.payload, if(holdPayload) m.valid && r else r)
        )
      }

      if(flushPreserveInput){
        println("WARNING flushPreserveInput NOT IMPLEMENTED")
      }
      if (flush != null && !flushPreserveInput) rValid clearWhen(flush)

      if(m.ready != null) {
        m.ready := s.ready
        if (collapse) m.ready setWhen (!s.valid)
      }

      s.valid := rValid
      s.payload := rData
    }

    override def latency = 1
    override def tokenCapacity = 1
  }

}

class Pipeline extends Area{
  case class ConnectionModel() extends Nameable {
    var m, s : Stage = null
    val logics = ArrayBuffer[ConnectionLogic]()
  }
//  val stages = ArrayBuffer[Stage]()
  val connections = ArrayBuffer[ConnectionModel]()
  val joins = ArrayBuffer[ConnectionModel]() //Not implemented yet
  val forks = ArrayBuffer[ConnectionModel]() //Not implemented yet

  def connect(m : Stage, s : Stage)(logics : ConnectionLogic*) = {
    val c = new ConnectionModel
    connections += c
    c.m = m
    c.s = s
    c.logics ++= logics
  }

  def precedenceOf(that : Stage, over : Stage) : Boolean = {
    val stageMasters = mutable.LinkedHashMap[Stage, ArrayBuffer[Stage]]()
    for(c <- connections){
      stageMasters(c.s) += c.m
    }
    def rec(end : Stage): Boolean ={
      if(stageMasters.contains(that)) return true
      stageMasters.get(end) match {
        case Some(x) => x.map(rec).reduce(_ || _)
        case None => false
      }
    }
    rec(over)
  }


  def build(): Unit = {
    implicit def internalsImplicit(stage : Stage) = stage.internals
    val stages = mutable.LinkedHashSet[Stage]() ++ connections.map(c => List(c.m, c.s)).flatten
    val stagesWithSink = mutable.LinkedHashSet[Stage]() ++ connections.map(_.m)
    val connectionsWithoutSinks = stages -- stagesWithSink
    val stageMasters = mutable.LinkedHashMap[Stage, ArrayBuffer[Stage]]()
    val stageDriver = mutable.LinkedHashMap[Stage, Any]()
    stageMasters ++= stages.map(s => (s -> ArrayBuffer[Stage]()))
    for(c <- connections){
      stageMasters(c.s) += c.m
      assert(!stageDriver.contains(c.s))
      stageDriver(c.s) = c
    }

    //Fill payload holes in the pipeline
    def propagateData(key : Stageable[Data], stage : Stage): Boolean ={
      stage.stageableToData.get(key) match {
        case None => {
          val hits = ArrayBuffer[Stage]()
          for(m <- stageMasters(stage)){
            if(propagateData(key, m)){
              stage.apply(key) //Force creation
              hits += m
            }
          }
          hits.size match {
            case 0 => false
            case 1 => true
            case 2 => PendingError(s"$key at $stage has multiple drivers : ${hits.mkString(",")}"); false
          }
        }
        case Some(x) => true
      }
    }

    def propagateRequirements(stage : Stage): Unit ={
      if(stage.request.halts.nonEmpty){
        stage.isReady //Force creation
      }
      stageDriver.get(stage) match {
        case Some(c : ConnectionModel) => {
          if(c.s.input.ready != null && c.m.output.ready == null){
            c.m.output.ready = Bool()
          }
        }
        case None =>
      }

      for(m <- stageMasters(stage)){
        propagateRequirements(m)
      }
    }

    for(end <- connectionsWithoutSinks){
      for(key <- end.stageableToData.keys){
        for(m <- stageMasters(end)) {
          propagateData(key, m);
        }
      }

      propagateRequirements(end)
    }

    //Internal connections
    for(s <- stages){
      s.output.valid := s.input.valid

      (s.input.ready,  s.output.ready) match {
        case (null, null) =>
        case (null, o) => ???
        case (i, null) => s.input.ready := True
        case (i, o) => s.input.ready := s.output.ready
      }

      if(s.request.halts.nonEmpty){
        val doHalt = s.request.halts.orR
        when(doHalt){
          s.input.ready := False
          s.output.valid := False
        }
      }
    }

    //Interconnect stages
    for(c <- connections){
      val stageables = c.m.stageableToData.keys.filter(c.s.stageableToData.contains(_))
      var m = ConnectionPoint(c.m.output.valid, c.m.output.ready, Vec(stageables.map(c.m.stageableToData(_))))
      for((l, id) <- c.logics.zipWithIndex){

        val s = if(l == c.logics.last)
          ConnectionPoint(c.s.input.valid, c.s.input.ready, Vec(stageables.map(c.s.outputOf(_))))
        else {
          ConnectionPoint(Bool(), (m.ready != null) generate Bool(), Vec(stageables.map(_.craft())))
        }
        val area = l.on(m, s, null) //TODO
        area.setCompositeName(c, s"level_$id")
        m = s
      }

    }

    //Name stuff
    for(stage <- stages){
      for((key, value) <- stage.internals.stageableToData){
        value.setName(s"${stage}_${key}")
      }
    }

    for(c <- connections){
      c.setName(s"${c.m}_to_${c.s}")
    }
  }

//  Component.current.afterElaboration(build)
}

case class PipelineTop() extends Component {
  val io = new Bundle {
    val source = slave Stream(UInt(8 bits))
    val sink   = master Flow(UInt(8 bits))

    val cond0 = in UInt(8 bits)
  }

  val pipeline = new Pipeline{
    val s0, s1, s2 = new Stage()
    val A, B, C = Stageable(UInt(8 bits))

    import Connection._
    connect(s0, s1)(DIRECT())
    connect(s1, s2)(M2S(), M2S(), M2S())

    val onS0 = new Area {
      import s0._
      valid := io.source.valid
      io.source.ready := s0.isReady
      A := io.source.payload
    }

    val onS1 = new Area{
      import s1._
      when(io.cond0 === 0){
        haltIt()
      }
      when(io.cond0 === 1){
        haltIt()
      }

      B := A + 1
      C := A + 2
    }

    val onS2 = new Area {
      import s2._
      io.sink.valid := internals.output.valid
      io.sink.payload := B + C
    }

    build()
  }
}

object PipelinePlay extends App{
  SpinalVerilog{
    PipelineTop()
  }
}