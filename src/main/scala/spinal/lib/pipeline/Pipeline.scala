package spinal.lib.pipeline

import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Pipeline extends Area{
  case class ConnectionModel() extends Nameable {
    var m, s : Stage = null
    val logics = ArrayBuffer[ConnectionLogic]()
  }
  val stagesSet = mutable.LinkedHashSet[Stage]()
  val connections = ArrayBuffer[ConnectionModel]()
  val joins = ArrayBuffer[ConnectionModel]() //Not implemented yet
  val forks = ArrayBuffer[ConnectionModel]() //Not implemented yet

  def newStage() : Stage = {
    val s = new Stage
    stagesSet += s
    s
  }

  def addStage(s : Stage): Stage ={
    stagesSet += s
    s
  }

  def connect(m : Stage, s : Stage)(logics : ConnectionLogic*) = {
    val c = new ConnectionModel
    connections += c
    c.m = m
    c.s = s
    c.logics ++= logics
    c
  }

  def connect(sequentially : Seq[Stage])(logics : => Seq[ConnectionLogic]): Unit ={
    for((m, s) <- (sequentially.dropRight(1), sequentially.tail).zipped){
      connect(m, s)(logics :_*)
    }
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
    this.stagesSet ++= connections.map(c => List(c.m, c.s)).flatten
    val stagesWithSink = mutable.LinkedHashSet[Stage]() ++ connections.map(_.m)
    val connectionsWithoutSinks = stagesSet -- stagesWithSink
    val stageMasters = mutable.LinkedHashMap[Stage, ArrayBuffer[Stage]]()
    val stageDriver = mutable.LinkedHashMap[Stage, Any]()
    stageMasters ++= stagesSet.map(s => (s -> ArrayBuffer[Stage]()))
    for(c <- connections){
      stageMasters(c.s) += c.m
      assert(!stageDriver.contains(c.s))
      stageDriver(c.s) = c
    }

    for(s <- stagesSet){
      for(key <- s.internals.stageableResultingToData.keys){
        s.apply(key)
      }
    }

    //Fill payload holes in the pipeline
    def propagateData(key : StageableKey, stage : Stage): Boolean ={
      if(stage.internals.stageableTerminal.contains(key)) return false
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

    val clFlush = mutable.LinkedHashMap[ConnectionLogic, Bool]()
    val clFlushNext = mutable.LinkedHashMap[ConnectionLogic, Bool]()
    val clFlushNextHit = mutable.LinkedHashMap[ConnectionLogic, Bool]()

    def propagateRequirements(stage : Stage): Unit ={
      if(stage.arbitration.isRemoved != null){
        stage.arbitration.isRemoved := stage.isFlushed
      }
      if(stage.request.halts.nonEmpty){
        stage.arbitration.propagateReady = true
        stage.isReady //Force creation
      }
      def orR(l : Seq[Bool]) : Bool = l.size match {
        case 0 => null
        case 1 => l.head
        case _ => l.orR
      }
      var flush = stage.internals.request.flush.nonEmpty generate orR(stage.internals.request.flush)
      var flushNext = stage.internals.request.flushNext.nonEmpty generate orR(stage.internals.request.flushNext)
      (stage.internals.arbitration.isFlushed, flush) match {
        case (null, null) =>
        case (x, null) => stage.isFlushed := False
        case (_, x) =>    stage.isFlushed := flush
      }
      (stage.internals.arbitration.isFlushingNext, flushNext) match {
        case (null, null) =>
        case (x, null) => stage.isFlushingNext := False
        case (_, x) =>    stage.isFlushingNext := flushNext
      }
      stageDriver.get(stage) match {
        case Some(c : ConnectionModel) => {
          if(c.s.arbitration.propagateReady && c.m.output.ready == null){
            c.m.output.ready = Bool()
            if(c.m.input.ready == null){
              c.m.input.ready = Bool()
            }
          }
          c.logics.reverseIterator.foreach{ l =>
            clFlush(l) = flush
            clFlushNext(l) = flushNext
            clFlushNextHit(l) = null
            if(flushNext != null){
              clFlushNextHit(l) = Bool()
              flush = flush match {
                case null => clFlushNext(l) && clFlushNextHit(l)
                case _ => flush || clFlushNext(l) && clFlushNextHit(l)
              }
              flushNext = l.alwasContainsSlaveToken match {
                case true => null
                case false => clFlushNext(l) && !clFlushNextHit(l)
              }
            }
          }
          if(flush != null) c.m.flushIt(flush, false)
          if(flushNext != null) c.m.flushNext(flushNext)
        }
        case None =>
      }

      for(m <- stageMasters(stage)){
        if(stage.internals.arbitration.propagateReady) m.internals.arbitration.propagateReady = true
        propagateRequirements(m)
      }
    }

    for(stage <- stagesSet){
      for(key <- stage.stageableToData.keys){
        for(m <- stageMasters(stage)) {
          propagateData(key, m);
        }
      }
    }

    for(end <- connectionsWithoutSinks){
      propagateRequirements(end)
    }


    for(s <- stagesSet){
      if(s.internals.request.flushRoot.nonEmpty) s.isFlushingRoot
      if(s.internals.arbitration.isFlushingRoot != null)s.internals.arbitration.isFlushingRoot := s.internals.request.flushRoot.orR
    }

    //Name stuff
    for(s <- stagesSet){
      import s.internals._
      s.internals.output.valid.setCompositeName(s, "valid_output", true)
      if(s.internals.output.ready != null) s.internals.output.ready.setCompositeName(s, "ready_output", true)
      s.internals.input.valid.setCompositeName(s, "valid", true)
      if(s.internals.input.ready != null) s.internals.input.ready.setCompositeName(s, "ready", true)
      if(arbitration.isFlushed != null) arbitration.isFlushed.setCompositeName(s, "isFlushed", true)
      if(arbitration.isFlushingNext != null) arbitration.isFlushingNext.setCompositeName(s, "isFlushingNext", true)
      if(arbitration.isFlushingRoot != null) arbitration.isFlushingRoot.setCompositeName(s, "isFlushingRoot", true)
      if(arbitration.isHalted != null) arbitration.isHalted.setCompositeName(s, "isHalted", true)
      if(arbitration.isHaltedByOthers != null) arbitration.isHaltedByOthers.setCompositeName(s, "isHaltedByOthers", true)
      if(arbitration.isRemoved != null) arbitration.isRemoved.setCompositeName(s, "isRemoved", true)
      if(arbitration.isForked != null) arbitration.isForked.setCompositeName(s, "isForked", true)

      if(arbitration.isForked != null) arbitration.isForked := output.valid && output.ready
    }

    //Internal connections
    for(s <- stagesSet){
      s.output.valid := s.input.valid
      if(s.internals.request.flushRoot.nonEmpty) s.output.valid clearWhen(s.internals.arbitration.isFlushingRoot)

      (s.input.ready,  s.output.ready) match {
        case (null, null) =>
        case (null, o) => ???
        case (i, null) => s.input.ready := True
        case (i, o) => s.input.ready := s.output.ready
      }
      if(s.request.throws.nonEmpty){
        val doThrow = s.request.throws.orR
        when(doThrow){
//          s.input.ready := False //Maybe to reconsiderate
          s.output.valid := False
        }
      }
      if(s.request.halts.nonEmpty){
        val doHalt = s.request.halts.orR
        when(doHalt){
          s.input.ready := False
          s.output.valid := False
        }
      }
      if(s.request.forks.nonEmpty){
        val doFork = s.request.forks.orR
        when(doFork){
          s.input.ready := False //Maybe to reconsiderate
        }
      }

      for((key, value) <- s.internals.stageableResultingToData){
        value := s.internals.outputOf(key)
      }
    }

    //Interconnect stages
    for(c <- connections){
      val stageables = (c.m.stageableToData.keys).filter(key => c.s.stageableToData.contains(key) && !c.m.stageableTerminal.contains(key))
      var m = ConnectionPoint(c.m.output.valid, c.m.output.ready, stageables.map(c.m.outputOf(_)).toList)
      for((l, id) <- c.logics.zipWithIndex){

        val s = if(l == c.logics.last)
          ConnectionPoint(c.s.input.valid, c.s.input.ready, stageables.map(c.s.stageableToData(_)).toList)
        else {
          ConnectionPoint(Bool(), (m.ready != null) generate Bool(), stageables.map(_.stageable.craft()).toList)
        }
        val area = l.on(m, s, clFlush(l), clFlushNext(l), clFlushNextHit(l))
        if(c.logics.size != 1)
          area.setCompositeName(c, s"level_$id", true)
        else
          area.setCompositeName(c, true)
        m = s
      }

    }

    //Name stuff
    for(stage <- stagesSet){
      for((key, value) <- stage.internals.stageableToData){
        value.setCompositeName(stage, s"${key}")
      }
      for((key, value) <- stage.internals.stageableOverloadedToData){
        value.setCompositeName(stage, s"${key}_overloaded")
      }
      for((key, value) <- stage.internals.stageableResultingToData){
        value.setCompositeName(stage, s"${key}_resulting")
      }
    }

    for(c <- connections){
      if(c.isUnnamed) c.setName(s"${c.m}_to_${c.s}")
    }
  }

  def getFollowing(m : Stage, latency : Int): Stage = latency match{
    case 0 => m
    case x if latency > 0 => connections.find(_.m == m) match {
      case Some(c) => getFollowing(c.s, latency - c.logics.map(_.latency).sum)
      case None => ???
    }
  }

  //  Component.current.afterElaboration(build)
}