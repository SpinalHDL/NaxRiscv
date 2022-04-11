package naxriscv.prediction

import naxriscv.{Frontend, Global, ROB}
import naxriscv.fetch.{AlignerPlugin, FetchPlugin, PcPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.RobService
import naxriscv.misc.CommitPlugin
import naxriscv.prediction.Prediction.{CONDITIONAL_TAKE_IT, IS_BRANCH}
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable

case class HistoryPush(width : Int) extends Bundle{
  val flush = Bool()
  val mask = Bits(width bits)
  val taken = Bits(width bits)
}

case class HistoryJump(width : Int) extends Bundle{
  val history = Bits(width bits)
}

class HistoryPlugin(var historyFetchBypass : Boolean = true) extends Plugin{
  class HistorySpec(val priority : Int){
    def update(history : Bits) = {}
  }
  case class HistoryPushSpec(override val priority : Int, width : Int, port : HistoryPush, state : Bits) extends HistorySpec(priority){
    override def update(history: Bits) = state := history
  }
  val historyPushSpecs = mutable.ArrayBuffer[HistoryPushSpec]()
  def createPushPort(priority : Int, width : Int): HistoryPush = {
    historyPushSpecs.addRet(HistoryPushSpec(priority, width, HistoryPush(width), Reg(keys.BRANCH_HISTORY()) init(0))).port
  }

  case class HistoryJumpSpec(override val priority : Int, port : Flow[HistoryJump]) extends HistorySpec(priority)
  val historyJumpSpecs = mutable.ArrayBuffer[HistoryJumpSpec]()
  def createJumpPort(priority : Int): Flow[HistoryJump] = {
    historyJumpSpecs.addRet(HistoryJumpSpec(priority, Flow(HistoryJump(historyWidth)))).port
  }
  
  def getState(priority : Int) = historyPushSpecs.find(_.priority == priority).get.state

  def historyWidth = (0 +: getServicesOf[HistoryUser].map(_.historyWidthUsed)).max
  val keys = create early new AreaRoot{
    val BRANCH_HISTORY_WIDTH = historyWidth
    val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val rob = getService[RobService]

    frontend.retain()
    fetch.retain()
    rob.retain()

    getService[AlignerPlugin].addLastWordContext(keys.BRANCH_HISTORY)
    getService[BranchContextPlugin].dispatchWrite(keys.BRANCH_HISTORY)
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val commit = getService[CommitPlugin]
    val predictor = getService[DecoderPredictionPlugin]
    val branchContext = getService[BranchContextPlugin]
    val rob = getService[RobService]

    val fetchUsages = getServicesOf[FetchConditionalPrediction]
    val fetchInsertAt = if(fetchUsages.nonEmpty) fetchUsages.map(_.useHistoryAt).min else fetch.pipeline.stages.size-1
    val onCommit = new Area {
      val value = Reg(keys.BRANCH_HISTORY) init (0)
      val whitebox = Verilator.public(Vec.fill(Global.COMMIT_COUNT)(keys.BRANCH_HISTORY()))

      val event = commit.onCommit()
      val isConditionalBranch = rob.readAsync(IS_BRANCH, Global.COMMIT_COUNT, event.robId)
      val isTaken = rob.readAsync(branchContext.keys.BRANCH_TAKEN, Global.COMMIT_COUNT, event.robId)
      var valueNext = CombInit(value)
      for (slotId <- 0 until Global.COMMIT_COUNT) {
        whitebox(slotId) := valueNext
        when(event.mask(slotId) && isConditionalBranch(slotId)) {
          valueNext \= (valueNext ## isTaken(slotId)).resize(keys.BRANCH_HISTORY_WIDTH)
        }
      }
      value := valueNext.resized

    }

    val onFetch = new Area{
      val value = Reg(keys.BRANCH_HISTORY) init (0)
      val valueNext = CombInit(value)
      value := valueNext
    }

    val update = new Area{
//      assert(fetchInsertAt >= 1, "Would require some bypass of the stage(0) value, maybe later it could be implemented")
      fetch.getStage(fetchInsertAt)(keys.BRANCH_HISTORY) := (if(historyFetchBypass) onFetch.valueNext else onFetch.value)

      val ports = (historyPushSpecs ++ historyJumpSpecs).sortBy(_.priority)
      assert(ports.map(_.priority).distinct.size == ports.size)
      val pushes = for(spec <- ports) yield spec match {
        case spec : HistoryPushSpec => new Area{
          val state = spec.state
          var stateNext = CombInit(spec.state)
          for(slotId <- 0 until spec.width){
            when(spec.port.mask(slotId)) {
              stateNext \= (stateNext ## spec.port.taken(slotId)).resize(keys.BRANCH_HISTORY_WIDTH)
            }
          }
          spec.state := stateNext
          when(spec.port.flush){
            onFetch.valueNext := stateNext
            historyPushSpecs.filter(_.priority < spec.priority).foreach(_.update(stateNext))
          }
        }
        case spec : HistoryJumpSpec => new Area{
          when(spec.port.valid){
            onFetch.valueNext := spec.port.history
            historyPushSpecs.filter(_.priority < spec.priority).foreach(_.update(spec.port.history))
          }
        }
      }


      when(commit.reschedulingPort().valid){
        onFetch.valueNext := onCommit.valueNext
        historyPushSpecs.foreach(_.update(onCommit.valueNext))
      }
    }

    Verilator.public(onCommit.value)
    frontend.pipeline.allocated(0 until Frontend.DECODE_COUNT)(keys.BRANCH_HISTORY).foreach(Verilator.public(_))

    frontend.release()
    fetch.release()
    rob.release()
  }
}
