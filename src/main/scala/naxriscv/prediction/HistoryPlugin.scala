package naxriscv.prediction

import naxriscv.Global
import naxriscv.backend.CommitPlugin
import naxriscv.fetch.{AlignerPlugin, FetchConditionalPrediction, FetchPlugin, PcPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.RobService
import naxriscv.prediction.Prediction.CONDITIONAL_TAKE_IT
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable

case class HistoryPush(width : Int) extends Bundle{
  val mask = Bits(width bits)
  val taken = Bits(width bits)
}

class HistoryPlugin extends Plugin{
  case class HistoryPushSpec(priority : Int, width : Int, port : HistoryPush)
  val historyPushSpecs = mutable.ArrayBuffer[HistoryPushSpec]()
  def createPushPort(priority : Int, width : Int): HistoryPush = {
    historyPushSpecs.addRet(HistoryPushSpec(priority, width, HistoryPush(width))).port
  }

  val keys = create early new AreaRoot{
    val BRANCH_HISTORY_WIDTH = getServicesOf[HistoryUser].map(_.historyWidthUsed).max
    val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val rob = getService[RobService]
    frontend.retain()
    fetch.retain()
    rob.retain()

    getService[AlignerPlugin].addWordContext(keys.BRANCH_HISTORY)
    getService[BranchContextPlugin].dispatchWrite(keys.BRANCH_HISTORY)
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val commit = getService[CommitPlugin]
    val predictor = getService[PredictorPlugin]
    val branchContext = getService[BranchContextPlugin]
    val rob = getService[RobService]

    val fetchInsertAt = getServicesOf[FetchConditionalPrediction].map(_.useHistoryAt).min
    val onCommit = new Area {
      val value = Reg(keys.BRANCH_HISTORY) init (0)

      val event = commit.onCommit()
      val isConditionalBranch = rob.readAsync(predictor.keys.BRANCH_CONDITIONAL, Global.COMMIT_COUNT, event.robId)
      val isTaken = rob.readAsync(branchContext.keys.BRANCH_TAKEN, Global.COMMIT_COUNT, event.robId)
      var valueNext = CombInit(value)
      for (slotId <- 0 until Global.COMMIT_COUNT) {
        when(event.mask(slotId) && isConditionalBranch(slotId)) {
          valueNext \= valueNext.dropHigh(1) ## isTaken(slotId)
        }
      }
      value := valueNext.resized
    }

    val onFetch = new Area{
      val value = Reg(keys.BRANCH_HISTORY) init (0)
    }

    val update = new Area{
      assert(fetchInsertAt >= 1, "Would require some bypass of the stage(0) value, maybe later it could be implemented")
      fetch.getStage(fetchInsertAt)(keys.BRANCH_HISTORY) := onFetch.value

      val fetchJumps = getService[PcPlugin].getFetchJumps()
      val grouped = fetchJumps.groupBy(_._1)
      val ordered = grouped.toSeq.sortBy(_._1).map(_._2)
      val group = for(group <- grouped) yield new Area{ //TODO manage case where the group is < than branchHistoryFetchAt
        val valid = group._2.map(_._2).orR
        when(valid){
          onFetch.value := fetch.getStage(group._1).resulting(keys.BRANCH_HISTORY)
        }
      }

      val pushes = for(spec <- historyPushSpecs.sortBy(_.priority)) yield new Area{
        val state = Reg(keys.BRANCH_HISTORY) init (0)
        var stateNext = CombInit(state)
        for(slotId <- 0 until spec.width){
          when(spec.port.mask(slotId)) {
            stateNext \= stateNext.dropHigh(1) ## spec.port.taken(slotId)
          }
        }
        state := stateNext
        when(spec.port.mask =/= 0){
          onFetch.value := stateNext
        }
      }

      when(commit.reschedulingPort().valid){
        onFetch.value := onCommit.valueNext
        pushes.foreach(_.state := onCommit.valueNext)
      }
    }
    frontend.release()
    fetch.release()
    rob.release()
  }
}
