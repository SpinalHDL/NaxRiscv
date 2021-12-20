package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Global._
import naxriscv.fetch.{FetchConditionalPrediction, FetchPlugin, FetchWordPrediction}
import naxriscv.interfaces.JumpService
import naxriscv.prediction.Prediction._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

class GSharePlugin(entries : Int,
                   historyWidth : Int,
                   insertAt : Int = 2) extends Plugin with FetchConditionalPrediction with HistoryUser{
  val words = entries / SLICE_COUNT

  val readAt = insertAt - 1
  override def useHistoryAt = readAt
  override def historyWidthUsed = historyWidth

  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    val branchContext = getService[BranchContextPlugin]

    fetch.retain()
    branchContext.retain()
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val branchContext = getService[BranchContextPlugin]
    val predictor = getService[PredictorPlugin]
    val BRANCH_HISTORY = getService[HistoryPlugin].keys.BRANCH_HISTORY

//    val keys = new AreaRoot {
//      val GSHARE_STRONG = Stageable(Bits(SLICE_COUNT bits))
//    }

    def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.high + 1, log2Up(words) bits).reversed ^ U(history).resized

    val mem = new Area{ //TODO bypass read durring write ?
      val takeIt = Mem.fill(words)(CONDITIONAL_TAKE_IT)
//      val strong = Mem.fill(words)(keys.GSHARE_STRONG)
    }

    val readCmd = new Area{
      val stage = fetch.getStage(readAt)
      import stage._

      val address = gshareHash(FETCH_PC, BRANCH_HISTORY)
    }

    val readRsp = new Area{
      val stage = fetch.getStage(insertAt)

      stage(CONDITIONAL_TAKE_IT) := mem.takeIt.readSync(readCmd.address, readCmd.stage.isReady)
//      stage(keys.GSHARE_STRONG) := mem.strong.readSync(readCmd.address, readCmd.stage.isReady)
    }

    val onLearn = new Area{
      val ctx = branchContext.learnRead(branchContext.keys.BRANCH_FINAL)
      val hash = gshareHash(ctx.pcOnLastSlice, branchContext.learnRead(BRANCH_HISTORY))

      val takeItPort = mem.takeIt.writePort
      takeItPort.valid := branchContext.learnValid && branchContext.learnRead(predictor.keys.BRANCH_CONDITIONAL)
      takeItPort.address := hash
      takeItPort.data := branchContext.learnRead(CONDITIONAL_TAKE_IT)
      takeItPort.data(ctx.pcOnLastSlice(SLICE_RANGE)) := ctx.taken
    }

    fetch.release()
    branchContext.release()
  }
}
