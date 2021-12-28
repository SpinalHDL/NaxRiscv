package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Frontend
import naxriscv.Global._
import naxriscv.fetch.{AlignerPlugin, FetchConditionalPrediction, FetchPlugin, FetchWordPrediction}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.JumpService
import naxriscv.prediction.Prediction._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

class GSharePlugin(historyWidth : Int,
                   entries : Int = 0,
                   memBytes : BigInt = null,
                   readAt : Int = 1,
                   counterWidth : Int = 2,
                   readAsync : Boolean = false) extends Plugin with FetchConditionalPrediction with HistoryUser{

  override def useHistoryAt = readAt
  override def historyWidthUsed = historyWidth
  override def getPredictionAt(stageId: Int) = getService[FetchPlugin].getStage(stageId)(setup.keys.GSHARE_COUNTER).map(_.msb)

  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    val frontend = getService[FrontendPlugin]
    val branchContext = getService[BranchContextPlugin]
    val aligner = getService[AlignerPlugin]

    fetch.retain()
    frontend.retain()
    branchContext.retain()


    val keys = new AreaRoot {
      val GSHARE_COUNTER = Stageable(Vec.fill(SLICE_COUNT)(UInt(counterWidth bits)))
    }

    aligner.addWordContext(
      keys.GSHARE_COUNTER
    )
    branchContext.dispatchWrite(
      keys.GSHARE_COUNTER
    )
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val branchContext = getService[BranchContextPlugin]
    val predictor = getService[DecoderPredictionPlugin]
    val frontend = getService[FrontendPlugin]
    val BRANCH_HISTORY = getService[HistoryPlugin].keys.BRANCH_HISTORY

    var words = entries/SLICE_COUNT
    if(memBytes != null) words = (1 <<(log2Up(memBytes.toInt*8/counterWidth+1)-1)) / SLICE_COUNT
    assert(words != 0)
    assert(isPow2(words))

    val keys = setup.keys

    def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.high + 1, log2Up(words) bits).reversed ^ U(history).resized

    val mem = new Area{ //TODO bypass read durring write ?
      val counter = Mem.fill(words)(keys.GSHARE_COUNTER)
    }

    val readCmd = new Area{
      val stage = fetch.getStage(readAt)
      import stage._

      val address = gshareHash(FETCH_PC, BRANCH_HISTORY)
    }

    val readRsp = new Area{
      val stage = fetch.getStage(readAt+1)

      def readMem[T <: Data](mem : Mem[T], address : UInt = readCmd.address) = readAsync match {
        case false => mem.readSync(address, readCmd.stage.isReady)
        case true  => mem.readAsync(address)
      }
      stage(keys.GSHARE_COUNTER) := readMem(mem.counter)
    }

    val onDecompressed = new Area{
      val stage = frontend.pipeline.decompressed

      for(slotId <- 0 until Frontend.DECODE_COUNT) {
        stage(CONDITIONAL_TAKE_IT, slotId) := stage(keys.GSHARE_COUNTER, slotId).map(_.msb).asBits
      }
    }

    val onLearn = new Area{
      val ctx = branchContext.learnRead(branchContext.keys.BRANCH_FINAL)
      val hash = gshareHash(ctx.pcOnLastSlice, branchContext.learnRead(BRANCH_HISTORY))

      val counters = branchContext.learnRead(keys.GSHARE_COUNTER)
      val updated = keys.GSHARE_COUNTER()
      val incrValue = ctx.taken ? U(1) | U((1 << counterWidth)-1)
      val overflow = False
      for(sliceId <- 0 until SLICE_COUNT){
        updated(sliceId) := counters(sliceId) + incrValue.andMask(ctx.pcOnLastSlice(SLICE_RANGE) === sliceId)
        overflow setWhen(ctx.taken && counters(sliceId).msb && !updated(sliceId).msb || !ctx.taken && !counters(sliceId).msb && updated(sliceId).msb)
      }

      val counterPort = mem.counter.writePort
      counterPort.valid := branchContext.learnValid && branchContext.learnRead(IS_BRANCH) && !overflow
      counterPort.address := hash
      counterPort.data := updated
    }

    fetch.release()
    frontend.release()
    branchContext.release()
  }
}
