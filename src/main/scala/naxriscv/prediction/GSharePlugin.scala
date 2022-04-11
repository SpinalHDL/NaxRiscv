package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Frontend
import naxriscv.Global._
import naxriscv.fetch.{AlignerPlugin, FetchPlugin}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.JumpService
import naxriscv.prediction.Prediction._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

class GSharePlugin(var historyWidth : Int,
                   var entries : Int = 0,
                   var memBytes : BigInt = null,
                   var readAt : Int = 0,
                   var counterWidth : Int = 2,
                   var readAsync : Boolean = false) extends Plugin with FetchConditionalPrediction with HistoryUser{

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

    aligner.addLastWordContext(
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

    def hashWidth = log2Up(words)
    def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.high + 1, hashWidth bits).reversed ^ U(history).resized

    val mem = new Area{ //TODO bypass read durring write ?
      val counter = Mem.fill(words)(keys.GSHARE_COUNTER)
      val write = counter.writePort
    }

    val BYPASS = Stageable(cloneOf(mem.write))
    val HASH = Stageable(UInt(hashWidth bits))

    val readCmd = new Area{
      val stage = fetch.getStage(readAt)
      import stage._

      HASH := gshareHash(FETCH_PC, BRANCH_HISTORY)
      stage(BYPASS) := mem.write
    }

    val readRsp = new Area{
      val stage = fetch.getStage(readAt+1)
      import stage._

      def readMem[T <: Data](mem : Mem[T], address : UInt = readCmd.stage(HASH)) = readAsync match {
        case false => mem.readSync(address, readCmd.stage.isReady)
        case true  => mem.readAsync(address)
      }
      stage(keys.GSHARE_COUNTER) := readMem(mem.counter)
      when(BYPASS.valid && stage(BYPASS).address === HASH){
        stage(keys.GSHARE_COUNTER) := stage(BYPASS).data
      }

      KeepAttribute(stage(keys.GSHARE_COUNTER))
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

      mem.write.valid := branchContext.learnValid && branchContext.learnRead(IS_BRANCH) && !overflow
      mem.write.address := hash
      mem.write.data := updated
    }

    fetch.release()
    frontend.release()
    branchContext.release()
  }
}
