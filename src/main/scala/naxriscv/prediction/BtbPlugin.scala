package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Global._
import naxriscv.fetch.{AlignerPlugin, FetchPlugin}
import naxriscv.interfaces.JumpService
import naxriscv.prediction.Prediction._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

class BtbPlugin(entries : Int,
                hashWidth : Int = 16,
                readAt : Int = 0,
                hitAt : Int = 1,
                jumpAt : Int = 2) extends Plugin with FetchWordPrediction{

  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val branchContext = getService[BranchContextPlugin]
    val priority = JumpService.Priorities.FETCH_WORD(jumpAt, true)
    val btbJump = jump.createJumpInterface(priority, aggregationPriority = (jumpAt < 2).toInt)
    val historyPush = getService[HistoryPlugin].createPushPort(priority, 1)
    val aligned = getService[AlignerPlugin]

    aligned.addLastWordContext(
      BRANCH_HISTORY_PUSH_VALID,
      BRANCH_HISTORY_PUSH_SLICE,
      BRANCH_HISTORY_PUSH_VALUE
    )

    fetch.retain()
    branchContext.retain()
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val branchContext = getService[BranchContextPlugin]
    val history = getService[HistoryPlugin]


    //TODO learn conditional bias
    val wordBytesWidth = log2Up(FETCH_DATA_WIDTH/8)

    def getHash(value : UInt) = value(wordBytesWidth, hashWidth bits) //TODO better hash
    case class BtbEntry() extends Bundle {
      val hash = UInt(hashWidth bits)
      val slice  = UInt(log2Up(SLICE_COUNT) bits)
      val pcTarget = PC()
      val isBranch = Bool()
    }

    val ENTRY = Stageable(BtbEntry())
    val HIT = Stageable(Bool())
    val mem = Mem.fill(entries)(BtbEntry()) //TODO bypass read durring write ?

    val onLearn = new Area{
      val ctx = branchContext.learnRead(branchContext.keys.BRANCH_FINAL)
      val hash = getHash(ctx.pcOnLastSlice)

      val port = mem.writePort
      port.valid := branchContext.learnValid
      port.address := (ctx.pcOnLastSlice >> wordBytesWidth).resized
      port.data.hash := hash
      port.data.slice := (ctx.pcOnLastSlice >> SLICE_RANGE_LOW).resized
      port.data.pcTarget := ctx.pcTarget
      port.data.isBranch := branchContext.learnRead(IS_BRANCH)
    }

    val readCmd = new Area{
      val stage = fetch.getStage(readAt)
      import stage._
      val entryAddress = (FETCH_PC >> wordBytesWidth).resize(mem.addressWidth)
    }
    val readRsp = new Area{
      val stage = fetch.getStage(readAt+1)
      import stage._
      stage(ENTRY) := mem.readSync(readCmd.entryAddress, readCmd.stage.isReady)
      KeepAttribute(stage(ENTRY))
    }

    val hitCalc = new Area{
      val stage = fetch.getStage(hitAt)
      import stage._

      val postPcPrediction = FETCH_PC(SLICE_RANGE) > ENTRY.slice
      HIT := ENTRY.hash === getHash(FETCH_PC) && !postPcPrediction
    }

    val applyIt = new Area{
      val stage = fetch.getStage(jumpAt)
      import stage._

      val prediction = getServiceOption[FetchConditionalPrediction] match {
        case Some(s) => s.getPredictionAt(jumpAt)(ENTRY.slice)
        case None => True
      }

      val needIt = isValid && HIT && !(ENTRY.isBranch && !prediction)
      val correctionSent = RegInit(False) setWhen(isValid) clearWhen(isReady || isFlushed)
      val doIt = needIt && !correctionSent

      flushNext(doIt)

      setup.btbJump.valid := doIt
      setup.btbJump.pc := ENTRY.pcTarget

      WORD_BRANCH_VALID := needIt
      WORD_BRANCH_SLICE := ENTRY.slice
      WORD_BRANCH_PC_NEXT := ENTRY.pcTarget

      setup.historyPush.flush    := isValid && HIT && ENTRY.isBranch && !correctionSent
      setup.historyPush.mask(0)  := setup.historyPush.flush
      setup.historyPush.taken(0) := prediction

      BRANCH_HISTORY_PUSH_VALID := setup.historyPush.flush
      BRANCH_HISTORY_PUSH_SLICE := ENTRY.slice
      BRANCH_HISTORY_PUSH_VALUE := prediction
    }

    fetch.release()
    branchContext.release()
  }
}