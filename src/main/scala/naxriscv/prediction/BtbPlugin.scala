package naxriscv.prediction

import naxriscv.Fetch._
import naxriscv.Global._
import naxriscv.fetch.{AlignerPlugin, FetchConditionalPrediction, FetchPlugin, FetchWordPrediction}
import naxriscv.interfaces.JumpService
import naxriscv.prediction.Prediction._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

class BtbPlugin(entries : Int,
                hashWidth : Int = 16,
                readAt : Int = 0,
                jumpAt : Int = 2) extends Plugin with FetchWordPrediction{

  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val branchContext = getService[BranchContextPlugin]
    val btbJump = jump.createJumpInterface(JumpService.Priorities.FETCH_WORD(jumpAt, true))

    fetch.retain()
    branchContext.retain()
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val branchContext = getService[BranchContextPlugin]


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
    }
    val applyIt = new Area{
      val stage = fetch.getStage(jumpAt)
      import stage._

      val prediction = getServiceOption[FetchConditionalPrediction] match {
        case Some(s) => s.getPredictionAt(jumpAt)(ENTRY.slice)
        case None => True
      }

      val postPcPrediction = FETCH_PC(SLICE_RANGE) > ENTRY.slice
      val hit = isValid && ENTRY.hash === getHash(FETCH_PC) && !postPcPrediction// && FETCH_PC(SLICE_RANGE) =/= entry.pcNext(SLICE_RANGE) //TODO ?
      val needIt = hit && !(ENTRY.isBranch && !prediction)
      val correctionSent = RegInit(False) setWhen(needIt) clearWhen(isReady || isFlushed)
      val doIt = needIt && !correctionSent

      flushNext(doIt)

      setup.btbJump.valid := doIt
      setup.btbJump.pc := ENTRY.pcTarget

      WORD_BRANCH_VALID := needIt
      WORD_BRANCH_SLICE := ENTRY.slice
      WORD_BRANCH_PC_NEXT := ENTRY.pcTarget
    }

    fetch.release()
    branchContext.release()
  }
}