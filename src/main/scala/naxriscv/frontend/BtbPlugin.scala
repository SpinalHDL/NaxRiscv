package naxriscv.frontend

import naxriscv.Fetch._
import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK}
import naxriscv.backend.BranchContextPlugin
import naxriscv.fetch.{AlignerPlugin, FetchPlugin, FetchWordPrediction, PcPlugin}
import naxriscv.interfaces.{AddressTranslationService, CommitService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import naxriscv.utilities.Plugin
import naxriscv.{Frontend, Global, ROB}
import spinal.core._
import spinal.lib._
import spinal.lib.logic.{DecodingSpec, Masked}
import spinal.lib.pipeline.{Stageable, StageableOffset}

class BtbPlugin(entries : Int,
                hashWidth : Int = 16,
                jumpAt : Int = 1) extends Plugin with FetchWordPrediction{

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
    val PC = getService[AddressTranslationService].PC
    val ak = getService[AlignerPlugin].keys.get
    import ak._

    val FETCH_PC = fetch.keys.FETCH_PC

    //TODO learn conditional bias
    val wordBytesWidth = log2Up(FETCH_DATA_WIDTH/8)

    def getHash(value : UInt) = value(wordBytesWidth, hashWidth bits) //TODO better hash
    case class BtbEntry() extends Bundle {
      val hash = UInt(hashWidth bits)
      val slice  = UInt(log2Up(SLICE_COUNT) bits)
      val pcNext = PC()
    }

    val mem = Mem.fill(entries)(BtbEntry()) //TODO bypass read durring write ?

    val onLearn = new Area{
      val ctx = branchContext.learnRead(branchContext.keys.BRANCH_FINAL)
      val hash = getHash(ctx.pcOnLastSlice)

      val port = mem.writePort
      port.valid := branchContext.learnValid
      port.address := (ctx.pcOnLastSlice >> wordBytesWidth).resized
      port.data.hash := hash
      port.data.slice := (ctx.pcOnLastSlice >> SLICE_RANGE_LOW).resized
      port.data.pcNext := ctx.pcNext
    }

    val read = new Area{
      val stage = fetch.getStage(jumpAt-1)
      import stage._
      val entryAddress = (FETCH_PC >> wordBytesWidth).resize(mem.addressWidth)
    }
    val applyIt = new Area{
      val stage = fetch.getStage(jumpAt)
      import stage._
      val entry = mem.readSync(read.entryAddress, read.stage.isReady)
      val hit = isValid && entry.hash === getHash(FETCH_PC)// && FETCH_PC(SLICE_RANGE) =/= entry.pcNext(SLICE_RANGE) //TODO ?
      flushNext(hit)
      setup.btbJump.valid := hit
      setup.btbJump.pc := entry.pcNext

      WORD_BRANCH_VALID := hit
      WORD_BRANCH_SLICE := entry.slice
      WORD_BRANCH_PC_NEXT := entry.pcNext
    }

    fetch.release()
    branchContext.release()
  }
}