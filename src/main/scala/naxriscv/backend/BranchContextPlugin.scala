package naxriscv.backend

import naxriscv.Frontend._
import naxriscv._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{AddressTranslationService, CommitService, LockedImpl, RobCompletion, RobLineMask, RobService}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.pipeline.{Stageable, StageableOffset}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class BranchEarlyContext(pcWidth : Int) extends Bundle{
  val pcNext = UInt(pcWidth bits)
}


case class BranchFinalContext(pcWidth : Int) extends Bundle{
  val pcOnLastSlice = UInt(pcWidth bits)
  val pcNext = UInt(pcWidth bits)
  val taken = Bool()
}


class BranchContextPlugin(branchCount : Int) extends Plugin with LockedImpl {
  def PC = getService[AddressTranslationService].PC
  assert(isPow2(branchCount))

  def readEarly(address : UInt) = logic.mem.earlyBranch.readAsync(address)
  def writeFinal() = logic.mem.finalBranch.writePort

  val keys = create early new AreaRoot{
    val BRANCH_SEL   = Stageable(Bool())
    val BRANCH_ID    = Stageable(UInt(log2Up(branchCount) bits))
    val BRANCH_EARLY = Stageable(BranchEarlyContext(widthOf(PC)))
    val BRANCH_FINAL = Stageable(BranchFinalContext(widthOf(PC)))
  }

  val setup = create early new Area{
    getService[RobService].retain()
    getService[FrontendPlugin].retain()
  }


  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val rob = getService[RobService]
    val commit = getService[CommitService]
    val k = keys.get
    import k._

    lock.await()

    val ptr = new Area{
      val alloc, commited, free = Reg(UInt(log2Up(branchCount) + 1 bits)) init(0)
      def isFull(ptr : UInt) = (ptr ^ free) === branchCount
    }

    val mem = new Area{
      val earlyBranch = Mem.fill(branchCount)(BRANCH_EARLY)
      val finalBranch = Mem.fill(branchCount)(BRANCH_FINAL)
    }

    val alloc = new Area{
      val stage = frontend.pipeline.allocated
      import stage._

      var allocNext = CombInit(ptr.alloc)
      val full = False
      val slots = for(slotId <- 0 until DISPATCH_COUNT) yield new Area{
        implicit val _ = StageableOffset(slotId)
        BRANCH_ID := allocNext.resized
        when(isValid && BRANCH_SEL && DISPATCH_MASK){
          full setWhen(ptr.isFull(allocNext))
          allocNext \= allocNext + 1
          when(isReady){
            mem.earlyBranch.write(
              address = BRANCH_ID,
              data = BRANCH_EARLY
            )
          }
        }
      }

      haltIt(full) //This could be optimized easily for timings, but that's maybe already ok
      when(isFireing) {
        ptr.alloc := allocNext
      }

      rob.write(BRANCH_ID, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(BRANCH_ID, _)),  ROB.ROB_ID, isFireing)
      rob.write(BRANCH_SEL, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(BRANCH_SEL, _)),  ROB.ROB_ID, isFireing)
    }

    val onCommit = new Area{
      val event = commit.onCommit()
      val isBranch = rob.readAsync(BRANCH_SEL, Global.COMMIT_COUNT, event.robId)
      val isBranchCommit = (0 until Global.COMMIT_COUNT).map(slotId => event.mask(slotId) && isBranch(slotId))
      ptr.commited := ptr.commited + CountOne(isBranchCommit)
    }

    val free = new Area{
      val learn = Flow(BRANCH_FINAL())
      learn.valid := ptr.free =/= ptr.commited
      learn.payload := mem.finalBranch.readAsync(ptr.free.resized)

      when(learn.fire){
        ptr.free := ptr.free + 1
      }
    }

    val onReschedule = new Area{
      val event = RegNext(commit.reschedulingPort.valid) init(False)
      when(event){
        ptr.alloc := ptr.commited
      }
    }

    frontend.release()
    rob.release()
  }
}
