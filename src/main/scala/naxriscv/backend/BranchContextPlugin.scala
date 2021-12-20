package naxriscv.backend

import naxriscv.Frontend._
import naxriscv.Global._
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

case class BranchLearn(pcWidth : Int, branchCount : Int) extends Bundle{
  val finalContext = BranchFinalContext(pcWidth)
  val id = UInt(log2Up(branchCount) bits)
}


class BranchContextPlugin(val branchCount : Int) extends Plugin with LockedImpl {
  assert(isPow2(branchCount))

  def readEarly(address : UInt) = logic.mem.earlyBranch.readAsync(address)
  def writeFinal() = logic.mem.finalBranch.writePort

  val dispatchWriteModel = mutable.LinkedHashSet[Stageable[_ <: Data]]()
  val learnReadModel = mutable.LinkedHashMap[Stageable[_ <: Data], Data]()
  def dispatchWrite(key : Stageable[_ <: Data]*) = dispatchWriteModel ++= key
  def learnRead[T <: Data](key : Stageable[T])  = learnReadModel.getOrElseUpdate(key, key()).asInstanceOf[T]
  def learnValid = setup.learnValid

  val keys = create early new AreaRoot{
    val BRANCH_SEL   = Stageable(Bool())
    val BRANCH_ID    = Stageable(UInt(log2Up(branchCount) bits))
    val BRANCH_EARLY = Stageable(BranchEarlyContext(PC_WIDTH))
    val BRANCH_FINAL = Stageable(BranchFinalContext(PC_WIDTH))
    val BRANCH_TAKEN = Stageable(Bool())
  }

  val setup = create early new Area{
    getService[RobService].retain()
    getService[FrontendPlugin].retain()
    val learnValid = Bool()
  }


  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val rob = getService[RobService]
    val commit = getService[CommitService]
    val k = keys.get
    import k._

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

      rob.write(BRANCH_ID, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(BRANCH_ID, _)),  ROB.ID, isFireing)
      rob.write(BRANCH_SEL, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(BRANCH_SEL, _)),  ROB.ID, isFireing)
    }

    val onCommit = new Area{
      val event = commit.onCommit()
      val isBranch = rob.readAsync(BRANCH_SEL, Global.COMMIT_COUNT, event.robId)
      val isBranchCommit = (0 until Global.COMMIT_COUNT).map(slotId => event.mask(slotId) && isBranch(slotId))
      ptr.commited := ptr.commited + CountOne(isBranchCommit)
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

  val free = create late new Area {
    lock.await()
    val l = logic.get

    import l._

    val dispatchMem = new Area {
      val lrmks = learnReadModel.keySet
      val dataKeys = dispatchWriteModel.filter(lrmks.contains)
      val width = dataKeys.map(widthOf(_)).sum
      val mem = Mem.fill(branchCount)(Bits(width bits))
      val writes = for (slotId <- 0 until Frontend.DISPATCH_COUNT) yield new Area {
        val stage = frontend.pipeline.dispatch
        import stage._
        implicit val _ = StageableOffset(slotId)

        val port = mem.writePort
        port.valid := isFireing && keys.BRANCH_SEL && DISPATCH_MASK
        port.address := keys.BRANCH_ID
        port.data := Cat(dataKeys.map(e => B(stage(e, slotId))))
      }
    }

    val learn = new Area {
      val valid = ptr.free =/= ptr.commited
      val bid = ptr.free.resize(log2Up(branchCount) bits)

      learnReadModel.get(keys.BRANCH_FINAL) match {
        case Some(x) => x := mem.finalBranch.readAsync(bid)
        case None =>
      }
      learnReadModel.get(keys.BRANCH_ID) match {
        case Some(x) => x := bid
        case None =>
      }

      val raw = dispatchMem.mem.readAsync(bid)
      val offsets = dispatchMem.dataKeys.scanLeft(0)(_ + widthOf(_))
      for((key, offset) <- (dispatchMem.dataKeys, offsets).zipped){
        val value = learnReadModel(key).setCompositeName(this, key.getName())
        value.assignFromBits(raw(offset, widthOf(value) bits))
      }

      setup.learnValid := valid

      when(valid) {
        ptr.free := ptr.free + 1
      }
    }
  }
}
