package naxriscv.misc

import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK}
import naxriscv.Global._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces._
import naxriscv.utilities.{DocPlugin, Plugin}
import naxriscv.{Global, ROB}
import spinal.core._
import spinal.core.fiber._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class CommitPlugin(var commitCount : Int,
                   var ptrCommitRetimed : Boolean = true) extends Plugin with CommitService{

  create config {
    Global.COMMIT_COUNT.set(commitCount)
  }

  override def onCommit() : CommitEvent = logic.commit.event
//  override def onCommitLine() =  logic.commit.lineEvent

  val completions = ArrayBuffer[Flow[ScheduleCmd]]()

  override def newSchedulePort(canTrap: Boolean, canJump: Boolean, causeWidth: Int = 4) = completions.addRet(
    Flow(ScheduleCmd(
      canTrap = canTrap,
      canJump = canJump,
      causeWidth = causeWidth,
      pcWidth = PC_WIDTH
    )
  ))
  override def reschedulingPort() = logic.commit.reschedulePort
  override def freePort() = logic.free.port
  override def nextCommitRobId = logic.commit.headNext
  override def currentCommitRobId = logic.commit.head
  override def isRobEmpty = setup.isRobEmpty

  //  TRAP_CAUSE_WIDTH.set(Handle())
  override def rescheduleCauseWidth = {
    setup.get
    (completions.map(_.causeWidth) :+ 4).max
  }

  val setup = create early new Area{
    val jump = getService[JumpService].createJumpInterface(JumpService.Priorities.COMMIT_RESCHEDULE) //Flush missing
    val rob = getService[RobService]
    val robLineMask = rob.newRobLineValids(bypass = ptrCommitRetimed)
    val isRobEmpty = Bool()
    rob.retain()
  }

  val logic = create late new Area {
    val rob = getService[RobService]
//    TRAP_CAUSE_WIDTH.load((completions.map(_.causeWidth) :+ 4).max)

    val ptr = new Area {
      val alloc, commit, free = Reg(UInt(ROB.ID_WIDTH + 1 bits)) init (0)
      val commitRow = commit >> log2Up(ROB.COLS)
      val commitNext = CombInit(commit)
      val allocNext = UInt(ROB.ID_WIDTH + 1 bits)
      commit := commitNext
      alloc := allocNext

      val full = (alloc ^ free) === ROB.SIZE.get
      val empty = RegNext(allocNext === commitNext) init(True) //Retimed to relieve that critical path
      val canFree = free =/= commit

      val robLineMaskRsp = Bits(ROB.COLS bits)
      ptrCommitRetimed match {
        case false => {
          setup.robLineMask.line := commit.resized
          robLineMaskRsp := setup.robLineMask.mask
        }
        case true => {
          setup.robLineMask.line := commitNext.resized
          robLineMaskRsp.setAsReg()
          robLineMaskRsp := setup.robLineMask.mask
        }
      }

      //Manage frontend ROB id allocation
      val frontend = getService[FrontendPlugin]
      val stage = frontend.pipeline.allocated
      stage(ROB.ID) := alloc.resized
      stage.haltIt(full)

      allocNext := alloc + (stage.isFireing ? U(ROB.COLS) | U(0))
      setup.isRobEmpty := empty
    }

    val reschedule = new Area {
      val valid    = Reg(Bool()) init(False)
      val trap     = Reg(Bool())
      val skipCommit = Reg(Bool())
      val robId    = Reg(UInt(ROB.ID_WIDTH bits))
      val pcTarget = Reg(PC)
      val cause    = Reg(UInt(rescheduleCauseWidth bits))
      val tval     = Reg(Bits(Global.XLEN bits))
      val reason   = Reg(ScheduleReason.hardType)
      val commit = new Area{
        val (row, col) = robId.splitAt(log2Up(ROB.COLS))
        val rowHit = valid && U(row) === ptr.commitRow.resized
      }

      val age = robId - ptr.free.resized

      val portsLogic = if(completions.nonEmpty) new Area{
        val perPort = for((c, i) <- completions.zipWithIndex) yield new Area{
          val age = c.robId - ptr.free.resized
          val id = i
          def port = c
        }
        val hits = Bits(completions.size bits)
        val fill = for(self <- perPort) yield new Area {
          val others = ArrayBuffer[(Bool, UInt, Boolean)]() //[valid, age, self has priority] The priority thing is there to ensure double reschedule on the same age is OK
          others += ((valid, age, false))
          others ++= perPort.filter(_.port != self.port).map(o => (o.port.valid, o.age, o.id > self.id))
          hits(self.id) := self.port.valid && others.map(o => !o._1 || (if(o._3) o._2 >= self.age else o._2 > self.age)).andR
        }
        when(hits.orR){
          val canTrap = (0 until completions.size).filter(completions(_).canTrap)
          val canJump = (0 until completions.size).filter(completions(_).canJump)
          valid      := True
          robId      := MuxOH.or(hits, completions.map(_.robId))
          trap       := MuxOH.or(hits, completions.map(_.isTrap))
          pcTarget   := MuxOH.or(canJump.map(hits(_)), canJump.map(completions(_).pcTarget))
          cause      := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).cause.resize(rescheduleCauseWidth bits)))
          reason     := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).reason))
          tval       := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).tval))
          skipCommit := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).skipCommit))
        }
      }
    }

    val commit = new Area {
      val rescheduleHit = False
      val active = rob.readAsync(DISPATCH_MASK, ROB.COLS, ptr.commit.dropHigh(1).asUInt).asBits //TODO can be ignore if schedule width == 1
      val mask = Reg(Bits(ROB.COLS bits)) init ((1 << ROB.COLS) - 1) //Bit set to zero after completion
      val maskComb = CombInit(mask)
      mask := maskComb
      val lineCommited = (maskComb & active) === 0

      val event = CommitEvent()
      event.mask := 0
      event.robId := ptr.commit.resized

      val lineEvent = Flow(CommitEvent())
      lineEvent.valid := False
      lineEvent.mask := active ^ maskComb
      lineEvent.robId := ptr.commit.resized

      val reschedulePort = Flow(RescheduleEvent(rescheduleCauseWidth))
      reschedulePort.valid     := rescheduleHit
      reschedulePort.robId     := reschedule.robId
      reschedulePort.robIdNext := ptr.allocNext.resized
      reschedulePort.trap      := reschedule.trap
      reschedulePort.cause     := reschedule.cause
      reschedulePort.tval      := reschedule.tval
      reschedulePort.reason    := reschedule.reason


      val head = UInt(ROB.ID_WIDTH bits)
      val headNext = CombInit(head)
      head := (ptr.commit + OHToUInt(OHMasking.first(active & mask))).resized

      setup.jump.valid := rescheduleHit
      setup.jump.pc    := reschedule.pcTarget //TODO another target for trap
      reschedule.valid clearWhen(rescheduleHit)

      var continue = True
      when(!ptr.empty) {
        for (colId <- 0 until ROB.COLS) new Area{
          setName(s"CommitPlugin_commit_slot_$colId")
          val enable = mask(colId) && active(colId)
          val readyForCommit = ptr.robLineMaskRsp(colId)
          val rescheduleHitSlot = reschedule.commit.rowHit && reschedule.commit.col === colId

          when(enable){
            when(continue){
              when(readyForCommit && !(rescheduleHitSlot && reschedule.skipCommit)) {
                maskComb(colId) := False
                event.mask(colId) := True
              }
              when(rescheduleHitSlot && (reschedule.skipCommit || readyForCommit)){
                rescheduleHit := True
              }
            }
            when(!readyForCommit || rescheduleHitSlot){
              continue \= False
            }
          }
        }

        when(lineCommited || rescheduleHit) {
          mask := (1 << ROB.COLS) - 1
          lineEvent.valid := True
        }

        when(lineCommited){
          ptr.commitNext := ptr.commit + ROB.COLS
        }
        when(setup.jump.valid){
          ptr.commitNext := ptr.allocNext
          headNext := ptr.allocNext.resized
        }
      }
    }

    val free = new Area{
      val lineEventStream = commit.lineEvent.toStream
      val commited = lineEventStream.queueLowLatency(size = ROB.LINES, latency = 1)
      val robHit = commited.robId === ptr.free.resized
      val hit = commited.valid && robHit
      commited.ready := robHit && ptr.canFree

      val port = Flow(CommitFree())
      port.valid := ptr.canFree
      port.robId := ptr.free.resized
      port.commited := hit ? commited.mask | B(0)
      when(ptr.canFree){
        ptr.free := ptr.free + ROB.COLS
      }
    }

    val cmt = commit
    val rsd = reschedule
    val whitebox = new AreaRoot {
      def patch[T <: Data](that : T) = Verilator.public(CombInit(that))
      val robToPc = new Area {
        val valid = patch(ptr.stage.isFireing)
        val robId = patch(ptr.stage(ROB.ID))
        val pc = (0 until DISPATCH_COUNT).map(i => patch(ptr.stage(PC, i)))
      }
      val commit = patch(cmt.event)
      val reschedule = patch(cmt.reschedulePort)
      val rescheduleReason = patch(rsd.reason)

      Verilator.public(ptr.stage.isFireing)
      Verilator.public(ptr.stage(ROB.ID))
    }

    getService[DocPlugin].property("COMMIT_COUNT", COMMIT_COUNT.get)
    getService[DocPlugin].property("DISPATCH_COUNT", DISPATCH_COUNT)
    rob.release()
  }
}
