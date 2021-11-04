package naxriscv.backend

import naxriscv.Frontend.{DISPATCH_MASK, ROB_ID}
import naxriscv.{Global, ROB}
import naxriscv.interfaces.{CommitEvent, CommitFree, CommitService, CompletionCmd, JumpService, RescheduleCmd, RfAllocationService, RobService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import naxriscv.Global._
import naxriscv.frontend.FrontendPlugin

import scala.collection.mutable.ArrayBuffer

class CommitPlugin extends Plugin with CommitService{
  override def onCommit() : CommitEvent = logic.commit.event
  override def onCommitLine() =  logic.commit.lineEvent

  val completions = ArrayBuffer[Flow[CompletionCmd]]()
  override def newCompletionPort(canTrap : Boolean, canJump : Boolean) = {
    val port = Flow(CompletionCmd(canTrap = canTrap, canJump = canJump))
    completions += port
    port
  }
  override def reschedulingPort() = logic.reschedule.port
  override def freePort() = logic.free.port

  val setup = create early new Area{
    val jump = getService[JumpService].createJumpInterface(JumpService.Priorities.COMMIT) //Flush missing
    val rob = getService[RobService]
    val robLineMask = rob.robLineValids()
    rob.retain()
  }

  val logic = create late new Area {
    val rob = getService[RobService]

    val ptr = new Area {
      val alloc, commit, free = Reg(UInt(ROB.ID_WIDTH + 1 bits)) init (0)
      val full = (alloc ^ free) === ROB.SIZE.get
      val empty = alloc === commit
      val canFree = free =/= commit
      val commitLine = commit >> log2Up(ROB.COLS)

      setup.robLineMask.line := commit.resized

      //Manage frontend ROB id allocation
      val frontend = getService[FrontendPlugin]
      val stage = frontend.pipeline.allocated
      stage(ROB_ID) := alloc.resized
      stage.haltIt(full)

      val allocNext = alloc + (stage.isFireing ? U(ROB.COLS) | U(0))
      alloc := allocNext
    }

    val reschedule = new Area {
      val valid = Reg(Bool()) init(False)
      valid := False //TODO
      val trap = Reg(Bool())
      val age = Reg(UInt(ROB.ID_WIDTH bits))
      val pcTarget = Reg(Global.PC)
      val commit = new Area{
        val (row, line) = age.splitAt(log2Up(ROB.COLS))
        val lineHit = age >> log2Up(ROB.COLS) === ptr.commitLine
      }

      val port = Flow(RescheduleCmd())
      port.valid := False
      port.nextRob := ptr.allocNext.resized

      //TODO remove
      age := 0
      pcTarget := 0
      trap := False
    }

    val commit = new Area {
      var continue = True
      val force = False
      val active = rob.readAsync(DISPATCH_MASK, ROB.COLS, ptr.commit.dropHigh(1).asUInt).asBits //TODO can be ignore if schedule width == 1
      val mask = Reg(Bits(ROB.COLS bits)) init ((1 << ROB.COLS) - 1)
      val maskComb = CombInit(mask)
      mask := maskComb

      val event = CommitEvent()
      event.mask := 0
      event.robId := ptr.commit.resized


      val lineEvent = Flow(CommitEvent())
      lineEvent.valid := False
      lineEvent.mask := active ^ maskComb
      lineEvent.robId := ptr.commit.resized

      setup.jump.valid := False
      setup.jump.payload.assignDontCare()
      when(!ptr.empty) {
        for (rowId <- 0 until ROB.COLS) {
          when(setup.robLineMask.mask(rowId) && mask(rowId) && active(rowId) && continue) {
            maskComb(rowId) := False
            when(reschedule.valid && reschedule.commit.lineHit && reschedule.commit.row === rowId){
              continue \= False
              force := True
              setup.jump.valid := True
              setup.jump.pc := reschedule.pcTarget
              ptr.commit := ptr.alloc //TODO likely buggy
              event.mask(rowId) := True
            }
          }
        }
        when((maskComb & active) === 0 || force) {
          mask := (1 << ROB.COLS) - 1
          ptr.commit := ptr.commit + ROB.COLS
          lineEvent.valid := True
        }
      }
    }

    val free = new Area{
      val lineEventStream = commit.lineEvent.toStream
      val commited = lineEventStream.queueLowLatency(size = ROB.LINES, latency = 1)
      val hit = commited.valid && commited.robId === ptr.free
      commited.ready := ptr.canFree

      val port = Flow(CommitFree())
      port.valid := ptr.canFree
      port.robId := ptr.free.resized
      port.commited := hit ? commited.mask | B(0)
      when(ptr.canFree){
        ptr.free := ptr.free + ROB.COLS
      }
    }


    val completion = for(c <- completions) yield new Area{
      when(c.valid){

      }
    }

    rob.release()
  }
}
