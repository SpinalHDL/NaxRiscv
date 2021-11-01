package naxriscv.backend

import naxriscv.Frontend.ROB_ID
import naxriscv.{Global, ROB}
import naxriscv.interfaces.{CommitService, CompletionCmd, JumpService, RescheduleCmd, RfAllocationService, RobService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import naxriscv.Global._
import naxriscv.frontend.FrontendPlugin

import scala.collection.mutable.ArrayBuffer

class CommitPlugin extends Plugin with CommitService{
  override def onCommit() = ???

  val completions = ArrayBuffer[Flow[CompletionCmd]]()
  override def newCompletionPort(canTrap : Boolean, canJump : Boolean) = {
    val port = Flow(CompletionCmd(canTrap = canTrap, canJump = canJump))
    completions += port
    port
  }
  override def reschedulingPort() = logic.reschedule.port


  val setup = create early new Area{
    val jump = getService[JumpService].createJumpInterface(JumpService.Priorities.COMMIT) //Flush missing
    val rob = getService[RobService]
    val robLineMask = rob.robLineValids()
  }

  val logic = create late new Area {
    val ptr = new Area {
      val alloc, commit, free = Reg(UInt(ROB.ID_WIDTH + 1 bits)) init (0)
      val full = (alloc ^ free) === ROB.SIZE.get
      val empty = alloc === commit
      val canFree = free =/= commit
      val commitLine = commit >> log2Up(ROB.COLS.get)

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
      val trap = Reg(Bool())
      val age = Reg(UInt(ROB.ID_WIDTH bits))
      val pcTarget = Reg(Global.PC)
      val commit = new Area{
        val (row, line) = age.splitAt(log2Up(ROB.COLS.get))
        val lineHit = age >> log2Up(ROB.COLS.get) === ptr.commitLine
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
      val mask = Reg(Bits(ROB.COLS bits)) init ((1 << ROB.COLS) - 1)
      val maskComb = CombInit(mask)
      mask := maskComb

      setup.jump.valid := False
      setup.jump.payload.assignDontCare()
      when(!ptr.empty) {
        for (rowId <- 0 until ROB.COLS) {
          when(setup.robLineMask.mask(rowId) && mask(rowId) && continue) {
            maskComb(rowId) := False
            when(reschedule.valid && reschedule.commit.lineHit && reschedule.commit.row === rowId){
              continue \= False
              force := True
              setup.jump.valid := True
              setup.jump.pc := reschedule.pcTarget
              ptr.commit := ptr.alloc //TODO likely buggy
            }
          }
        }
        when(maskComb === 0 || force) {
          mask := (1 << ROB.COLS) - 1
          ptr.commit := ptr.commit + ROB.COLS.get
        }
      }
    }

    val free = new Area{
      val allocator = getService[RfAllocationService].getFreePort()
      for(port <- allocator){
        port.valid := False
        port.payload := 0
      }
      when(ptr.canFree){
        ptr.free := ptr.free + ROB.COLS.get
      }
    }


    val completion = for(c <- completions) yield new Area{
      when(c.valid){

      }
    }
  }
}
