package naxriscv.backend

import naxriscv.{Global, ROB}
import naxriscv.interfaces.{CommitService, JumpService, RfAllocationService, RobService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import naxriscv.Global._

class CommitPlugin extends Plugin with CommitService{
  override def onCommit() = ???
  override def newCompletionPort() = ???

  override def rollback() = False

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
      val commitLine = commit >> log2Up(ROB.ROWS.get)

      setup.robLineMask.line := commit.resized
    }

    val reschedule = new Area {
      val valid = Reg(Bool()) init(False)
      val trap = Reg(Bool())
      val age = Reg(UInt(ROB.ID_WIDTH bits))
      val pcTarget = Reg(Global.PC)
      val commit = new Area{
        val (row, line) = age.splitAt(log2Up(ROB.ROWS.get))
        val lineHit = age >> log2Up(ROB.ROWS.get) === ptr.commitLine
      }

      //TODO remove
      age := 0
      pcTarget := 0
      trap := False
    }

    val commit = new Area {
      var continue = True
      val force = False
      val mask = Reg(Bits(ROB.ROWS bits)) init ((1 << ROB.ROWS) - 1)
      val maskComb = CombInit(mask)
      mask := maskComb

      setup.jump.valid := False
      setup.jump.payload.assignDontCare()
      when(!ptr.empty) {
        for (rowId <- 0 until ROB.ROWS) {
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
          mask := (1 << ROB.ROWS) - 1
          ptr.commit := ptr.commit + ROB.ROWS.get
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
        ptr.free := ptr.free + ROB.ROWS.get
      }
    }
  }
}
