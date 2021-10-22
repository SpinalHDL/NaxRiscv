package naxriscv.backend

import naxriscv.Global
import naxriscv.interfaces.{JumpService, RobService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import naxriscv.Global._

class CommitPlugin extends Plugin{

  val setup = create early new Area{
    val rob = getService[RobService].robPopLine()
    val jump = getService[JumpService].createJumpInterface(JumpService.Priorities.COMMIT) //Flush missing
  }

  val logic = create late new Area {
    val ptr = new Area {
      val alloc, commit, free = Reg(UInt(ROB_ID_WIDTH + 1 bits)) init (0)
      val full = (alloc ^ free) === ROB_SIZE.get
      val empty = alloc === commit
      val canFree = free =/= commit
      val commitLine = commit >> log2Up(ROB_ROWS.get)
    }

    val reschedule = new Area {
      val valid = Reg(Bool()) init(False)
      val trap = Reg(Bool())
      val age = Reg(UInt(ROB_ID_WIDTH bits))
      val pcTarget = Reg(Global.PC)
      val commit = new Area{
        val (row, line) = age.splitAt(log2Up(ROB_ROWS.get))
        val lineHit = age >> log2Up(ROB_ROWS.get) === ptr.commitLine
      }
    }

    val commit = new Area {
      var continue = True
      val force = False
      val mask = Reg(Bits(ROB_ROWS bits)) init ((1 << ROB_ROWS) - 1)
      val maskComb = CombInit(mask)
      mask := maskComb

      setup.jump.valid := False
      setup.jump.payload.assignDontCare()
      when(!ptr.empty) {
        for (rowId <- 0 until ROB_ROWS) {
          val entry = setup.rob.entries(rowId)
          when(entry.valid && mask(rowId) && continue) {
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
          mask := (1 << ROB_ROWS) - 1
          ptr.commit := ptr.commit + 1
        }
      }
    }

    val free = new Area{
      when(ptr.canFree){
        ptr.free := ptr.free + 1
      }
    }
  }
}
