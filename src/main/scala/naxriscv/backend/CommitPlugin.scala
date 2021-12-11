package naxriscv.backend

import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK, ROB_ID}
import naxriscv.{Global, ROB}
import naxriscv.interfaces.{AddressTranslationService, CommitEvent, CommitFree, CommitService, JumpService, RescheduleEvent, RfAllocationService, RobService, ScheduleCmd}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.lib._
import naxriscv.Global._
import naxriscv.frontend.FrontendPlugin

import scala.collection.mutable.ArrayBuffer

class CommitPlugin extends Plugin with CommitService{
  override def onCommit() : CommitEvent = logic.commit.event
//  override def onCommitLine() =  logic.commit.lineEvent

  val completions = ArrayBuffer[Flow[ScheduleCmd]]()
  override def newSchedulePort(canTrap : Boolean, canJump : Boolean) = completions.addRet(Flow(ScheduleCmd(canTrap = canTrap, canJump = canJump, pcWidth = widthOf(getService[AddressTranslationService].PC))))
  override def reschedulingPort() = logic.commit.reschedulePort
  override def freePort() = logic.free.port
  override def nextCommitRobId = logic.commit.head

  val setup = create early new Area{
    val jump = getService[JumpService].createJumpInterface(JumpService.Priorities.COMMIT) //Flush missing
    val rob = getService[RobService]
    val robLineMask = rob.newRobLineValids()
    rob.retain()
  }

  val logic = create late new Area {
    val rob = getService[RobService]
    val PC = getService[AddressTranslationService].PC

    val ptr = new Area {
      val alloc, commit, free = Reg(UInt(ROB.ID_WIDTH + 1 bits)) init (0)
      val full = (alloc ^ free) === ROB.SIZE.get
      val empty = alloc === commit
      val canFree = free =/= commit
      val commitRow = commit >> log2Up(ROB.COLS)

      setup.robLineMask.line := commit.resized

      //Manage frontend ROB id allocation
      val frontend = getService[FrontendPlugin]
      val stage = frontend.pipeline.allocated
      stage(ROB_ID) := alloc.resized
      stage.haltIt(full)

      val whitebox = new Area{
        setName("robToPc")
        val valid = Verilator.public(CombInit(stage.isFireing))
        val robId = Verilator.public(CombInit(stage(ROB_ID)))
        val pc = (0 until DISPATCH_COUNT).map(i => Verilator.public(CombInit(stage(PC, i))))
      }

      val allocNext = alloc + (stage.isFireing ? U(ROB.COLS) | U(0))
      alloc := allocNext
    }

    val reschedule = new Area {
      val valid    = Reg(Bool()) init(False)
      val trap     = Reg(Bool())
      val skipCommit = Reg(Bool())
      val robId    = Reg(UInt(ROB.ID_WIDTH bits))
      val pcTarget = Reg(PC)
      val cause    = Reg(UInt(Global.TRAP_CAUSE_WIDTH bits))
      val tval     = Reg(Bits(Global.XLEN bits))
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
          cause      := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).cause))
          tval       := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).tval))
          skipCommit := MuxOH.or(canTrap.map(hits(_)), canTrap.map(completions(_).skipCommit))
        }
      }
    }

    val commit = new Area {
      var continue = True
      val rescheduleHit = False
      val active = rob.readAsync(DISPATCH_MASK, ROB.COLS, ptr.commit.dropHigh(1).asUInt).asBits //TODO can be ignore if schedule width == 1
      val mask = Reg(Bits(ROB.COLS bits)) init ((1 << ROB.COLS) - 1) //Bit set to zero after completion
      val maskComb = CombInit(mask)
      mask := maskComb
      val lineCommited = (maskComb & active) === 0

      val event = CommitEvent()
      event.setName("commit").addAttribute(Verilator.public)
      event.mask := 0
      event.robId := ptr.commit.resized

      val lineEvent = Flow(CommitEvent())
      lineEvent.valid := False
      lineEvent.mask := active ^ maskComb
      lineEvent.robId := ptr.commit.resized

      val reschedulePort = Flow(RescheduleEvent())
      reschedulePort.setName("reschedule").addAttribute(Verilator.public)
      reschedulePort.valid := rescheduleHit
      reschedulePort.nextRob := ptr.allocNext.resized

      val head = Reg(UInt(ROB.ID_WIDTH bits)) init(0)
      head := head + CountOne(event.mask.asBools)

      setup.jump.valid := rescheduleHit
      setup.jump.pc := reschedule.pcTarget //TODO another target for trap
      reschedule.valid clearWhen(rescheduleHit)

      when(!ptr.empty) {
        for (colId <- 0 until ROB.COLS) new Area{
          setName(s"CommitPlugin_commit_slot_$colId")
          val enable = mask(colId) && active(colId)
          val hold = !setup.robLineMask.mask(colId)
          val rescheduleHitSlot = reschedule.commit.rowHit && reschedule.commit.col === colId

          when(enable){
            when(continue){
              when(!hold && !(rescheduleHitSlot && reschedule.skipCommit)) {
                maskComb(colId) := False
                event.mask(colId) := True
              }
              when(rescheduleHitSlot){
                rescheduleHit := True
              }
            }
            when(hold || rescheduleHitSlot){
              continue \= False
            }
          }
        }
        when(lineCommited || rescheduleHit) {
          mask := (1 << ROB.COLS) - 1
          lineEvent.valid := True
        }

        when(lineCommited){
          ptr.commit := ptr.commit + ROB.COLS
        }
        when(setup.jump.valid){
          ptr.commit := ptr.allocNext
          head := ptr.allocNext.resized
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


    getService[DocPlugin].property("COMMIT_COUNT", COMMIT_COUNT.get)
    getService[DocPlugin].property("DISPATCH_COUNT", DISPATCH_COUNT)
    rob.release()
  }
}
