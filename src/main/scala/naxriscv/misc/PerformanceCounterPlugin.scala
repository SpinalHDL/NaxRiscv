package naxriscv.misc

import naxriscv.Global._
import naxriscv.execute.CsrAccessPlugin
import naxriscv.interfaces.{CommitService, CsrListFilter, LockedImpl}
import naxriscv.riscv.CSR
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{Plugin, Service}
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer


trait PerformanceCounterService extends Service with LockedImpl{
  def addEvent(id : Int, event : Bool) : Unit
}

class PerformanceCounterPlugin(additionalCounterCount : Int,
                               bufferWidth : Int = 7) extends Plugin with PerformanceCounterService{
  val counterCount = 2 + (if(additionalCounterCount != 0) additionalCounterCount + 1 else 0)

  override def addEvent(id: Int, event: Bool) = ???

  val setup = create early new Area{
    val csr = getService[CsrAccessPlugin]
    val ram = getService[CsrRamPlugin]
    csr.retain()

    val withHigh = XLEN.get == 32
    val readPort = ram.ramReadPort()
    val writePort = ram.ramWritePort()
    val allocation = ram.ramAllocate(entries = counterCount*(if(withHigh) 2 else 1))
  }

  val logic = create late new Area{
    lock.await()
    val s = setup.get
    import s._

    val csrList = ArrayBuffer[Int]()
    csrList ++= List(CSR.MCYCLE, CSR.MINSTRET)
    csrList ++= CSR.MHPMCOUNTER3 until CSR.MHPMCOUNTER3 + additionalCounterCount
    if(withHigh) csrList ++= csrList.map(_ + 0x80)
    val csrFilter = CsrListFilter(csrList.toList)

    val commitMask = getService[CommitService].onCommit().mask
    val commitCount = CountOne(commitMask)
    val ignoreNextCommit = RegInit(False)
    when(ignoreNextCommit){
      commitCount := 0
      ignoreNextCommit clearWhen(commitMask.orR)
    }

    val slots = for(id <- 0 until counterCount) yield new Area{
      val counter = Reg(UInt(bufferWidth bits)) init(0)
      val needFlush = counter.msb

      id match {
        case 0 =>
          setPartialName("cycle")
          counter := counter + 1
        case 2 =>
          setPartialName("instret")
          counter := counter + RegNext(commitCount).init(0)
        case _ => setPartialName(s"mhpmcounter$id")
      }
    }


    val fsm = new StateMachine{
      val IDLE, READ_LOW, CALC, WRITE_LOW = new State
      val READ_HIGH, WRITE_HIGH = withHigh generate new State
      setEntry(IDLE)

      val csrReadCmd, flusherCmd = Stream(new Bundle {
        val address = UInt(log2Up(counterCount) bits)
      })
      val csrWriteCmd = Stream(new Bundle {
        val address = UInt(log2Up(counterCount) bits)
        val high = withHigh generate Bool()
      })
      List(csrReadCmd, flusherCmd, csrWriteCmd).foreach(_.ready := False)

      val cmd = new Area {
        val flusher = Reg(Bool())
        val address = Reg(UInt(log2Up(counterCount) bits))
      }

      readPort.valid := False
      readPort.address := allocation.getAddress(cmd.address.resized)

      writePort.valid := False
      writePort.address := allocation.getAddress(cmd.address.resized)
      writePort.data.assignDontCare()

      val done = RegInit(False)
      val resultCsr = Reg(UInt(64 bits))
      val result = Reg(UInt(64 bits))
      val ramReaded = Reg(UInt(64 bits))
      val counterReaded = Reg(UInt(bufferWidth bits))

      val calc = (ramReaded(ramReaded.high downto bufferWidth-1) + U(counterReaded.msb)) @@ counterReaded.resize(bufferWidth-1)

      IDLE whenIsActive{
        when(flusherCmd.valid){
          cmd.flusher := True
          cmd.address := flusherCmd.address
          flusherCmd.ready := True
          goto(READ_LOW)
        } elsewhen(csrReadCmd.valid){
          cmd.flusher := False
          cmd.address := csrReadCmd.address
          csrReadCmd.ready := True
          goto(READ_LOW)
        } elsewhen(csrWriteCmd.valid){
          writePort.valid := True
          writePort.address := allocation.getAddress(csrWriteCmd.address.resized)
          if(withHigh) writePort.address(log2Up(counterCount)) := csrWriteCmd.high
          writePort.data := csr.onWriteBits
          when(!csrWriteCmd.high) {
            whenIndexed(slots, csrWriteCmd.address) { slot =>
              slot.counter := csr.onWriteBits.asUInt.resized
              slot.counter.msb := False
            }
          }
          csrWriteCmd.ready := writePort.ready
          ignoreNextCommit setWhen(csrWriteCmd.address === 2) // && (if(withHigh) !csrWriteCmd.high else True)
        }
      }
      READ_LOW.whenIsActive{
        readPort.valid := True
        ramReaded(0, XLEN bits) := U(readPort.data)
        counterReaded := slots.map(_.counter).read(cmd.address.resized)
        when(readPort.ready) {
          if(withHigh) goto(READ_HIGH) else goto(CALC)
        }
      }

      if(withHigh) READ_HIGH.whenIsActive{
        readPort.valid := True
        readPort.address(log2Up(counterCount)) := True

        ramReaded(XLEN, XLEN bits) := U(readPort.data)
        when(readPort.ready) {
          goto(CALC)
        }
      }

      CALC.whenIsActive{
        result := calc
        when(counterReaded.msb){
          whenIndexed(slots, cmd.address)(_.counter.msb := False)
        }
        when(cmd.flusher) {
          goto(WRITE_LOW)
        } otherwise {
          resultCsr := calc
          done := True
          goto(IDLE)
        }
      }

      WRITE_LOW whenIsActive{
        writePort.valid := True
        writePort.data := B(result).resized
        when(writePort.ready) {
          if(withHigh) goto(WRITE_HIGH) else goto(IDLE)
        }
      }


      WRITE_HIGH whenIsActive{
        writePort.valid := True
        writePort.address(log2Up(counterCount)) := True
        writePort.data := B(result >> 32)
        when(writePort.ready) {
          goto(IDLE)
        }
      }
    }

    val flusher = new Area{
      val hits = B(slots.map(_.needFlush))
      val hit = hits.orR
      val oh = OHMasking.first(hits)
      val sel = OHToUInt(oh)

      fsm.flusherCmd.valid := hit
      fsm.flusherCmd.address := sel
    }

    val csrRead = new Area {
      val fired = RegInit(False) setWhen(fsm.csrReadCmd.fire)
      val requested = csr.isReading(csrFilter)
      fsm.csrReadCmd.valid := requested && !fired
      fsm.csrReadCmd.address := csr.onReadAddress(0, log2Up(counterCount) bits)
      if(withHigh)
        csr.read(fsm.resultCsr.subdivideIn(2 slices).read(csr.onReadAddress(7).asUInt), csrFilter)
      else
        csr.read(fsm.resultCsr, csrFilter)

      when(requested){
        when(!fsm.done){
          csr.onReadHalt()
        } otherwise {
          fsm.done := False
        }
      }
      when(csr.onReadMovingOff){
        fired := False
        fsm.done := False
      }
    }

    val csrWrite = new Area{
      val fired = RegInit(False) setWhen(fsm.csrWriteCmd.fire)
      fsm.csrWriteCmd.address := csr.onWriteAddress(0, log2Up(counterCount) bits)
      fsm.csrWriteCmd.high := csr.onWriteAddress(7)
      fsm.csrWriteCmd.valid := False

      csr.onWrite(csrFilter, false){
        when(!fired){
          fsm.csrWriteCmd.valid := True
          when(!fsm.csrWriteCmd.ready){
            csr.onWriteHalt()
          }
        }
      }
      when(csr.onWriteMovingOff){
        fired := False
      }
    }
    csr.release()
  }
}
