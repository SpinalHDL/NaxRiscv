package naxriscv.execute

import naxriscv.{DecodeList, Frontend, ROB}
import naxriscv.interfaces.{CommitService, CsrOnRead, CsrOnReadData, CsrOnWrite, CsrService, DecoderService, MicroOp, RS1, ScheduleReason}
import naxriscv.riscv.{CSR, Const, IMM, IntRegFile, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities._
import spinal.lib.pipeline._
import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.frontend.DispatchPlugin
import naxriscv.misc.CommitPlugin

object CsrAccessPlugin extends AreaObject{
  val CSR_IMM  = Stageable(Bool())
  val CSR_MASK = Stageable(Bool())
  val CSR_CLEAR = Stageable(Bool())
  val CSR_WRITE = Stageable(Bool())
  val CSR_READ = Stageable(Bool())
  val CSR_READ_TRAP = Stageable(Bool())
  val CSR_WRITE_TRAP = Stageable(Bool())
  val CSR_VALUE = Stageable(Bits(XLEN bits))
  val ALU_INPUT = Stageable(Bits(XLEN bits))
  val ALU_MASK = Stageable(Bits(XLEN bits))
  val ALU_MASKED = Stageable(Bits(XLEN bits))
  val ALU_RESULT = Stageable(Bits(XLEN bits))
}

class CsrAccessPlugin(euId : String,
                      writebackAt: Int,
                      staticLatency : Boolean) extends ExecutionUnitElementSimple(euId, staticLatency) with CsrService{
  override def euWritebackAt = writebackAt

  override def onReadHalt()   = setup.onReadHalt   := True
  override def onWriteHalt()  = setup.onWriteHalt  := True
  override def onReadTrap()  = setup.onReadTrap  := True
  override def onWriteTrap() = setup.onWriteTrap := True
  override def onWriteBits = setup.onWriteBits

  import CsrAccessPlugin._
  val setup = create early new Setup{
    val dispatch = getService[DispatchPlugin]
    assert(getServicesOf[CsrService].size == 1)


    val onReadHalt  = False
    val onWriteHalt = False
    val onReadTrap  = False
    val onWriteTrap = False
    val onWriteBits = Bits(XLEN bits)

    add(Rvi.CSRRW , Nil, DecodeList(CSR_IMM -> False, CSR_MASK -> False))
    add(Rvi.CSRRS , Nil, DecodeList(CSR_IMM -> False, CSR_MASK -> True , CSR_CLEAR -> False))
    add(Rvi.CSRRC , Nil, DecodeList(CSR_IMM -> False, CSR_MASK -> True , CSR_CLEAR -> True))
    add(Rvi.CSRRWI, Nil, DecodeList(CSR_IMM -> True , CSR_MASK -> False))
    add(Rvi.CSRRSI, Nil, DecodeList(CSR_IMM -> True , CSR_MASK -> True , CSR_CLEAR -> False))
    add(Rvi.CSRRCI, Nil, DecodeList(CSR_IMM -> True , CSR_MASK -> True , CSR_CLEAR -> True))

    for(op <- List(Rvi.CSRRW, Rvi.CSRRS, Rvi.CSRRC, Rvi.CSRRWI, Rvi.CSRRSI, Rvi.CSRRCI)){
      dispatch.fenceYounger(op)
      dispatch.fenceOlder(op)
    }

    val commit = getService[CommitService]
    val trap = commit.newSchedulePort(canTrap = true, canJump = false)
  }

  val logic = create late new Logic{
    lock.await()

    val decoder = getService[DecoderService]

    val miaou = new ExecuteArea(writebackAt) { //TODO
      import stage._
      val imm = IMM(MICRO_OP)
      val immZero = imm.z === 0
      val srcZero = CSR_IMM ? immZero otherwise MICRO_OP(Const.rs1Range) === 0
      CSR_WRITE := !(CSR_MASK && srcZero)
      CSR_READ := !(!CSR_MASK && !decoder.WRITE_RD)

      def filterToName(filter : Any) = filter match{
        case f : Int => f.toString
      }

      val specLogic = new Area {
        val grouped = spec.groupByLinked(_.csrFilter)
        val sels = grouped.map(g => g._1 -> Stageable(Bool()).setName("CSR_" + filterToName(g._1)))
        sels.foreach{
          case (filter : Int, stageable) => stageable := MICRO_OP(Const.csrRange) === filter
        }

        CSR_WRITE_TRAP := setup.onWriteTrap
        CSR_READ_TRAP := setup.onReadTrap

        val onReadsDo = isValid && CSR_READ
        val onWritesDo = isValid && CSR_WRITE
        val onReadsFireDo = isFireing && CSR_READ && !CSR_READ_TRAP
        val onWritesFireDo = isFireing && CSR_WRITE && !CSR_WRITE_TRAP && !CSR_READ_TRAP

        haltIt(onReadsDo && setup.onReadHalt)
        haltIt(onWritesDo && setup.onWriteHalt)

        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area{
          setPartialName(filterToName(csrFilter))

          val onReads  = elements.collect{ case e : CsrOnRead  => e }
          val onWrites = elements.collect{ case e : CsrOnWrite => e }
          val onReadsData = elements.collect{ case e : CsrOnReadData => e }
          val onReadsAlways  = onReads.filter(!_.onlyOnFire)
          val onWritesAlways = onWrites.filter(!_.onlyOnFire)
          val onReadsFire  = onReads.filter(_.onlyOnFire)
          val onWritesFire = onWrites.filter(_.onlyOnFire)

          if(onReadsAlways.nonEmpty)  when(onReadsDo      && sels(csrFilter)){ onReadsAlways.foreach(_.body()) }
          if(onWritesAlways.nonEmpty) when(onWritesDo     && sels(csrFilter)){ onWritesAlways.foreach(_.body()) }
          if(onReadsFire.nonEmpty)   when(onReadsFireDo  && sels(csrFilter)){ onReadsFire.foreach(_.body()) }
          if(onWritesFire.nonEmpty)  when(onWritesFireDo && sels(csrFilter)){ onWritesFire.foreach(_.body()) }

          val read = onReadsData.nonEmpty generate new Area {
            val bitCount = onReadsData.map(e => widthOf(e.value)).sum
            assert(bitCount <= XLEN.get)

            val value = if(bitCount != XLEN.get) B(0, XLEN bits) else Bits(XLEN bits)
            onReadsData.foreach (e => value.assignFromBits(e.value.asBits, e.bitOffset, widthOf(e.value) bits) )
            val masked = value.andMask(sels(csrFilter))
          }
        }

        if(groupedLogic.exists(_.onReadsData.nonEmpty)){
          CSR_VALUE := groupedLogic.filter(_.onReadsData.nonEmpty).map(_.read.masked).toList.reduceBalancedTree(_ | _)
        } else {
          CSR_VALUE := 0
        }
      }

      ALU_INPUT := CSR_VALUE
      ALU_MASK := CSR_MASK ? imm.z.resized | eu(IntRegFile, RS1)
      ALU_MASKED := CSR_CLEAR ? (ALU_INPUT & ~ALU_MASK) otherwise (ALU_INPUT | ALU_MASK)
      ALU_RESULT := CSR_MASK ? stage(ALU_MASKED) otherwise ALU_MASK

      setup.onWriteBits := ALU_RESULT
      wb.payload := CSR_VALUE

      setup.trap.valid      := isValid && (CSR_READ_TRAP || CSR_WRITE_TRAP)
      setup.trap.robId      := ROB.ID
      setup.trap.cause      := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      setup.trap.tval       := MICRO_OP
      setup.trap.skipCommit := True
      setup.trap.reason     := ScheduleReason.TRAP
    }

    val whitebox = new AreaRoot{
      val csrWrite = Verilator.public(Flow(new Bundle {
        val robId = ROB.ID()
        val address = UInt(12 bits)
        val data = Bits(XLEN bits)
      }))
      csrWrite.valid := miaou.stage.isFireing
      csrWrite.robId := miaou.stage(ROB.ID)
      csrWrite.address := U(miaou.stage(MICRO_OP)(Const.csrRange))
      csrWrite.data := setup.onWriteBits
    }
  }
}
