package naxriscv.execute

import naxriscv.Frontend
import naxriscv.interfaces.{CsrOnRead, CsrOnReadData, CsrOnWrite, CsrService, DecoderService, MicroOp, RS1}
import naxriscv.riscv.{Const, IMM, IntRegFile, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities._
import spinal.lib.pipeline._
import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.misc.CommitPlugin

object CsrPlugin extends AreaObject{
  val CSR_IMM  = Stageable(Bool())
  val CSR_MASK = Stageable(Bool())
  val CSR_CLEAR = Stageable(Bool())
  val CSR_WRITE = Stageable(Bool())
  val CSR_READ = Stageable(Bool())
  val CSR_VALUE = Stageable(Bits(XLEN bits))
  val ALU_INPUT = Stageable(Bits(XLEN bits))
  val ALU_MASK = Stageable(Bits(XLEN bits))
  val ALU_MASKED = Stageable(Bits(XLEN bits))
  val ALU_RESULT = Stageable(Bits(XLEN bits))
}

class CsrPlugin(euId : String,
                writebackAt: Int,
                staticLatency : Boolean) extends ExecutionUnitElementSimple(euId, staticLatency) with CsrService{
  override def euWritebackAt = writebackAt


  override def onReadHalt() = setup.onReadHalt
  override def onWriteHalt() = setup.onWriteHalt
  override def onWriteBits = setup.onWriteBits

  import CsrPlugin._

  val setup = create early new Setup{
    val onReadHalt  = False
    val onWriteHalt = False
    val onWriteBits = Bits(XLEN bits)

    add(Rvi.CSRRW , Nil, eu.DecodeList(CSR_IMM -> False, CSR_MASK -> False))
    add(Rvi.CSRRS , Nil, eu.DecodeList(CSR_IMM -> False, CSR_MASK -> True , CSR_CLEAR -> False))
    add(Rvi.CSRRC , Nil, eu.DecodeList(CSR_IMM -> False, CSR_MASK -> True , CSR_CLEAR -> True))
    add(Rvi.CSRRWI, Nil, eu.DecodeList(CSR_IMM -> True , CSR_MASK -> False))
    add(Rvi.CSRRSI, Nil, eu.DecodeList(CSR_IMM -> True , CSR_MASK -> True , CSR_CLEAR -> False))
    add(Rvi.CSRRCI, Nil, eu.DecodeList(CSR_IMM -> True , CSR_MASK -> True , CSR_CLEAR -> True))
  }

  val logic = create late new Logic{
    lock.await()

    val decoder = getService[DecoderService]

    val feed = new ExecuteArea(writebackAt) { //TODO
      import stage._
      val imm = IMM(MICRO_OP)
      val immZero = imm.z === 0
      val srcZero = CSR_IMM ? immZero otherwise MICRO_OP(Const.rs1Range) === 0
      CSR_WRITE := !(CSR_MASK && srcZero)
      CSR_READ := !(!CSR_MASK && !decoder.WRITE_RD)

      val specLogic = new Area {
        val grouped = spec.groupByLinked(_.csrFilter)
        val sels = grouped.map(_._1 -> Stageable(Bool()))
        sels.foreach{
          case (filter : Int, stageable) => stageable := MICRO_OP(Const.csrRange) === filter
        }

        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area{
          val onReads  = elements.collect{ case e : CsrOnRead  => e }
          val onWrites = elements.collect{ case e : CsrOnWrite => e }
          val onReadsData = elements.collect{ case e : CsrOnReadData => e }
          val onReadsFire  = onReads.filter(_.onlyOnFire)
          val onWritesFire = onWrites.filter(_.onlyOnFire)

          if(onReads.nonEmpty)      when(isValid   && CSR_READ  && sels(csrFilter)){ onReads.foreach(_.body()) }
          if(onWrites.nonEmpty)     when(isValid   && CSR_WRITE && sels(csrFilter)){ onWrites.foreach(_.body()) }
          if(onReadsFire.nonEmpty)  when(isFireing && CSR_READ  && sels(csrFilter)){ onReadsFire.foreach(_.body()) }
          if(onWritesFire.nonEmpty) when(isFireing && CSR_WRITE && sels(csrFilter)){ onWritesFire.foreach(_.body()) }

          val read = onReadsData.nonEmpty generate new Area {
            val value = B(0, XLEN bits)
            onReadsData.foreach (e => value.assignFromBits(e.value.asBits, e.bitOffset, widthOf(e.value) bits) )
            val masked = value.andMask(sels(csrFilter))
          }
        }

        if(groupedLogic.exists(_.onReadsData.nonEmpty)){
          CSR_VALUE := groupedLogic.filter(_.onReadsData.nonEmpty).map(_.read.value).toList.reduceBalancedTree(_ | _)
        } else {
          CSR_VALUE := 0
        }
      }

      ALU_INPUT := CSR_VALUE
      ALU_MASK := CSR_MASK ? imm.z.resized | eu(IntRegFile, RS1)
      ALU_MASKED := CSR_CLEAR ? (ALU_INPUT & ~ALU_MASK) otherwise (ALU_INPUT | ALU_MASK)
      ALU_RESULT := CSR_MASK ? stage(ALU_MASKED) otherwise ALU_INPUT

      wb.payload := ALU_RESULT
    }
  }
}
