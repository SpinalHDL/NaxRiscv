package naxriscv.execute

import naxriscv.{DecodeList, Frontend, ROB}
import naxriscv.interfaces._
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
  val RAM_SEL    = Stageable(Bool())
}

class CsrAccessPlugin(euId: String)(decodeAt: Int,
                                    readAt: Int,
                                    writeAt: Int,
                                    writebackAt: Int,
                                    staticLatency: Boolean) extends ExecutionUnitElementSimple(euId, staticLatency) with CsrService{
  override def euWritebackAt = writebackAt

  override def onReadHalt()   = setup.onReadHalt   := True
  override def onWriteHalt()  = setup.onWriteHalt  := True
  override def onReadTrap()  = setup.onReadTrap  := True
  override def onWriteTrap() = setup.onWriteTrap := True
  override def onWriteBits = setup.onWriteBits
  override def getCsrRam() = getService[CsrRamService]

  import CsrAccessPlugin._

  val setup = create early new Setup{
    val dispatch = getService[DispatchPlugin]
    getServiceOption[CsrRamService] match {
      case Some(x) => x.retain()
      case None =>
    }
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
    val ram = getServiceOption[CsrRamService] match {
      case Some(x) => x
      case None => null
    }
    val decoder = getService[DecoderService]
    lock.await()

    val useRamRead = spec.exists(_.isInstanceOf[CsrRamSpec])
    val useRamWrite = spec.exists(_.isInstanceOf[CsrRamSpec])
    val useRam = useRamRead || useRamWrite

    val ramReadPort = useRamRead generate ram.ramReadPort()
    val ramWritePort = useRamWrite generate ram.ramWritePort()
    if(ram != null) ram.release()

    def filterToName(filter : Any) = filter match{
      case f : Int => f.toString
      case f : Nameable => f.getName()
    }
    val grouped = spec.groupByLinked(_.csrFilter)
    val sels = grouped.map(g => g._1 -> Stageable(Bool()).setName("CSR_" + filterToName(g._1)))

    val RAM_ADDRESS = Stageable(UInt(ramReadPort.addressWidth bits))
    val decodeLogic = new ExecuteArea(decodeAt) {
      import stage._
      sels.foreach{
        case (filter : Int, stageable) => stageable := MICRO_OP(Const.csrRange) === filter
        case (filter : CsrRamFilter, stageable) => stageable := filter.mapping.map(MICRO_OP(Const.csrRange) === _).orR
      }
      RAM_SEL := spec.filter(_.isInstanceOf[CsrRamSpec]).map(e => stage(sels(e.csrFilter))).orR

      val imm = IMM(MICRO_OP)
      val immZero = imm.z === 0
      val srcZero = CSR_IMM ? immZero otherwise MICRO_OP(Const.rs1Range) === 0
      CSR_WRITE := !(CSR_MASK && srcZero)
      CSR_READ := !(!CSR_MASK && !decoder.WRITE_RD)

      val ram = useRam generate new Area{
        RAM_ADDRESS.assignDontCare()
        switch(MICRO_OP(Const.csrRange)) {
          for (e <- spec.collect{ case x : CsrRamSpec => x}) e.csrFilter match {
            case filter: CsrRamFilter => for((csrId, offset) <- filter.mapping.zipWithIndex){
              is(csrId){
                RAM_ADDRESS := e.alloc.at + offset
              }
            }
            case csrId : Int => {
              is(csrId){
                RAM_ADDRESS := e.alloc.at
              }
            }
          }
        }
      }
    }

    val readLogic = new ExecuteArea(readAt) {
      import stage._

      CSR_READ_TRAP := setup.onReadTrap
      val onReadsDo = isValid && SEL && CSR_READ
      val onReadsFireDo = isFireing && SEL && CSR_READ  && !CSR_READ_TRAP

      haltIt(setup.onReadHalt)

      val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area{
        setPartialName(filterToName(csrFilter))

        val onReads  = elements.collect{ case e : CsrOnRead  => e }
        val onReadsData = elements.collect{ case e : CsrOnReadData => e }
        val onReadsAlways  = onReads.filter(!_.onlyOnFire)
        val onReadsFire  = onReads.filter(_.onlyOnFire)

        if(onReadsAlways.nonEmpty)  when(onReadsDo      && sels(csrFilter)){ onReadsAlways.foreach(_.body()) }
        if(onReadsFire.nonEmpty)   when(onReadsFireDo  && sels(csrFilter)){ onReadsFire.foreach(_.body()) }

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

      val ramRead = useRamRead generate new Area {
        val fired = RegInit(False) setWhen(ramReadPort.fire) clearWhen(isReady || isFlushed)
        ramReadPort.valid := onReadsDo && RAM_SEL && !fired
        ramReadPort.address := RAM_ADDRESS
        when(RAM_SEL) {
          CSR_VALUE := ramReadPort.data
        }
        haltIt(ramReadPort.valid && !ramReadPort.ready)
      }
    }

    val writeLogic = new ExecuteArea(writeAt) {
      import stage._
      val imm = IMM(MICRO_OP)

      ALU_INPUT := CSR_VALUE
      ALU_MASK := CSR_MASK ? imm.z.resized | eu(IntRegFile, RS1)
      ALU_MASKED := CSR_CLEAR ? (ALU_INPUT & ~ALU_MASK) otherwise (ALU_INPUT | ALU_MASK)
      ALU_RESULT := CSR_MASK ? stage(ALU_MASKED) otherwise ALU_MASK

      setup.onWriteBits := ALU_RESULT

      CSR_WRITE_TRAP := setup.onWriteTrap

      val onWritesDo = isValid && SEL && CSR_WRITE && !CSR_READ_TRAP
      val onWritesFireDo = isFireing && SEL && CSR_WRITE && !CSR_WRITE_TRAP && !CSR_READ_TRAP

      haltIt(setup.onWriteHalt)

      val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area{
        setPartialName(filterToName(csrFilter))

        val onWrites = elements.collect{ case e : CsrOnWrite => e }
        val onWritesAlways = onWrites.filter(!_.onlyOnFire)
        val onWritesFire = onWrites.filter(_.onlyOnFire)

        if(onWritesAlways.nonEmpty) when(onWritesDo     && sels(csrFilter)){ onWritesAlways.foreach(_.body()) }
        if(onWritesFire.nonEmpty)  when(onWritesFireDo && sels(csrFilter)){ onWritesFire.foreach(_.body()) }
      }

      val ramWrite = useRamRead generate new Area {
        val fired = RegInit(False) setWhen(ramWritePort.fire) clearWhen(isReady || isFlushed)
        ramWritePort.valid := onWritesDo && RAM_SEL && !fired && !CSR_WRITE_TRAP
        ramWritePort.address := RAM_ADDRESS
        ramWritePort.data := setup.onWriteBits
        haltIt(ramWritePort.valid && !ramWritePort.ready)
      }
    }

    val writebackLogic = new ExecuteArea(writebackAt) {
      import stage._
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
      csrWrite.valid := writeLogic.stage.isFireing
      csrWrite.robId := writeLogic.stage(ROB.ID)
      csrWrite.address := U(writeLogic.stage(MICRO_OP)(Const.csrRange))
      csrWrite.data := setup.onWriteBits
    }
  }
}
