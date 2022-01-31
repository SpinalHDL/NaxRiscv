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
import spinal.lib.logic.Symplify

object CsrAccessPlugin extends AreaObject{
  val CSR_IMM  = Stageable(Bool())
  val CSR_MASK = Stageable(Bool())
  val CSR_CLEAR = Stageable(Bool())
  val CSR_WRITE = Stageable(Bool())
  val CSR_READ = Stageable(Bool())
  val CSR_VALUE = Stageable(Bits(XLEN bits))
  val CSR_VALUE_TO_ALU = Stageable(Bits(XLEN bits))
  val ALU_INPUT = Stageable(Bits(XLEN bits))
  val ALU_MASK = Stageable(Bits(XLEN bits))
  val ALU_MASKED = Stageable(Bits(XLEN bits))
  val ALU_RESULT = Stageable(Bits(XLEN bits))
  val RAM_SEL    = Stageable(Bool())
  val CSR_IMPLEMENTED = Stageable(Bool())
  val CSR_TRAP = Stageable(Bool())
  val CSR_FLUSH_PIPELINE = Stageable(Bool())
}

class CsrAccessPlugin(euId: String)(decodeAt: Int,
                                    readAt: Int,
                                    writeAt: Int,
                                    writebackAt: Int,
                                    staticLatency: Boolean) extends ExecutionUnitElementSimple(euId, staticLatency) with CsrService{
  override def euWritebackAt = writebackAt

  override def onReadHalt()   = setup.onReadHalt   := True
  override def onWriteHalt()  = setup.onWriteHalt  := True
  override def onWriteBits = setup.onWriteBits
  override def onWriteAddress = setup.onWriteAddress
  override def onReadAddress  = setup.onReadAddress
  override def onReadMovingOff = setup.onReadMovingOff
  override def onWriteMovingOff = setup.onWriteMovingOff
  override def getCsrRam() = getService[CsrRamService]



  import CsrAccessPlugin._

  override def onDecodeTrap() = setup.onDecodeTrap := True
  override def onDecodeUntrap() = setup.onDecodeTrap := False
  override def onDecodeFlushPipeline() = setup.onDecodeFlushPipeline := True
  override def onDecodeRead = setup.onDecodeRead
  override def onDecodeWrite = setup.onDecodeWrite
  override def onDecodeAddress  = setup.onDecodeAddress
  override def onReadToWriteBits = setup.onReadToWriteBits

  val setup = create early new Setup{
    val dispatch = getService[DispatchPlugin]
    getServiceOption[CsrRamService] match {
      case Some(x) => x.portLock.retain()
      case None =>
    }
    assert(getServicesOf[CsrService].size == 1)

    val onDecodeTrap = False
    val onDecodeFlushPipeline = False
    val onDecodeRead  = Bool()
    val onDecodeWrite = Bool()
    val onDecodeAddress = UInt(12 bits)

    val onReadHalt  = False
    val onWriteHalt = False

    val onReadToWriteBits = Bits(XLEN bits)
    val onWriteBits = Bits(XLEN bits)
    val onWriteAddress = UInt(12 bits)
    val onReadAddress  = UInt(12 bits)
    val onReadMovingOff = Bool()
    val onWriteMovingOff = Bool()

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

    val ramReadPort = useRamRead generate ram.ramReadPort(CsrRamService.priority.CSR)
    val ramWritePort = useRamWrite generate ram.ramWritePort(CsrRamService.priority.CSR)
    if(ram != null) ram.portLock.release()

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
        case (filter : CsrListFilter, stageable) => stageable := filter.mapping.map(MICRO_OP(Const.csrRange) === _).orR
      }
      RAM_SEL := spec.filter(_.isInstanceOf[CsrRamSpec]).map(e => stage(sels(e.csrFilter))).orR

      val imm = IMM(MICRO_OP)
      val immZero = imm.z === 0
      val srcZero = CSR_IMM ? immZero otherwise MICRO_OP(Const.rs1Range) === 0
      CSR_WRITE := !(CSR_MASK && srcZero)
      CSR_READ := !(!CSR_MASK && !decoder.WRITE_RD)
      CSR_IMPLEMENTED := sels.values.map(stage(_)).toSeq.orR

      setup.onDecodeRead := CSR_READ
      setup.onDecodeWrite := CSR_WRITE
      CSR_TRAP := !CSR_IMPLEMENTED || setup.onDecodeTrap
      CSR_FLUSH_PIPELINE := setup.onDecodeFlushPipeline

      setup.onDecodeAddress := U(MICRO_OP)(Const.csrRange)

      val onDecodeDo = isValid && SEL
      val priorities = spec.collect{ case e : CsrOnDecode  => e.priority }.distinct.sorted
      for(priority <- priorities) {
        for ((csrFilter, elements) <- grouped) {
          val onDecodes = elements.collect { case e: CsrOnDecode if e.priority == priority => e }
          if (onDecodes.nonEmpty) when(onDecodeDo && sels(csrFilter)) {
            onDecodes.foreach(_.body())
          }
        }
      }

      val ram = useRam generate new Area{
        ramReadPort.get //Ensure the ram port is generated
        RAM_ADDRESS.assignDontCare()
        switch(MICRO_OP(Const.csrRange)) {
          for (e <- spec.collect{ case x : CsrRamSpec => x}) e.csrFilter match {
            case filter: CsrListFilter => for((csrId, offset) <- filter.mapping.zipWithIndex){
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

      setup.onReadAddress := U(MICRO_OP)(Const.csrRange)
      val onReadsDo = isValid && SEL && !CSR_TRAP && CSR_READ
      val onReadsFireDo = isFireing && SEL && !CSR_TRAP && CSR_READ
      setup.onReadMovingOff := stage.isReady || stage.isFlushed

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
        //val fired = RegInit(False) setWhen(ramReadPort.fire) clearWhen(isReady || isFlushed)
        ramReadPort.valid := onReadsDo && RAM_SEL// && !fired
        ramReadPort.address := RAM_ADDRESS
        when(RAM_SEL) {
          CSR_VALUE := ramReadPort.data
        }
        haltIt(ramReadPort.valid && !ramReadPort.ready)
      }

      setup.onReadToWriteBits := CSR_VALUE
      for ((csrFilter, elements) <- grouped) {
        val onReadToWrite = elements.collect { case e: CsrOnReadToWrite => e }
        if(onReadToWrite.nonEmpty)  when(onReadsDo  && sels(csrFilter)){ onReadToWrite.foreach(_.body()) }
      }
      CSR_VALUE_TO_ALU := setup.onReadToWriteBits
    }

    val writeLogic = new ExecuteArea(writeAt) {
      import stage._
      val imm = IMM(MICRO_OP)
      setup.onWriteMovingOff := stage.isReady || stage.isFlushed

      ALU_INPUT := CSR_VALUE_TO_ALU
      ALU_MASK := CSR_IMM ? imm.z.resized | eu(IntRegFile, RS1)
      ALU_MASKED := CSR_CLEAR ? (ALU_INPUT & ~ALU_MASK) otherwise (ALU_INPUT | ALU_MASK)
      ALU_RESULT := CSR_MASK ? stage(ALU_MASKED) otherwise ALU_MASK

      setup.onWriteBits := ALU_RESULT
      setup.onWriteAddress := U(MICRO_OP)(Const.csrRange)

      val onWritesDo = isValid && SEL && !CSR_TRAP && CSR_WRITE
      val onWritesFireDo = isFireing && SEL && !CSR_TRAP && CSR_WRITE

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
        ramWritePort.valid := onWritesDo && RAM_SEL && !fired
        ramWritePort.address := RAM_ADDRESS
        ramWritePort.data := setup.onWriteBits
        haltIt(ramWritePort.valid && !ramWritePort.ready)
      }
    }

    val writebackLogic = new ExecuteArea(writebackAt) {
      import stage._
      wb.payload := CSR_VALUE

      setup.trap.valid      := isValid && SEL && (CSR_TRAP || CSR_FLUSH_PIPELINE)
      setup.trap.robId      := ROB.ID
      setup.trap.cause      := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      setup.trap.tval       := MICRO_OP
      setup.trap.skipCommit := CSR_TRAP
      setup.trap.reason     := ScheduleReason.TRAP

      when(!CSR_TRAP){
        setup.trap.cause := EnvCallPlugin.CAUSE_FLUSH
      }
    }

    val whitebox = new AreaRoot {
      import writeLogic.stage._

      val csrAccess = Verilator.public(Flow(new Bundle {
        val robId = ROB.ID()
        val address = UInt(12 bits)
        val write = Bits(XLEN bits)
        val read = Bits(XLEN bits)
        val writeDone = Bool()
        val readDone = Bool()
      }))
      csrAccess.valid := isFireing && SEL && !CSR_TRAP
      csrAccess.robId := ROB.ID
      csrAccess.address := U(MICRO_OP)(Const.csrRange)
      csrAccess.write := setup.onWriteBits
      csrAccess.read := CSR_VALUE
      csrAccess.writeDone := CSR_WRITE
      csrAccess.readDone := CSR_READ
    }
  }
}

