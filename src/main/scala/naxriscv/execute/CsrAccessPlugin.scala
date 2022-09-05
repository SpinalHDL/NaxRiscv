package naxriscv.execute

import naxriscv.{DecodeList, Frontend, ROB, riscv}
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
import spinal.lib.fsm.{State, StateMachine}
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

class CsrAccessPlugin(val euId: String)(var writebackAt: Int) extends ExecutionUnitElementSimple(euId, staticLatency = false) with CsrService{
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
  override def onWriteFlushPipeline() = setup.onWriteFlushPipeline := True

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
    val onWriteFlushPipeline = False
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


    val RAM_ADDRESS = Stageable(UInt(ramReadPort.addressWidth bits))


    val fsm = new StateMachine{
      val IDLE, READ, WRITE, DONE = new State
      setEntry(IDLE)

      val regs = new Area{
        val sels = grouped.map(e => e._1 -> Reg(Bool()).setName("REG_CSR_" + filterToName(e._1)))
        val ramAddress = useRam generate Reg(RAM_ADDRESS)
        val ramSel = useRam generate Reg(Bool())
        val microOp = Reg(MICRO_OP)
        val doImm, doMask, doClear = Reg(Bool())
        val rs1 = Reg(Bits(XLEN bits))

        val implemented, trap, flushPipeline = Reg(Bool())
        val read, write = Reg(Bool())

        val aluInput, csrValue = Reg(CSR_VALUE)
      }


      val startLogic = new ExecuteArea(0) {
        import stage._

        val imm = IMM(MICRO_OP)
        val immZero = imm.z === 0
        val srcZero = CSR_IMM ? immZero otherwise MICRO_OP(Const.rs1Range) === 0
        val csrWrite = !(CSR_MASK && srcZero)
        val csrRead  = !(!CSR_MASK && !decoder.WRITE_RD)
        val sels = grouped.map(e => e._1 -> Bool().setName("COMB_CSR_" + filterToName(e._1)))
        for((filter, sel) <- sels) sel := (filter match {
          case filter : Int           => MICRO_OP(Const.csrRange) === filter
          case filter : CsrListFilter => filter.mapping.map(MICRO_OP(Const.csrRange) === _).orR
        })
        val implemented = sels.values.orR

        setup.onDecodeWrite := csrWrite
        setup.onDecodeRead  := csrRead
        val trap = !implemented || setup.onDecodeTrap

        val write = SEL && !trap && csrWrite
        val read  = SEL && !trap && csrRead

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

        IDLE whenIsActive{
          regs.microOp := MICRO_OP
          regs.doImm   := CSR_IMM
          regs.doMask  := CSR_MASK
          regs.doClear := CSR_CLEAR
          regs.rs1     := eu(IntRegFile, RS1)
          regs.implemented := implemented
          regs.trap := trap
          regs.flushPipeline := setup.onDecodeFlushPipeline
          regs.write := write
          regs.read  := read

          (regs.sels.values, sels.values).zipped.foreach(_ := _)

          if(useRam){
            ramReadPort.get //Ensure the ram port is generated
            regs.ramSel := False
            switch(MICRO_OP(Const.csrRange)) {
              for (e <- spec.collect { case x: CsrRamSpec => x }) e.csrFilter match {
                case filter: CsrListFilter => for ((csrId, offset) <- filter.mapping.zipWithIndex) {
                  is(csrId) {
                    regs.ramSel := True
                    regs.ramAddress := e.alloc.at + offset
                  }
                }
                case csrId: Int => {
                  is(csrId) {
                    regs.ramSel := True
                    regs.ramAddress := e.alloc.at
                  }
                }
              }
            }
          }


          when(onDecodeDo){
            goto(READ)
          }
        }
      }

      val readLogic = new Area{
        setup.onReadAddress := U(regs.microOp)(Const.csrRange)
        val onReadsDo = False
        val onReadsFireDo = False

        setup.onReadMovingOff := !setup.onReadHalt || eu.getExecute(0).isFlushed

        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area{
          setPartialName(filterToName(csrFilter))

          val onReads  = elements.collect{ case e : CsrOnRead  => e }
          val onReadsData = elements.collect{ case e : CsrOnReadData => e }
          val onReadsAlways  = onReads.filter(!_.onlyOnFire)
          val onReadsFire  = onReads.filter(_.onlyOnFire)

          if(onReadsAlways.nonEmpty)  when(onReadsDo      && regs.sels(csrFilter)){ onReadsAlways.foreach(_.body()) }
          if(onReadsFire.nonEmpty)   when(onReadsFireDo  && regs.sels(csrFilter)){ onReadsFire.foreach(_.body()) }

          val read = onReadsData.nonEmpty generate new Area {
            val bitCount = onReadsData.map(e => widthOf(e.value)).sum
            assert(bitCount <= XLEN.get)

            val value = if(bitCount != XLEN.get) B(0, XLEN bits) else Bits(XLEN bits)
            onReadsData.foreach (e => value.assignFromBits(e.value.asBits, e.bitOffset, widthOf(e.value) bits) )
            val masked = value.andMask(regs.sels(csrFilter))
          }
        }

        val csrValue = CSR_VALUE()
        if(groupedLogic.exists(_.onReadsData.nonEmpty)){
          csrValue := groupedLogic.filter(_.onReadsData.nonEmpty).map(_.read.masked).toList.reduceBalancedTree(_ | _)
        } else {
          csrValue := 0
        }

        val ramRead = useRamRead generate new Area {
          ramReadPort.valid := onReadsDo && regs.ramSel
          ramReadPort.address := regs.ramAddress
          when(regs.ramSel) {
            csrValue := ramReadPort.data
          }
          setup.onReadHalt setWhen(ramReadPort.valid && !ramReadPort.ready)
        }

        setup.onReadToWriteBits := csrValue
        for ((csrFilter, elements) <- grouped) {
          val onReadToWrite = elements.collect { case e: CsrOnReadToWrite => e }
          if(onReadToWrite.nonEmpty)  when(onReadsDo  && regs.sels(csrFilter)){ onReadToWrite.foreach(_.body()) }
        }

        READ.whenIsActive{
          onReadsDo := regs.read
          regs.aluInput := setup.onReadToWriteBits
          regs.csrValue := csrValue
          when(!setup.onReadHalt){
            onReadsFireDo :=  regs.read
            goto(WRITE)
          }
        }
      }

      val writeLogic = new Area{
        val imm = IMM(regs.microOp)
        setup.onWriteMovingOff := !setup.onWriteHalt || eu.getExecute(0).isFlushed

        val alu = new Area {
          val mask = regs.doImm ? imm.z.resized | regs.rs1
          val masked = regs.doClear ? (regs.aluInput & ~mask) otherwise (regs.aluInput | mask)
          val result = regs.doMask ? masked otherwise mask
        }

        setup.onWriteBits    := alu.result
        setup.onWriteAddress := U(regs.microOp)(Const.csrRange)

        val onWritesDo     = False
        val onWritesFireDo = False

        WRITE.whenIsActive{
          regs.flushPipeline setWhen(setup.onWriteFlushPipeline)
          onWritesDo :=  regs.write
          when(!setup.onWriteHalt){
            onWritesFireDo :=  regs.write
            goto(DONE)
          }
        }


        val groupedLogic = for ((csrFilter, elements) <- grouped) yield new Area{
          setPartialName(filterToName(csrFilter))

          val cancels = elements.collect{ case e : CsrWriteCancel => e }.toList
          val onWrites = elements.collect{ case e : CsrOnWrite => e }
          val onWritesAlways = onWrites.filter(!_.onlyOnFire)
          val onWritesFire = onWrites.filter(_.onlyOnFire)

          def doIt(){
            if(onWritesAlways.nonEmpty) when(onWritesDo     && regs.sels(csrFilter)){ onWritesAlways.foreach(_.body()) }
            if(onWritesFire.nonEmpty)  when(onWritesFireDo && regs.sels(csrFilter)){ onWritesFire.foreach(_.body()) }
          }

          cancels match {
            case Nil => doIt()
            case l => when(cancels.map(_.cond).orR === False){ doIt() }
          }
        }

        val ramWrite = useRamRead generate new Area {
          val fired = RegInit(False) setWhen(ramWritePort.fire) clearWhen(setup.onWriteMovingOff)
          ramWritePort.valid := onWritesDo && regs.ramSel && !fired
          ramWritePort.address := regs.ramAddress
          ramWritePort.data := setup.onWriteBits
          setup.onWriteHalt setWhen(ramWritePort.valid && !ramWritePort.ready)
        }
      }

      val isDone = False
      val isCompletionReady = False
      DONE.whenIsActive{
        isDone := True
        when(isCompletionReady){
          goto(IDLE)
        }
      }

      always{
        when(eu.getExecute(0).isFlushed){
          goto(IDLE)
        }
      }

      build()
    }

    val writebackLogic = new ExecuteArea(writebackAt) {
      import stage._
      wb.payload := fsm.regs.csrValue

      setup.trap.valid      := isValid && SEL && fsm.isDone && (fsm.regs.trap || fsm.regs.flushPipeline)
      setup.trap.robId      := ROB.ID
      setup.trap.cause      := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      setup.trap.tval       := fsm.regs.microOp.resized
      setup.trap.skipCommit := fsm.regs.trap
      setup.trap.reason     := ScheduleReason.TRAP

      haltIt(isValid && SEL && !fsm.isDone)
      fsm.isCompletionReady setWhen(isReady)

      when(!fsm.regs.trap){
        setup.trap.cause := EnvCallPlugin.CAUSE_FLUSH
      }
    }

    val whitebox = new AreaRoot {
      import writebackLogic.stage._
      val csrAccess = Verilator.public(Flow(new Bundle {
        val robId = ROB.ID()
        val address = UInt(12 bits)
        val write = Bits(XLEN bits)
        val read = Bits(XLEN bits)
        val writeDone = Bool()
        val readDone = Bool()
      }))
      csrAccess.valid := isFireing && SEL && !fsm.regs.trap
      csrAccess.robId := ROB.ID
      csrAccess.address := U(MICRO_OP)(Const.csrRange)
      csrAccess.write := setup.onWriteBits
      csrAccess.read := fsm.regs.csrValue
      csrAccess.writeDone := fsm.regs.write
      csrAccess.readDone := fsm.regs.read
    }
  }
}

