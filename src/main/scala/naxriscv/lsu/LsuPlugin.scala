package naxriscv.lsu

import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK}
import naxriscv.backend.RobPlugin
import naxriscv.{Frontend, Global, ROB}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces._
import naxriscv.riscv.CSR
import naxriscv.utilities.{AddressToMask, DocPlugin, Plugin}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stageable, StageableOffset}

import scala.collection.mutable.ArrayBuffer

object LsuUtils{
  def sizeWidth(wordWidth : Int) = log2Up(log2Up(wordWidth/8)+1)
}

case class LsuLoadPort(lqSize : Int, wordWidth : Int, physicalRdWidth : Int, pcWidth : Int) extends Bundle {
  val robId = ROB.ID()
  val lqId = UInt(log2Up(lqSize) bits)
  val address = UInt(Global.XLEN bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
  val unsigned = Bool()
  val physicalRd = UInt(physicalRdWidth bits)
  val writeRd = Bool()
  val pc = UInt(pcWidth bits)
}

case class LsuStorePort(sqSize : Int, wordWidth : Int) extends Bundle {
  val robId = ROB.ID()
  val sqId = UInt(log2Up(sqSize) bits)
  val address = UInt(Global.XLEN bits)
  val data = Bits(wordWidth bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
}

case class LsuPeripheralBusParameter(addressWidth : Int,
                                     dataWidth : Int)

case class LsuPeripheralBusCmd(p : LsuPeripheralBusParameter) extends Bundle{
  val write = Bool()
  val address = UInt(p.addressWidth bits)
  val data = Bits(p.dataWidth bits)
  val mask = Bits(p.dataWidth / 8 bit)
  val size = UInt(log2Up(log2Up(p.dataWidth/8)+1) bits)
}

case class LsuPeripheralBusRsp(p : LsuPeripheralBusParameter) extends Bundle{
  val error = Bool()
  val data = Bits(p.dataWidth bits)
}

object LsuPeripheralBus{
  def apply(addressWidth : Int, dataWidth : Int) : LsuPeripheralBus = LsuPeripheralBus(LsuPeripheralBusParameter(addressWidth,dataWidth))
}

case class LsuPeripheralBus(p : LsuPeripheralBusParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(LsuPeripheralBusCmd(p))
  val rsp = Flow(LsuPeripheralBusRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}


class LsuPlugin(lqSize: Int,
                sqSize : Int,
                loadTranslationParameter : Any,
                storeTranslationParameter : Any,
                loadFeedAt : Int = 1, //Stage at which the d$ cmd is sent
                loadCheckSqAt : Int = 1) extends Plugin with LockedImpl with WakeRobService with WakeRegFileService {

  val wordWidth = Global.XLEN.get
  val wordBytes = wordWidth/8
  val wordSizeWidth = LsuUtils.sizeWidth(wordWidth)
  val pageOffsetRange = 11 downto log2Up(wordBytes)
  val pageNumberRange = Global.XLEN.get-1 downto 12
  val pageOffsetWidth = pageOffsetRange.size
  val pageNumberWidth = pageNumberRange.size


  val peripheralBus = create late master(LsuPeripheralBus(postWidth, wordWidth))

  def postWidth = getService[AddressTranslationService].postWidth
  def virtualAddressWidth = getService[AddressTranslationService].preWidth

  case class StorePortSpec(port : Flow[LsuStorePort])
  val storePorts = ArrayBuffer[StorePortSpec]()
  def newStorePort(): Flow[LsuStorePort] = {
    storePorts.addRet(StorePortSpec(Flow(LsuStorePort(sqSize, wordWidth)))).port
  }

  case class LoadPortSpec(port : Flow[LsuLoadPort])
  val loadPorts = ArrayBuffer[LoadPortSpec]()
  def newLoadPort(): Flow[LsuLoadPort] = {
    val physicalRdWidth = getService[DecoderService].PHYS_RD
    loadPorts.addRet(LoadPortSpec(Flow(LsuLoadPort(lqSize, wordWidth, widthOf(physicalRdWidth), virtualAddressWidth)))).port
  }


  override def wakeRobs = List(logic.get.load.pipeline.cacheRsp.wakeRob)
  override def wakeRegFile = List(logic.get.load.pipeline.cacheRsp.wakeRf)


  val keys = new AreaRoot{
    val SQ_ALLOC = Stageable(Bool())
    val LQ_ALLOC = Stageable(Bool())
    val LSU_ID = Stageable(UInt(log2Up(lqSize max sqSize) bits))
    val SIZE = Stageable(UInt(wordSizeWidth bits))
    val UNSIGNED = Stageable(Bool())
    val WRITE_RD = Stageable(Bool())
    val LQ_SEL = Stageable(UInt(log2Up(lqSize) bits))
    val LQ_SEL_OH = Stageable(Bits(lqSize bits))
    val SQ_SEL = Stageable(UInt(log2Up(sqSize) bits))
    val SQ_SEL_OH = Stageable(Bits(sqSize bits))
    val ADDRESS_PRE_TRANSLATION = Stageable(UInt(virtualAddressWidth bits))
    val DATA_MASK = Stageable(Bits(wordBytes bits))
  }
  import keys._

  val setup = create early new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val cache = getService[DataCachePlugin]
    val regfile = getService[RegfileService]
    val commit = getService[CommitService]
    val translation = getService[AddressTranslationService]
    val doc = getService[DocPlugin]

    rob.retain()
    decoder.retain()
    frontend.retain()
    translation.retain()

    val rfWrite = regfile.newWrite(withReady = false, latency = 1)
    val cacheLoad = cache.newLoadPort()
    val cacheStore = cache.newStorePort()
    val loadCompletion = rob.newRobCompletion()
    val storeCompletion = rob.newRobCompletion()
    val loadTrap = commit.newSchedulePort(canTrap = true, canJump = false)
    val storeTrap = commit.newSchedulePort(canTrap = true, canJump = true)

    decoder.addResourceDecoding(naxriscv.interfaces.LQ, LQ_ALLOC)
    decoder.addResourceDecoding(naxriscv.interfaces.SQ, SQ_ALLOC)
    doc.property("LSU_PERIPHERAL_WIDTH", wordWidth)
  }

  val logic = create late new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val cache = getService[DataCachePlugin]
    val commit = getService[CommitService]
    val translationService = getService[AddressTranslationService]
    val PC = getService[AddressTranslationService].PC
    lock.await()

    val keysLocal = new AreaRoot {
      val LQ_ID = Stageable(UInt(log2Up(lqSize) bits))
      val SQ_ID = Stageable(UInt(log2Up(sqSize) bits))

      val LQ_ID_CARRY = Stageable(Bool())
      val SQ_ID_CARRY = Stageable(Bool())

      val YOUNGER_LOAD_PC         = Stageable(PC)
      val YOUNGER_LOAD_ROB        = Stageable(ROB.ID)
      val YOUNGER_LOAD_RESCHEDULE = Stageable(Bool())

      val OLDER_STORE_RESCHEDULE  = Stageable(Bool())
      val OLDER_STORE_ID = Stageable(SQ_ID)
      val OLDER_STORE_COMPLETED = Stageable(Bool()) //Used to avoid LQ waiting on SQ which just fired

      val LQCHECK_START_ID = Stageable(UInt(log2Up(lqSize) + 1 bits))
      val LQCHECK_HITS = Stageable(Bits(lqSize bits))
      val LQCHECK_NO_YOUNGER = Stageable(Bool())

      val SQCHECK_END_ID = Stageable(UInt(log2Up(sqSize) + 1 bits))
      val SQCHECK_HITS = Stageable(Bits(sqSize bits))
      val SQCHECK_NO_OLDER = Stageable(Bool())
    }
    import keysLocal._

    val cpuWordToRfWordRange = log2Up(wordBytes)-1 downto log2Up(wordBytes) //useless for now
    val memToCpuRange = log2Up(cache.memDataWidth/8)-1 downto log2Up(wordBytes)

    val rescheduling = commit.reschedulingPort

    val allocStage = frontend.pipeline.dispatch
    for(slotId <- 0 until Frontend.DISPATCH_COUNT){
      allocStage(LSU_ID, slotId).assignDontCare()
    }

    val lq = new Area{
      val regs = for(i <- 0 until lqSize) yield RegType(i)
      case class RegType(id : Int) extends Area{
        val valid = RegInit(False)
        val waitOn = new Area{
          val address     = Reg(Bool())
          val cacheRsp    = Reg(Bool())
          val cacheRefill = Reg(Bits(cache.refillCount bits))
          val cacheRefillAny = Reg(Bool())
          val sq     = Reg(Bool())
          val sqId   = Reg(SQ_ID)
          val commit = Reg(Bool())

          val cacheRefillSet = cacheRefill.getZero
          val cacheRefillAnySet = False

          cacheRefill := (cacheRefill | cacheRefillSet) & ~cache.refillCompletions
          cacheRefillAny := (cacheRefillAny | cacheRefillAnySet) & !cache.refillCompletions.orR
        }
        val address = new Area {
          val pageOffset = Reg(UInt(pageOffsetWidth bits))
          val size = Reg(SIZE())
          val unsigned = Reg(UNSIGNED())
          val mask = Reg(DATA_MASK)
        }

        val ready = valid && !waitOn.address && !waitOn.cacheRsp && waitOn.cacheRefill === 0 && !waitOn.cacheRefillAny && !waitOn.commit && !waitOn.sq
      }
      val mem = new Area{
        val addressPre = Mem.fill(lqSize)(UInt(virtualAddressWidth bits))
        val addressPost = Mem.fill(lqSize)(UInt(virtualAddressWidth bits))
        val physRd = Mem.fill(lqSize)(decoder.PHYS_RD)
        val robId = Mem.fill(lqSize)(ROB.ID)
        val pc = Mem.fill(lqSize)(PC)
        val sqAlloc = Mem.fill(lqSize)(UInt(log2Up(sqSize)+1 bits))
        val io = Mem.fill(lqSize)(Bool())
        val writeRd = Mem.fill(lqSize)(Bool())
      }

      val ptr = new Area{
        val alloc, free = Reg(UInt(log2Up(lqSize) + 1 bits)) init (0)
        val allocReal = U(alloc.dropHigh(1))
        val freeReal = U(free.dropHigh(1))
        def isFull(ptr : UInt) = (ptr ^ free) === lqSize
        val priority = Reg(Bits(lqSize-1 bits)) init(0) //TODO check it work properly
      }
    }

    val sq = new Area{
      val regs = for(i <- 0 until sqSize) yield RegType(i)
      case class RegType(id : Int) extends Area{
        val valid = RegInit(False)
        val commited = RegInit(False) //Warning, commited is meaning full even when valid == False !
        val commitedNext = CombInit(commited)
        commited := commitedNext

        val address = new Area{
          val pageOffset  = Reg(UInt(pageOffsetWidth bits))
          val size = Reg(SIZE())
          val mask = Reg(DATA_MASK)
        }

        val waitOn = new Area {
          val address = Reg(Bool())
          val translationRsp  = Reg(Bool())
          val translationWake = Reg(Bits(translationService.wakerCount bits))
          val translationWakeAny = Reg(Bool)
          val writeback  = Reg(Bool())

          val translationWakeSet = translationWake.getZero
          val translationWakeAnySet = False

          translationWake := (translationWake | translationWakeSet) & ~translationService.wakes
          translationWakeAny := (translationWakeAny | translationWakeAnySet) & !translationService.wakes.orR
        }

        val allLqIsYounger = Reg(Bool())

        val ready = valid && !waitOn.address && !waitOn.translationRsp && !waitOn.writeback && waitOn.translationWake === 0 && !waitOn.translationWakeAny
      }

      val mem = new Area{
        val addressPre = Mem.fill(sqSize)(UInt(virtualAddressWidth bits))
        val addressPost = Mem.fill(sqSize)(UInt(virtualAddressWidth bits))
        val word = Mem.fill(sqSize)(Bits(wordWidth bits))
        val robId = Mem.fill(sqSize)(ROB.ID)
        val lqAlloc = Mem.fill(sqSize)(UInt(log2Up(lqSize) + 1 bits))
        val io = Mem.fill(sqSize)(Bool())
      }

      val ptr = new Area{
        val alloc, commit, writeBack, free = Reg(UInt(log2Up(sqSize) + 1 bits)) init (0)
        val allocReal = U(alloc.dropHigh(1))
        val freeReal = U(free.dropHigh(1))
        val writeBackReal = U(writeBack.dropHigh(1))
        val commitReal = U(commit.dropHigh(1))
        val commitNext = cloneOf(commit)
        commit := commitNext
        val priority = Reg(Bits(sqSize-1 bits)) init(0) //TODO check it work properly
        def isFull(ptr : UInt) = (ptr ^ free) === sqSize

        val onFree = Flow(UInt(log2Up(sqSize) bits))
        val onFreeLast = onFree.stage()
      }
    }

    val load = new Area{
      import lq._
      for(spec <- loadPorts){
        import spec._
        mem.addressPre.write(
          enable = port.valid,
          address = port.lqId,
          data = port.address
        )
        mem.physRd.write(
          enable = port.valid,
          address = port.lqId,
          data = port.physicalRd
        )
        mem.robId.write(
          enable = port.valid,
          address = port.lqId,
          data = port.robId
        )
        mem.pc.write(
          enable = port.valid,
          address = port.lqId,
          data = port.pc
        )
        mem.writeRd.write(
          enable = port.valid,
          address = port.lqId,
          data = port.writeRd
        )

        when(port.valid) {
          for (entry <- regs) when(port.lqId === entry.id) {
            entry.waitOn.address := False
            entry.address.pageOffset := port.address(pageOffsetRange)
            entry.address.size := port.size
            entry.address.unsigned := port.unsigned
            entry.address.mask := AddressToMask(port.address, port.size, wordBytes)
          }
        }
      }

      for(reg <- regs) when(sq.ptr.onFree.valid && sq.ptr.onFree.payload === reg.waitOn.sqId){
        reg.waitOn.sq := False
      }

      val allocate = new Area{
        import allocStage._

        val full = False
        haltIt(isValid && full)

        var alloc = CombInit(ptr.alloc)
        for(slotId <- 0 until Frontend.DISPATCH_COUNT){
          implicit val _ = StageableOffset(slotId)
          LQ_ID := alloc.resized
          LQ_ID_CARRY := alloc.msb
          when(DISPATCH_MASK && LQ_ALLOC){
            LSU_ID := alloc.resized
            when(isFireing) {
              mem.sqAlloc.write(
                address = LQ_ID,
                data = U(SQ_ID_CARRY ## allocStage(SQ_ID, slotId))
              )
            }
            when(ptr.isFull(alloc)){
              full := True
            }
            alloc \= alloc + 1
          }
        }




        when(isFireing){
          ptr.alloc := alloc
          for(reg <- regs){
            val hits = for(slotId <- 0 until Frontend.DISPATCH_COUNT) yield{
              (DISPATCH_MASK, slotId) && (LQ_ALLOC, slotId) && (LSU_ID, slotId).resize(log2Up(lqSize) bits) === reg.id
            }
            when(hits.orR){
              reg.valid := True
              reg.waitOn.address := True
              reg.waitOn.cacheRsp := False
              reg.waitOn.cacheRefill := 0
              reg.waitOn.cacheRefillAny := False
              reg.waitOn.commit := False
              reg.waitOn.sq := False
            }
          }
        }
      }

      val pipeline = new Pipeline{
        val stages = Array.fill(loadFeedAt + cache.loadRspLatency + 1)(newStage())
        connect(stages)(List(M2S()))

        stages.last.flushIt(rescheduling.valid, root = false)

        val translationPort = translationService.newTranslationPort(
          stages = this.stages,
          preAddress = ADDRESS_PRE_TRANSLATION,
          p = storeTranslationParameter
        )
        val tpk = translationPort.keys

        val feed = new Area{
          val stage = stages(0)
          import stage._

          val hits = B(regs.map(reg => reg.ready))
          val hit = hits.orR

          val selOh = OHMasking.roundRobinMasked(hits, ptr.priority)
          val sel = OHToUInt(selOh)

          for(reg <- regs) when(selOh(reg.id)){
            reg.waitOn.cacheRsp := True
          }

          isValid := hit
          LQ_SEL := sel
          LQ_SEL_OH := selOh
          decoder.PHYS_RD := mem.physRd.readAsync(sel)
          ROB.ID := mem.robId.readAsync(sel)
          WRITE_RD := mem.writeRd.readAsync(sel)
          ADDRESS_PRE_TRANSLATION := mem.addressPre.readAsync(LQ_SEL)
          SIZE     := regs.map(_.address.size).read(sel)
          UNSIGNED := regs.map(_.address.unsigned).read(sel)
          DATA_MASK := AddressToMask(ADDRESS_PRE_TRANSLATION, SIZE, wordBytes)
          SQCHECK_END_ID :=  mem.sqAlloc.readAsync(sel)
          PC := mem.pc(sel)
        }

        val feedCache = new Area{
          val stage = stages(loadFeedAt)
          import stage._

          val cmd = setup.cacheLoad.cmd //If you move that in another stage, be carefull to update loadFeedAt usages (sq d$ writeback rsp delay)
          cmd.valid := stage.isFireing
          cmd.virtual := ADDRESS_PRE_TRANSLATION
          cmd.size := SIZE
        }

        val feedTranslation = new Area{
          val stage = stages(loadFeedAt + setup.cacheLoad.translatedAt)
          import stage._

          setup.cacheLoad.translated.physical := tpk.TRANSLATED
          setup.cacheLoad.translated.abord := tpk.IO || tpk.PAGE_FAULT || !tpk.ALLOW_READ || tpk.REDO
        }

        val cancels = for(stageId <- 0 to cache.loadRspLatency){
          setup.cacheLoad.cancels(stageId) := stages(loadFeedAt + stageId).isValid && rescheduling.valid
        }

        val checkSqMask = new Area{
          val stage = stages(loadCheckSqAt) //WARNING, SQ delay between writeback and entry.valid := False should not be smaller than the delay of reading the cache and checkSq !!
          import stage._

          val startId = CombInit(sq.ptr.free)
          val startMask = U(UIntToOh(U(startId.dropHigh(1))))-1
          val endMask   = U(UIntToOh(U(SQCHECK_END_ID.dropHigh(1))))-1
          val loopback = endMask <= startMask
          val youngerMask = loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask)
          val olderMaskEmpty = startId === SQCHECK_END_ID

          val hits = Bits(sqSize bits)
          val entries = for(sqReg <- sq.regs) yield new Area {
            val pageHit = sqReg.address.pageOffset === ADDRESS_PRE_TRANSLATION(pageOffsetRange)
            val wordHit = (sqReg.address.mask & DATA_MASK) =/= 0
            hits(sqReg.id) := sqReg.valid && !sqReg.waitOn.address && pageHit && wordHit && youngerMask(sqReg.id)
          }

          SQCHECK_HITS := hits
          SQCHECK_NO_OLDER := olderMaskEmpty
        }

        val checkSqArbi = new Area{
          val stage = stages(loadCheckSqAt + 1) //Warning, if you remove the +1 remove some of the OLDER_STORE_COMPLETED bypass
          import stage._

          val olderHit = !SQCHECK_NO_OLDER && SQCHECK_HITS =/= 0
          val olderOh   = if(sqSize == 1) B(1) else OHMasking.roundRobinMaskedFull(SQCHECK_HITS.reversed, ~((sq.ptr.priority ## !sq.ptr.priority.msb).reversed)).reversed //reverted priority, imprecise would be ok
          val olderSel  = OHToUInt(olderOh)

          OLDER_STORE_RESCHEDULE := olderHit
          OLDER_STORE_ID := olderSel
          OLDER_STORE_COMPLETED := sq.ptr.onFreeLast.valid && sq.ptr.onFreeLast.payload === OLDER_STORE_ID
          for(s <- stages.dropWhile(_ != stage)){
            s.overloaded(OLDER_STORE_COMPLETED) := s(OLDER_STORE_COMPLETED) || sq.ptr.onFree.valid && sq.ptr.onFree.payload === s(OLDER_STORE_ID)
          }


//          OLDER_STORE_RESCHEDULE := False
//          OLDER_STORE_ID := 0
//          OLDER_STORE_COMPLETED := False
        }

        val cacheRsp = new Area{
          val stage = stages.last
          import stage._

          val rsp = CombInit(setup.cacheLoad.rsp)
          val peripheralOverride = False //Allow the peripheral ctrl to cannibalise this data path logic <3

          val rspSize = CombInit(stage(SIZE))
          val rspAddress = CombInit(stage(ADDRESS_PRE_TRANSLATION))
          val rspRaw = CombInit(rsp.data.subdivideIn(wordWidth bits).read(ADDRESS_PRE_TRANSLATION(memToCpuRange)))
          val rspSplits = rspRaw.subdivideIn(8 bits)
          val rspShifted = Bits(wordWidth bits)

          //Generate minimal mux to move from a wide aligned memory read to the register file shifter representation
          for(i <- 0 until wordBytes){
            val srcSize = 1 << (log2Up(wordBytes) - log2Up(i+1))
            val srcZipped = rspSplits.zipWithIndex.filter{case (v, b) => b % (wordBytes/srcSize) == i}
            val src = srcZipped.map(_._1)
            val range = cpuWordToRfWordRange.high downto cpuWordToRfWordRange.high+1-log2Up(srcSize)
            val sel = rspAddress(range)
            //        println(s"$i $srcSize $range ${srcZipped.map(_._2).mkString(",")}")
            rspShifted(i*8, 8 bits) := src.read(sel)
          }

          assert(Global.XLEN.get == 32)
          val rspFormated = rspSize.mux(
            0 -> B((31 downto 8) -> (rspShifted(7) && !UNSIGNED),(7 downto 0) -> rspShifted(7 downto 0)),
            1 -> B((31 downto 16) -> (rspShifted(15) && !UNSIGNED),(15 downto 0) -> rspShifted(15 downto 0)),
            default -> rspShifted //W
          )

          setup.rfWrite.valid   := False
          setup.rfWrite.address := decoder.PHYS_RD
          setup.rfWrite.data    := rspFormated
          setup.rfWrite.robId   := ROB.ID

          setup.loadCompletion.valid := False
          setup.loadCompletion.id := ROB.ID

          val wakeRob = Flow(WakeRob())
          wakeRob.valid := False
          wakeRob.robId := ROB.ID

          val wakeRf = Flow(WakeRegFile(decoder.PHYS_RD, needBypass = false))
          wakeRf.valid := False
          wakeRf.physical := decoder.PHYS_RD

          setup.loadTrap.valid      := False
          setup.loadTrap.robId      := ROB.ID
          setup.loadTrap.tval       := B(ADDRESS_PRE_TRANSLATION)
          setup.loadTrap.skipCommit := True
          setup.loadTrap.cause.assignDontCare()
          setup.loadTrap.reason := ScheduleReason.TRAP

          tpk.IO || tpk.PAGE_FAULT || !tpk.ALLOW_READ || tpk.REDO
          val missAligned = (1 to log2Up(wordWidth/8)).map(i => SIZE === i && ADDRESS_PRE_TRANSLATION(i-1 downto 0) =/= 0).orR
          val pageFault = !tpk.ALLOW_READ || tpk.PAGE_FAULT
          val accessFault = CombInit(rsp.fault)

          def onRegs(body : RegType => Unit) = for(reg <- regs) when(LQ_SEL_OH(reg.id)){ body(reg) }
          when(isFireing) {
            mem.addressPost.write(
              address = LQ_SEL,
              data   = tpk.TRANSLATED
            )
            mem.io.write(
              address = LQ_SEL,
              data   = tpk.IO
            )
            onRegs(_.waitOn.cacheRsp := False)
            when(OLDER_STORE_RESCHEDULE){
              onRegs{r =>
                r.waitOn.sq setWhen(!stage.resulting(OLDER_STORE_COMPLETED))
                r.waitOn.sqId := OLDER_STORE_ID
              }
            } elsewhen(rsp.redo) {
              when(rsp.refillSlotAny) {
                onRegs(_.waitOn.cacheRefillAnySet := True)
              } otherwise {
                onRegs(_.waitOn.cacheRefillSet := rsp.refillSlot)
              }
            } elsewhen(missAligned) {
              setup.loadTrap.valid := True
              setup.loadTrap.cause := CSR.MCAUSE.LOAD_MISALIGNED
            } elsewhen(pageFault) {
              setup.loadTrap.valid := True
              setup.loadTrap.cause := CSR.MCAUSE.LOAD_PAGE_FAULT
            } elsewhen (accessFault) {
              setup.loadTrap.valid := True
              setup.loadTrap.cause := CSR.MCAUSE.LOAD_ACCESS_FAULT
            } otherwise {
              when(tpk.IO || !peripheralOverride) {
                onRegs(_.waitOn.commit := True)
              }
              when(!tpk.IO && !peripheralOverride) {
                setup.loadCompletion.valid := True
                when(WRITE_RD) {
                  setup.rfWrite.valid := True
                  wakeRob.valid := True
                  wakeRf.valid := True
                }
              }
            }
          }
        }
      }


      val onCommit = new Area{
        val event = commit.onCommit()
        val lqAlloc = rob.readAsync(LQ_ALLOC, Global.COMMIT_COUNT, event.robId)
        val lqCommits = (0 until Global.COMMIT_COUNT).map(slotId => event.mask(slotId) && lqAlloc(slotId))
        var free = CombInit(ptr.free)
        var priority = CombInit(ptr.priority)
        for(inc <- lqCommits){
          for(reg <- regs) when(free.resize(log2Up(lqSize)) === reg.id && inc){
            reg.valid := False
          }
          when(inc) {
            priority \= (priority === 0) ? B(widthOf(priority) bits, default -> true).resized | (priority |<< 1)
          }
          free \= free + U(inc)
        }
        ptr.priority := priority
        ptr.free := free
      }
      pipeline.build()
    }


    val store = new Area{
      import sq._
      for(spec <- storePorts){
        import spec._
        mem.addressPre.write(
          enable = port.valid,
          address = port.sqId,
          data = port.address
        )
        mem.word.write(
          enable = port.valid,
          address = port.sqId,
          data = port.data
        )
        mem.robId.write(
          enable = port.valid,
          address = port.sqId,
          data = port.robId
        )
        when(port.valid) {
          for (entry <- regs) when(port.sqId === entry.id) {
            entry.waitOn.address := False
            entry.address.pageOffset := port.address(pageOffsetRange)
            entry.address.size := port.size
            entry.address.mask := AddressToMask(port.address, port.size, wordBytes)
          }
        }
      }

      val allocate = new Area{
        import allocStage._

        val full = False
        haltIt(isValid && full)

        var alloc = CombInit(ptr.alloc)

        for(slotId <- 0 until Frontend.DISPATCH_COUNT){
          implicit val _ = StageableOffset(slotId)
          SQ_ID := alloc.resized
          SQ_ID_CARRY := alloc.msb
          when(DISPATCH_MASK && SQ_ALLOC){
            LSU_ID := alloc.resized
            when(isFireing) {
              mem.lqAlloc.write(
                address = SQ_ID,
                data = U(LQ_ID_CARRY ## allocStage(LQ_ID, slotId))
              )
            }
            when(ptr.isFull(alloc)){
              full := True
            }
            alloc \= alloc + 1
          }
        }

        when(isFireing){
          ptr.alloc := alloc
          for(reg <- regs){
            val hits = for(slotId <- 0 until Frontend.DISPATCH_COUNT) yield{
              (DISPATCH_MASK, slotId) && (SQ_ALLOC, slotId) && (LSU_ID, slotId).resize(log2Up(sqSize) bits) === reg.id
            }
            when(hits.orR){
              reg.valid := True
              reg.allLqIsYounger := False
              reg.waitOn.address := True
              reg.waitOn.translationRsp := False
              reg.waitOn.writeback := False
              reg.waitOn.translationWake := 0
              reg.waitOn.translationWakeAny := False
            }
          }
        }
      }

      val pipeline = new Pipeline {
        val stages = Array.fill(4)(newStage()) //TODO
        connect(stages)(List(M2S()))

        stages.last.flushIt(rescheduling.valid, root = false)

        val translationPort = translationService.newTranslationPort(
          stages = this.stages,
          preAddress = ADDRESS_PRE_TRANSLATION,
          p = storeTranslationParameter
        )
        val tpk = translationPort.keys

        val feed = new Area{
          val stage = stages(0)
          import stage._

          val hits = B(regs.map(_.ready))
          val hit = hits.orR
          val selOh = OHMasking.roundRobinMasked(hits, ptr.priority)
          val sel = OHToUInt(selOh)

          for(reg <- regs) when(selOh(reg.id)){
            reg.waitOn.translationRsp := True
          }

          isValid := hit
          SQ_SEL := sel
          SQ_SEL_OH := selOh
          ROB.ID := mem.robId.readAsync(sel)
          ADDRESS_PRE_TRANSLATION := mem.addressPre.readAsync(sel)
          SIZE := regs.map(_.address.size).read(sel)
          DATA_MASK := AddressToMask(ADDRESS_PRE_TRANSLATION, SIZE, wordBytes)
          LQCHECK_START_ID := mem.lqAlloc.readAsync(sel)
        }

        //TODO timings
        val checkLqHits = new Area{
          val stage = stages(1)
          import stage._

          val endId = CombInit(lq.ptr.alloc)
          val startMask = U(UIntToOh(U(LQCHECK_START_ID.dropHigh(1))))-1
          val endMask   = U(UIntToOh(U(endId.dropHigh(1))))-1
          val loopback = endMask <= startMask
          val youngerMask = loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask)
          val youngerMaskEmpty = LQCHECK_START_ID === endId
          val allLqIsYounger = regs.map(_.allLqIsYounger).read(SQ_SEL)

          val entries = for(lqReg <- lq.regs) yield new Area {
            val pageHit = lqReg.address.pageOffset === ADDRESS_PRE_TRANSLATION(pageOffsetRange)
            val wordHit = (lqReg.address.mask & DATA_MASK) =/= 0
            LQCHECK_HITS(lqReg.id) := lqReg.valid && !lqReg.waitOn.address && pageHit && wordHit && (youngerMask(lqReg.id) || allLqIsYounger)
          }

          LQCHECK_NO_YOUNGER := youngerMaskEmpty
        }

        val checkLqPrio = new Area{
          val stage = stages(2)
          import stage._

          val youngerHit  = LQCHECK_HITS =/= 0 && !LQCHECK_NO_YOUNGER
          val youngerOh   = OHMasking.roundRobinMasked(stage(LQCHECK_HITS), lq.ptr.priority)
          val youngerSel  = OHToUInt(youngerOh)

          YOUNGER_LOAD_PC := lq.mem.pc(youngerSel)
          YOUNGER_LOAD_ROB := lq.mem.robId.readAsync(youngerSel)
          YOUNGER_LOAD_RESCHEDULE := youngerHit

//          YOUNGER_LOAD_PC := 0
//          YOUNGER_LOAD_ROB := 0
//          YOUNGER_LOAD_RESCHEDULE := False
        }

        val completion = new Area{
          val stage = stages.last
          import stage._

          setup.storeCompletion.valid := False
          setup.storeCompletion.id := ROB.ID

          when(YOUNGER_LOAD_RESCHEDULE){
            setup.storeTrap.valid    := isFireing
            setup.storeTrap.trap     := False
            setup.storeTrap.robId    := YOUNGER_LOAD_ROB
            setup.storeTrap.reason   := ScheduleReason.STORE_TO_LOAD_HAZARD
          } otherwise {
            setup.storeTrap.valid      := False
            setup.storeTrap.trap       := True
            setup.storeTrap.robId      := ROB.ID
            setup.storeTrap.reason     := ScheduleReason.TRAP
          }

          setup.storeTrap.tval       := B(ADDRESS_PRE_TRANSLATION)
          setup.storeTrap.skipCommit := True
          setup.storeTrap.cause.assignDontCare()
          setup.storeTrap.pcTarget   := YOUNGER_LOAD_PC

          val missAligned = (1 to log2Up(wordWidth/8)).map(i => SIZE === i && ADDRESS_PRE_TRANSLATION(i-1 downto 0) =/= 0).orR
          val pageFault = !tpk.ALLOW_WRITE || tpk.PAGE_FAULT

          def onRegs(body : RegType => Unit) = for(reg <- regs) when(SQ_SEL_OH(reg.id)){ body(reg) }
          when(isFireing) {
            onRegs(_.waitOn.translationRsp := False)

            mem.addressPost.write(
              address = SQ_SEL,
              data   = tpk.TRANSLATED
            )
            mem.io.write(
              address = SQ_SEL,
              data   = tpk.IO
            )

            when(tpk.REDO){
              whenMasked(regs, SQ_SEL_OH){reg =>
                reg.waitOn.translationWakeSet := tpk.WAKER
                reg.waitOn.translationWakeAnySet := tpk.WAKER_ANY
              }
            } elsewhen(missAligned) {
              setup.storeTrap.valid      := True
              setup.storeTrap.cause      := CSR.MCAUSE.STORE_MISALIGNED
            } elsewhen(pageFault) {
              setup.storeTrap.valid      := True
              setup.storeTrap.cause      := CSR.MCAUSE.STORE_PAGE_FAULT
            } otherwise {
              onRegs(_.waitOn.writeback := True)
              when(tpk.IO) {

              } otherwise {
                setup.storeCompletion.valid := True
              }
            }
          }

        }

        build()
      }

      val onCommit = new Area{
        val event = commit.onCommit()
        val sqAlloc = rob.readAsync(SQ_ALLOC, Global.COMMIT_COUNT, event.robId)
        val sqCommits = (0 until Global.COMMIT_COUNT).map(slotId => U(event.mask(slotId) && sqAlloc(slotId)))
        var commitComb = CombInit(ptr.commit)
        for(slotId <- 0 until Global.COMMIT_COUNT){
          val doit = event.mask(slotId) && sqAlloc(slotId)
          when(doit) { regs.map(_.commitedNext).write(U(commitComb.dropHigh(1)), True) } //TODO this is kind of a long combinatorial path (commit -> adder -> reg set)
          commitComb \= commitComb + U(doit)
        }
        ptr.commitNext := commitComb
      }

      val writeback = new Area{
        val generation = RegInit(False)
//        val pipeline = new Pipeline {
//          val stages = Array.fill(3)(newStage()) //TODO
//          connect(stages)(List(M2S()))
//        }

        val waitOn = new Area{
          val refillSlot    = Reg(Bits(cache.refillCount bits)) init(0) //Zero when refillSlotAny
          val refillSlotAny = Reg(Bool()) init(False)

          val ready = refillSlot === 0 && !refillSlotAny

          val refillSlotSet = refillSlot.getZero
          val refillSlotAnySet = False

          refillSlot := (refillSlot | refillSlotSet) & ~cache.refillCompletions
          refillSlotAny := (refillSlotAny | refillSlotAnySet) & !cache.refillCompletions.orR
        }

        val feed = new Area{
          //WARNING, setupCacheStore is also used by the peripheral controller to know what to do
          val io = sq.mem.io.readAsync(ptr.writeBackReal)
          val size = regs.map(_.address.size).read(ptr.writeBackReal)
          val data = mem.word.readAsync(ptr.writeBackReal)
          val doit = ptr.writeBack =/= ptr.commit && waitOn.ready
          val fire = CombInit(doit)

          setup.cacheStore.cmd.valid := doit
          setup.cacheStore.cmd.address := mem.addressPost.readAsync(ptr.writeBackReal)
          setup.cacheStore.cmd.mask :=  AddressToMask(setup.cacheStore.cmd.address, size, widthOf(setup.cacheStore.cmd.mask))
          setup.cacheStore.cmd.generation := generation
          setup.cacheStore.cmd.data.assignDontCare()
          setup.cacheStore.cmd.io := io
          switch(size){
            for(s <- 0 to log2Up(widthOf(setup.cacheStore.cmd.data)/8)) is(s){
              val w = (1 << s)*8
              setup.cacheStore.cmd.data.subdivideIn(w bits).foreach(_ := data(0, w bits))
            }
          }

          ptr.writeBack := ptr.writeBack + U(fire)
        }

//        val peripheralBypass = new Area{
//          val valid = Delay(feed.fire && feed.io, cache.storeRspLatency, init = False)
//          val generation = Delay(generation, cache.storeRspLatency)
//        }

        val rsp = new Area{
          val hazardFreeDelay = loadCheckSqAt - (loadFeedAt + cache.loadCmdHazardFreeLatency) + cache.storeRspHazardFreeLatency - 1 // -1 because sq regs update is sequancial
          val delayed = Vec.fill((hazardFreeDelay + 1) max 0)(cloneOf(setup.cacheStore.rsp))
          delayed.head << setup.cacheStore.rsp

          for((m,s) <- (delayed, delayed.tail).zipped) {
            val patched = m.combStage()
            s << patched.stage()
            patched.refillSlot.removeAssignments()
            patched.refillSlotAny.removeAssignments()
            patched.refillSlot  := m.refillSlot & ~cache.refillCompletions
            patched.refillSlotAny := m.refillSlotAny && cache.refillCompletions === 0
          }


          sq.ptr.onFree.valid := False
          sq.ptr.onFree.payload := ptr.freeReal

          when(delayed.last.valid && !delayed.last.generationKo){
            when(delayed.last.redo) {
              waitOn.refillSlotSet := delayed.last.refillSlot
              waitOn.refillSlotAnySet := delayed.last.refillSlotAny
              generation := !generation
              ptr.writeBack := ptr.free
            } otherwise {
              sq.ptr.onFree.valid := True
              ptr.free := ptr.free + 1
              ptr.priority := ptr.priority |<< 1
              when(ptr.priority === 0){
                ptr.priority := (default -> true)
              }
              for(reg <- regs) when(ptr.freeReal === reg.id){
                reg.valid := False
                reg.commited := False
              }
            }
          }
        }
      }
    }

    val peripheral = new Area{
      val lqOnTop = lq.mem.robId.readAsync(lq.ptr.freeReal) === commit.nextCommitRobId
      val sqOnTop = sq.mem.robId.readAsync(sq.ptr.commitReal) === commit.nextCommitRobId
      val storeWriteBackUsable = sq.ptr.writeBack === sq.ptr.commit
      val storeHit = sqOnTop && storeWriteBackUsable && sq.regs.map(reg => reg.valid && reg.waitOn.writeback).read(sq.ptr.commitReal) && sq.mem.io.readAsync(sq.ptr.commitReal)
      val loadHit = lqOnTop && lq.regs.map(reg => reg.valid && reg.waitOn.commit).read(lq.ptr.freeReal) && lq.mem.io.readAsync(lq.ptr.freeReal)
      val hit = storeHit || loadHit

      val fire = RegNext(peripheralBus.rsp.fire) init(False)
      val enabled = RegInit(False) setWhen(hit) clearWhen(fire)
      val isStore = RegNextWhen(storeHit, hit)
      val isLoad = RegNextWhen(!storeHit, hit)
      val cmdSent = RegInit(False) setWhen(peripheralBus.cmd.fire) clearWhen(fire)

      val robId = RegNext(commit.nextCommitRobId)
      val physRd = RegNext(lq.mem.physRd.readAsync(lq.ptr.freeReal))
      val loadAddress = RegNext(lq.mem.addressPost.readAsync(lq.ptr.freeReal))
      val loadSize = RegNext(lq.regs.map(_.address.size).read(lq.ptr.freeReal))
      val loadWriteRd = RegNext(lq.mem.writeRd.readAsync(lq.ptr.freeReal))
      val storeAddress = RegNextWhen(setup.cacheStore.cmd.address, hit)
      val storeSize = RegNextWhen(store.writeback.feed.size, hit)
      val storeData = RegNextWhen(setup.cacheStore.cmd.data, hit)
      val storeMask = RegNextWhen(setup.cacheStore.cmd.mask, hit)

      peripheralBus.cmd.valid := enabled && !cmdSent
      peripheralBus.cmd.write   := isStore
      peripheralBus.cmd.address := isStore ? storeAddress otherwise loadAddress
      peripheralBus.cmd.size    := isStore ? storeSize otherwise loadSize
      peripheralBus.cmd.data    := storeData
      peripheralBus.cmd.mask    := storeMask


      when(peripheralBus.rsp.fire) {
        //TODO handle trap, maybe can use a dedicated port which always win (lighter arbitration) as it is the next to commit
        setup.loadCompletion.valid := True
        setup.loadCompletion.id := robId
        load.pipeline.cacheRsp.peripheralOverride := True

        setup.rfWrite.valid               setWhen(isLoad && loadWriteRd)
        setup.rfWrite.address             := physRd
        setup.rfWrite.robId               := robId
        load.pipeline.cacheRsp.rspAddress := loadAddress
        load.pipeline.cacheRsp.rspSize    := loadSize
        load.pipeline.cacheRsp.rspRaw     := peripheralBus.rsp.data

        load.pipeline.cacheRsp.wakeRob.valid setWhen(isLoad && loadWriteRd)
        load.pipeline.cacheRsp.wakeRob.robId := robId

        load.pipeline.cacheRsp.wakeRf.valid setWhen(isLoad && loadWriteRd)
        load.pipeline.cacheRsp.wakeRf.physical := physRd
      }
    }


    //Store some robId related context for later uses
    def remapped[T <: Data](key : Stageable[T]) : Seq[T] = (0 until Frontend.DISPATCH_COUNT).map(allocStage(key, _))
    def writeLine[T <: Data](key : Stageable[T]) : Unit = writeLine(key, remapped(key))
    def writeLine[T <: Data](key : Stageable[T], value : Seq[T]) : Unit  = {
      rob.write(
        key = key,
        size = DISPATCH_COUNT,
        value = value,
        robId = allocStage(ROB.ID),
        enable = allocStage.isFireing
      )
    }
    writeLine(LSU_ID)
    writeLine(SQ_ALLOC)
    writeLine(LQ_ALLOC)

    when(rescheduling.valid){
      lq.regs.foreach(_.valid := False)
      lq.ptr.free := 0
      lq.ptr.alloc := 0
      lq.ptr.priority := 0
      for(reg <- sq.regs){
        reg.valid clearWhen(!reg.commitedNext)
        reg.allLqIsYounger := True
      }
      sq.ptr.alloc := sq.ptr.commitNext
      peripheral.enabled := False
    }

    rob.release()
    decoder.release()
    frontend.release()
    translationService.release()
  }


}
