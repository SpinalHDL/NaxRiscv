package naxriscv.units.lsu

import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK, ROB_ID}
import naxriscv.backend.RobPlugin
import naxriscv.{Frontend, Global, ROB}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces._
import naxriscv.utilities.{AddressToMask, Plugin}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stageable, StageableOffset}

import scala.collection.mutable.ArrayBuffer

object LsuUtils{
  def sizeWidth(wordWidth : Int) = log2Up(log2Up(wordWidth/8)+1)
}

case class LsuLoadPort(lqSize : Int, wordWidth : Int, physicalRdWidth : Int, pcWidth : Int) extends Bundle {
  val robId = ROB.ID_TYPE()
  val lqId = UInt(log2Up(lqSize) bits)
  val address = UInt(Global.XLEN bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
  val unsigned = Bool()
  val physicalRd = UInt(physicalRdWidth bits)
  val pc = UInt(pcWidth bits)
}

case class LsuStorePort(sqSize : Int, wordWidth : Int) extends Bundle {
  val robId = ROB.ID_TYPE()
  val sqId = UInt(log2Up(sqSize) bits)
  val address = UInt(Global.XLEN bits)
  val data = Bits(wordWidth bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
}


class LsuPlugin(lqSize: Int,
                sqSize : Int,
                loadFeedAt : Int = 0, //Stage at which the d$ cmd is sent
                loadCheckSqAt : Int = 1) extends Plugin with LockedImpl with WakeRobService with WakeRegFileService {

  val wordWidth = Global.XLEN.get
  val wordBytes = wordWidth/8
  val wordSizeWidth = LsuUtils.sizeWidth(wordWidth)
  val pageOffsetRange = 11 downto log2Up(wordBytes)
  val pageNumberRange = Global.XLEN.get-1 downto 12
  val pageOffsetWidth = pageOffsetRange.size
  val pageNumberWidth = pageNumberRange.size

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
    val UNSIGNED = Stageable(Bool)
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

    rob.retain()
    decoder.retain()
    frontend.retain()

    val rfWrite = regfile.newWrite(withReady = false, latency = 1)
    val cacheLoad = cache.newLoadPort()
    val cacheStore = cache.newStorePort()
    val loadCompletion = rob.newRobCompletion()
    val storeCompletion = rob.newRobCompletion()
    val loadTrap = commit.newSchedulePort(canTrap = true, canJump = false)
    val storeTrap = commit.newSchedulePort(canTrap = true, canJump = true)

    decoder.addResourceDecoding(naxriscv.interfaces.LQ, LQ_ALLOC)
    decoder.addResourceDecoding(naxriscv.interfaces.SQ, SQ_ALLOC)
  }

  val logic = create late new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val cache = getService[DataCachePlugin]
    val commit = getService[CommitService]
    val PC = getService[AddressTranslationService].PC
    lock.await()

    val keysLocal = new AreaRoot {
      val LQ_ID = Stageable(UInt(log2Up(lqSize) bits))
      val SQ_ID = Stageable(UInt(log2Up(sqSize) bits))

      val LQ_ID_CARRY = Stageable(Bool())
      val SQ_ID_CARRY = Stageable(Bool())

      val YOUNGER_LOAD_PC         = Stageable(PC)
      val YOUNGER_LOAD_ROB        = Stageable(ROB.ID_TYPE)
      val YOUNGER_LOAD_RESCHEDULE = Stageable(Bool())

      val OLDER_STORE_RESCHEDULE  = Stageable(Bool())
      val OLDER_STORE_ID = Stageable(SQ_ID)
      val OLDER_STORE_COMPLETED = Stageable(Bool())
    }
    import keysLocal._

    val cpuWordToRfWordRange = log2Up(wordBytes)-1 downto log2Up(wordBytes) //useless for now

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
        val address = Mem.fill(lqSize)(UInt(virtualAddressWidth bits))
        val physRd = Mem.fill(lqSize)(decoder.PHYS_RD)
        val robId = Mem.fill(lqSize)(ROB.ID_TYPE)
        val pc = Mem.fill(lqSize)(PC)
        val sqAlloc = Mem.fill(lqSize)(UInt(log2Up(sqSize)+1 bits))
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
          val writeback  = Reg(Bool())
        }

        val ready = valid && !waitOn.address && !waitOn.translationRsp && !waitOn.writeback
      }

      val mem = new Area{
        val address = Mem.fill(sqSize)(UInt(virtualAddressWidth bits))
        val word = Mem.fill(sqSize)(Bits(wordWidth bits))
        val robId = Mem.fill(sqSize)(ROB.ID_TYPE)
        val lqAlloc = Mem.fill(sqSize)(UInt(log2Up(lqSize) + 1 bits))
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
      }
    }

    val load = new Area{
      import lq._
      for(spec <- loadPorts){
        import spec._
        mem.address.write(
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
            mem.sqAlloc.write(
              address = LQ_ID,
              data = U(SQ_ID_CARRY ## allocStage(SQ_ID, slotId))
            )
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
        val stages = Array.fill(cache.loadRspLatency + 1)(newStage())
        connect(stages)(List(M2S()))

        stages.last.flushIt(rescheduling.valid, root = false)



        val feed = new Area{
          val stage = stages(loadFeedAt)
          import stage._

          val hits = B(regs.map(_.ready))
          val hit = hits.orR

          val selOh = OHMasking.roundRobinMasked(hits, ptr.priority)
          val sel = OHToUInt(selOh)

          val cmd = setup.cacheLoad.cmd //If you move that in another stage, be carefull to update loadFeedAt usages (sq d$ writeback rsp delay)
          cmd.valid := hit
          cmd.virtual := mem.address.readAsync(sel)
          cmd.size := SIZE
          for(reg <- regs) when(selOh(reg.id)){
            reg.waitOn.cacheRsp := True
          }

          isValid := cmd.fire
          LQ_SEL := sel
          LQ_SEL_OH := selOh
          decoder.PHYS_RD := mem.physRd.readAsync(sel)
          ROB.ID_TYPE := mem.robId.readAsync(sel)
          ADDRESS_PRE_TRANSLATION := cmd.virtual
          SIZE     := regs.map(_.address.size).read(sel)
          UNSIGNED := regs.map(_.address.unsigned).read(sel)
          DATA_MASK := AddressToMask(ADDRESS_PRE_TRANSLATION, SIZE, wordBytes)
        }

        val cancels = for((stage, stageId) <- stages.zipWithIndex){
          setup.cacheLoad.cancels(stageId) := stage.isValid && rescheduling.valid
        }

        val checkSq = new Area{
          val stage = stages(loadCheckSqAt) //WARNING, SQ delay between writeback and entry.valid := False should not be smaller than the delay of reading the cache and checkSq !!
          import stage._

          val startId = CombInit(sq.ptr.free)
          val endId = mem.sqAlloc.readAsync(LQ_SEL)
          val startMask = U(UIntToOh(U(startId.dropHigh(1))))-1
          val endMask   = U(UIntToOh(U(endId.dropHigh(1))))-1
          val loopback = endMask <= startMask
          val youngerMask = loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask)
          val olderMaskEmpty = startId === endId

          val hits = Bits(sqSize bits)
          val entries = for(sqReg <- sq.regs) yield new Area {
            val pageHit = sqReg.address.pageOffset === ADDRESS_PRE_TRANSLATION(pageOffsetRange)
            val wordHit = (sqReg.address.mask & DATA_MASK) =/= 0
            hits(sqReg.id) := sqReg.valid && !sqReg.waitOn.address && pageHit && wordHit && youngerMask(sqReg.id)
          }
          val olderHit = !olderMaskEmpty && hits =/= 0
          val olderOh   = if(sqSize == 1) B(1) else OHMasking.roundRobinMaskedFull(hits.reversed, ~((sq.ptr.priority ## !sq.ptr.priority.msb).reversed)).reversed //reverted priority, imprecise would be ok
          val olderSel  = OHToUInt(olderOh)

          OLDER_STORE_RESCHEDULE := olderHit
          OLDER_STORE_ID := olderSel
          PC := mem.pc(LQ_SEL)

          OLDER_STORE_COMPLETED := False
          for(s <- stages.dropWhile(_ != stage)){
            s.overloaded(OLDER_STORE_COMPLETED) := s(OLDER_STORE_COMPLETED) || sq.ptr.onFree.valid && sq.ptr.onFree.payload === s(OLDER_STORE_ID)
          }
        }

        val cacheRsp = new Area{
          val stage = stages.last
          import stage._

          val rsp = setup.cacheLoad.rsp

          val rspSplits = rsp.data.subdivideIn(8 bits)
          val rspShifted = Bits(wordWidth bits)

          //Generate minimal mux to move from a wide aligned memory read to the register file shifter representation
          for(i <- 0 until wordBytes){
            val srcSize = 1 << (log2Up(wordBytes) - log2Up(i+1))
            val srcZipped = rspSplits.zipWithIndex.filter{case (v, b) => b % (wordBytes/srcSize) == i}
            val src = srcZipped.map(_._1)
            val range = cpuWordToRfWordRange.high downto cpuWordToRfWordRange.high+1-log2Up(srcSize)
            val sel = ADDRESS_PRE_TRANSLATION(range)
            //        println(s"$i $srcSize $range ${srcZipped.map(_._2).mkString(",")}")
            rspShifted(i*8, 8 bits) := src.read(sel)
          }

          assert(Global.XLEN.get == 32)
          val rspFormated = SIZE.mux(
            0 -> B((31 downto 8) -> (rspShifted(7) && !UNSIGNED),(7 downto 0) -> rspShifted(7 downto 0)),
            1 -> B((31 downto 16) -> (rspShifted(15) && !UNSIGNED),(15 downto 0) -> rspShifted(15 downto 0)),
            default -> rspShifted //W
          )

          setup.rfWrite.valid   := False
          setup.rfWrite.address := decoder.PHYS_RD
          setup.rfWrite.data    := rspFormated
          setup.rfWrite.robId   := ROB.ID_TYPE

          setup.loadCompletion.valid := False
          setup.loadCompletion.id := ROB.ID_TYPE

          val wakeRob = Flow(WakeRob())
          wakeRob.valid := False
          wakeRob.robId := ROB.ID_TYPE

          val wakeRf = Flow(WakeRegFile(decoder.PHYS_RD, needBypass = false))
          wakeRf.valid := False
          wakeRf.physical := decoder.PHYS_RD

          setup.loadTrap.valid      := False
          setup.loadTrap.robId      := ROB.ID_TYPE
          setup.loadTrap.tval       := B(ADDRESS_PRE_TRANSLATION)
          setup.loadTrap.skipCommit := True
          setup.loadTrap.cause.assignDontCare()

          def onRegs(body : RegType => Unit) = for(reg <- regs) when(LQ_SEL_OH(reg.id)){ body(reg) }
          when(isValid) {
            onRegs(_.waitOn.cacheRsp := False)
            when(OLDER_STORE_RESCHEDULE){
              onRegs{r =>
                r.waitOn.sq setWhen(!stage.overloaded(OLDER_STORE_COMPLETED))
                r.waitOn.sqId := OLDER_STORE_ID
              }
            } elsewhen(rsp.redo) {
              when(rsp.refillSlotAny) {
                onRegs(_.waitOn.cacheRefillAnySet := True)
              } otherwise {
                onRegs(_.waitOn.cacheRefillSet := rsp.refillSlot)
              }
            } elsewhen (rsp.fault) {
              setup.loadTrap.valid := True
              setup.loadTrap.cause := 5
            } otherwise {
              onRegs(_.waitOn.commit := True)
              setup.rfWrite.valid := True
              setup.loadCompletion.valid := True
              wakeRob.valid := True
              wakeRf.valid := True
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
        mem.address.write(
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
            mem.lqAlloc.write(
              address = SQ_ID,
              data = U(LQ_ID_CARRY ## allocStage(LQ_ID, slotId))
            )
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
              reg.waitOn.address := True
              reg.waitOn.translationRsp := False
              reg.waitOn.writeback := False
            }
          }
        }
      }

      val pipeline = new Pipeline {
        val stages = Array.fill(3)(newStage()) //TODO
        connect(stages)(List(M2S()))

        stages.last.flushIt(rescheduling.valid, root = false)

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
          ROB.ID_TYPE := mem.robId.readAsync(sel)
          ADDRESS_PRE_TRANSLATION := mem.address.readAsync(sel)
          SIZE := regs.map(_.address.size).read(sel)
          DATA_MASK := AddressToMask(ADDRESS_PRE_TRANSLATION, SIZE, wordBytes)
        }

        val translated = new Area{
          val stage = stages(1)
          import stage._

        }

        //TODO timings
        val checkLq = new Area{
          val stage = stages(1)
          import stage._

          val startId = mem.lqAlloc.readAsync(SQ_SEL)
          val endId = CombInit(lq.ptr.alloc)
          val startMask = U(UIntToOh(U(startId.dropHigh(1))))-1
          val endMask   = U(UIntToOh(U(endId.dropHigh(1))))-1
          val loopback = endMask <= startMask
          val youngerMask = loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask)
          val youngerMaskEmpty = startId === endId

          val hits = Bits(lqSize bits)
          val entries = for(lqReg <- lq.regs) yield new Area {
            val pageHit = lqReg.address.pageOffset === ADDRESS_PRE_TRANSLATION(pageOffsetRange)
            val wordHit = (lqReg.address.mask & DATA_MASK) =/= 0
            hits(lqReg.id) := lqReg.valid && !lqReg.waitOn.address && pageHit && wordHit && youngerMask(lqReg.id)
          }
          val youngerHit  = hits =/= 0 && !youngerMaskEmpty
          val youngerOh   = OHMasking.roundRobinMasked(hits, lq.ptr.priority)
          val youngerSel  = OHToUInt(youngerOh)

          YOUNGER_LOAD_PC := lq.mem.pc(youngerSel)
          YOUNGER_LOAD_ROB := lq.mem.robId.readAsync(youngerSel)
          YOUNGER_LOAD_RESCHEDULE := youngerHit
        }

        val completion = new Area{
          val stage = stages.last
          import stage._

          setup.storeCompletion.valid := False
          setup.storeCompletion.id := ROB.ID_TYPE

          setup.storeTrap.valid      := False
          setup.storeTrap.trap       := True
          setup.storeTrap.robId      := ROB.ID_TYPE
          setup.storeTrap.tval       := B(ADDRESS_PRE_TRANSLATION)
          setup.storeTrap.skipCommit := True
          setup.storeTrap.cause.assignDontCare()
          setup.storeTrap.pcTarget   := YOUNGER_LOAD_PC

          when(YOUNGER_LOAD_RESCHEDULE){
            setup.storeTrap.valid    setWhen(isFireing)
            setup.storeTrap.trap     := False
            setup.storeTrap.robId    := YOUNGER_LOAD_ROB
          }

          def onRegs(body : RegType => Unit) = for(reg <- regs) when(SQ_SEL_OH(reg.id)){ body(reg) }
          when(isValid) {
            onRegs(_.waitOn.translationRsp := False)

            //TODO implement failures
            onRegs(_.waitOn.writeback := True)
            setup.storeCompletion.valid := True
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
          setup.cacheStore.cmd.valid := ptr.writeBack =/= ptr.commit && waitOn.ready
          setup.cacheStore.cmd.address := mem.address.readAsync(ptr.writeBackReal)
          setup.cacheStore.cmd.data := mem.word.readAsync(ptr.writeBackReal)
          setup.cacheStore.cmd.size := regs.map(_.address.size).read(ptr.writeBackReal)
          setup.cacheStore.cmd.generation := generation

          ptr.writeBack := ptr.writeBack + U(setup.cacheStore.cmd.valid)
        }

        val rsp = new Area{
          val hazardFreeDelay = loadCheckSqAt - (loadFeedAt + cache.loadCmdHazardFreeLatency) + cache.storeRspHazardFreeLatency - 1 // -1 because sq regs update is sequancial
          val delayed = Vec.fill((hazardFreeDelay + 1) max 0)(cloneOf(setup.cacheStore.rsp))
          delayed.head << setup.cacheStore.rsp
          for((m,s) <- (delayed, delayed.tail).zipped) s << m.stage()

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


    //Store some robId related context for later uses
    def remapped[T <: Data](key : Stageable[T]) : Seq[T] = (0 until Frontend.DISPATCH_COUNT).map(allocStage(key, _))
    def writeLine[T <: Data](key : Stageable[T]) : Unit = writeLine(key, remapped(key))
    def writeLine[T <: Data](key : Stageable[T], value : Seq[T]) : Unit  = {
      rob.write(
        key = key,
        size = DISPATCH_COUNT,
        value = value,
        robId = allocStage(ROB_ID),
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
      }
      sq.ptr.alloc := sq.ptr.commitNext
    }

    rob.release()
    decoder.release()
    frontend.release()
  }


}
