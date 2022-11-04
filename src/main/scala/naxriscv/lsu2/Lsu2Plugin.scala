package naxriscv.lsu2

import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.execute.EnvCallPlugin
import naxriscv.fetch.FetchPlugin
import naxriscv.frontend.{DispatchPlugin, FrontendPlugin}
import naxriscv.interfaces.AddressTranslationPortUsage.LOAD_STORE
import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.lsu.{DataCachePlugin, LsuFlushPayload, LsuFlusher, LsuPeripheralBus, LsuUtils}
import naxriscv.misc.RobPlugin
import naxriscv.riscv.{CSR, FloatRegFile, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin, WithRfWriteSharedSpec}
import spinal.core.fiber.hardFork
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

/*
vs the original LsuPlugin :

Pipeline rework ->
- Use the issue queue / EU0 to track depdencies and provide the data to store (greatly reducing area)
  So, no more store data fetch and store completion pipeline
- Load and store address pipeline are now fused, avoiding duplicating MMU ports. This will also allow store prefetch

Out of oder rework ->
- Store to load bypass info will be provided via a predictor instead of a CAM, reducing the need to have early address
- Store to load hazard will be checked via re-execution of the load
- To reduce load re-execution, a store vulnerability window filter is added
- Load to load on the same address ordering (snoop / line refill) will be on the side of the re-execution filter

Overall it should save quite a lot of area, and also reduce timings pressure (no CAM).
Also increasing LQ/SQ size will have much less impact on area and timings.
 */

case class AguPort(aguIdSize : Int,
                   wordWidth : Int,
                   physicalRdWidth : Int,
                   regfileRdWidth : Int,
                   pcWidth : Int) extends Bundle {
  val robId = ROB.ID()
  val robIdMsb = ROB.MSB()
  val aguId = UInt(log2Up(aguIdSize) bits)
  val load = Bool()
  val address = UInt(VIRTUAL_EXT_WIDTH bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
  val unsigned = Bool()
  val physicalRd = UInt(physicalRdWidth bits)
  val regfileRd = UInt(regfileRdWidth bits)
  val writeRd = Bool()
  val pc = UInt(pcWidth bits)
  val lr = Bool()

  val earlySample = Bool()
  val earlyPc = UInt(pcWidth bits) //One cycle early pc, to fetch prediction

  val data  = Bits(wordWidth bits)

  //Atomic stuff
  val amo = Bool()
  val sc = Bool()
  val swap = Bool()
  val op  = Bits(3 bits)

  def robIdFull = robIdMsb @@ robId
}


class Lsu2Plugin(var lqSize: Int,
                 var sqSize : Int,
                 var translationStorageParameter : Any,
                 var sharedTranslationParameter : Any,
                 var sharedAguAt  : Int = 1,
                 var sharedFeedAt : Int = 1, //Stage at which the d$ cmd is sent
                 var sharedCtrlAt : Int = 4,
                 var loadWriteRfOnPrivilegeFail : Boolean = true,
                 var lqToCachePipelined : Boolean = true,
                 var storeReadRfWithBypass : Boolean = false) extends Plugin with WakeRobService with WakeRegFileService with PostCommitBusy with WithRfWriteSharedSpec with LsuFlusher{


  def wordWidth = LSLEN
  def wordBytes = wordWidth/8
  def wordSizeWidth = LsuUtils.sizeWidth(wordWidth)
  def pageOffsetRange = 11 downto log2Up(wordBytes)
  def pageNumberRange = Global.XLEN.get-1 downto 12
  def pageOffsetWidth = pageOffsetRange.size
  def pageNumberWidth = pageNumberRange.size
  override def postCommitBusy = setup.postCommitBusy
  override def getFlushPort() : FlowCmdRsp[LsuFlushPayload, NoData]= setup.flushPort


  case class AguPortSpec(port : Flow[AguPort])
  val aguPorts = ArrayBuffer[AguPortSpec]()
  def newAguPort(): Flow[AguPort] = {
    aguPorts.addRet(AguPortSpec(Flow(AguPort(
      aguIdSize       = (sqSize max lqSize)+1,
      wordWidth       = wordWidth,
      physicalRdWidth = widthOf(getService[DecoderService].PHYS_RD),
      regfileRdWidth  = widthOf(getService[DecoderService].REGFILE_RD),
      pcWidth         = VIRTUAL_EXT_WIDTH
    )))).port
  }

  val keys = new AreaRoot{
    val SQ_ALLOC = Stageable(Bool())
    val LQ_ALLOC = Stageable(Bool())
    val LSU_ID = Stageable(UInt(log2Up(lqSize max sqSize)+1 bits))
    val SIZE = Stageable(UInt(wordSizeWidth bits))
    val UNSIGNED = Stageable(Bool())
    val WRITE_RD = Stageable(Bool())
    val LQ_SEL = Stageable(UInt(log2Up(lqSize) bits))
    val LQ_SEL_OH = Stageable(Bits(lqSize bits))
    val SQ_SEL = Stageable(UInt(log2Up(sqSize) bits))
    val SQ_SEL_OH = Stageable(Bits(sqSize bits))
    val ADDRESS_PRE_TRANSLATION = Stageable(UInt(VIRTUAL_EXT_WIDTH bits))
    val ADDRESS_POST_TRANSLATION = Stageable(UInt(VIRTUAL_EXT_WIDTH bits))
    val DATA_MASK = Stageable(Bits(wordBytes bits))


    val LQ_SEL_ARBI = Stageable(UInt(log2Up(lqSize) bits))
    val SQ_SEL_ARBI = Stageable(UInt(log2Up(sqSize) bits))

    val AMO, LR, SC, REGULAR = Stageable(Bool())
    val MISS_ALIGNED = Stageable(Bool())
    val PAGE_FAULT = Stageable(Bool())
    val ACCESS_FAULT = Stageable(Bool())
    val STORE_DO_TRAP = Stageable(Bool())

    val LQ_HIT = Stageable(Bool())
    val SQ_HIT = Stageable(Bool())

    val LQ_ID = Stageable(UInt(log2Up(lqSize) bits))
    val SQ_ID = Stageable(UInt(log2Up(sqSize) bits))

    val LQ_OLDER_THAN_SQ = Stageable(Bool())

    val LQ_ROB_ID = Stageable(ROB.ID)
    val SQ_ROB_ID = Stageable(ROB.ID)

    val ROB_FULL = Stageable(UInt(ROB.ID_WIDTH + 1 bits))
    val LQ_ROB_FULL = Stageable(ROB_FULL)
    val SQ_ROB_FULL = Stageable(ROB_FULL)


    val NEED_TRANSLATION = Stageable(Bool())
    val LOAD_WRITE_FAILURE = Stageable(Bool()) //True when register file write port was busy (need redo)
    val BYPASS_TOO_EARLY = Stageable(Bool())

    val HIT_SPECULATION, HIT_SPECULATION_WRITE_RD = Stageable(Bool())
//    val HIT_SPECULATION_COUNTER = Stageable(SInt(hitPredictionCounterWidth bits))

    val IS_LOAD = Stageable(Bool())
  }
  import keys._

  override def wakeRobs    = List(logic.get.sharedPip.ctrl.wakeRob/*, logic.get.special.wakeRob*/)
  override def wakeRegFile = List(logic.get.sharedPip.ctrl.wakeRf /*, logic.get.special.wakeRf*/)
  def flushPort = setup.flushPort


  val setup = create early new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val cache = getService[DataCachePlugin]
    val regfiles = getServicesOf[RegfileService]
    val commit = getService[CommitService]
    val translation = getService[AddressTranslationService]
    val doc = getService[DocPlugin]
    val dispatch = getService[DispatchPlugin]
    val fetch = getService[FetchPlugin]
    val postCommitBusy = False

    rob.retain()
    decoder.retain()
    frontend.retain()
    translation.retain()
    fetch.retain()

    getServiceOption[PrivilegedService].foreach(_.addMisa('A'))

    val amos = List(
      Rvi.AMOSWAP, Rvi.AMOADD, Rvi.AMOXOR, Rvi.AMOAND, Rvi.AMOOR,
      Rvi.AMOMIN, Rvi.AMOMAX, Rvi.AMOMINU, Rvi.AMOMAXU
    )
    amos.foreach(dispatch.fenceYounger(_))
    dispatch.fenceOlder  (Rvi.LR)
    dispatch.fenceYounger(Rvi.SC)
    dispatch.fenceOlder  (Rvi.SC)


    class RegfilePorts(regfile : RegfileService) extends Area{
      val read = regfile.newRead(withReady = false, forceNoBypass = !storeReadRfWithBypass)
      val sharing = getRfWriteSharing(regfile.rfSpec)
      assert(sharing.withReady == false)
      val write = regfile.newWrite(false, 0, sharing.key, sharing.priority)
    }
    val regfilePorts = (for(regfile <- regfiles) yield regfile.rfSpec -> new RegfilePorts(regfile)).toMapLinked()
    val cacheLoad = cache.newLoadPort(priority = 0)
    val cacheStore = cache.newStorePort()
    cacheStore.cmd.assignDontCare()
    val sharedCompletion = rob.newRobCompletion()
    val sharedTrap = commit.newSchedulePort(canTrap = true, canJump = false)
    val flushPort = FlowCmdRsp(LsuFlushPayload(), NoData()).setIdleAll() //TODO
    val translationStorage = translation.newStorage(translationStorageParameter)

    decoder.addResourceDecoding(naxriscv.interfaces.LQ, LQ_ALLOC)
    decoder.addResourceDecoding(naxriscv.interfaces.SQ, SQ_ALLOC)

    doc.property("LSU_PERIPHERAL_WIDTH", wordWidth)
    doc.property("RVA", true)
  }

  val peripheralBus = create late master(LsuPeripheralBus(PHYSICAL_WIDTH, wordWidth)).setName("LsuPlugin_peripheralBus")

  val logic = create late new Area{
    val imp = setup.get
    import imp._


    val keysLocal = new AreaRoot {
      val LOAD_CACHE_RSP = Stageable(cloneOf(setup.cacheLoad.rsp))
    }
    import keysLocal._

    val cpuWordToRfWordRange = log2Up(wordBytes)-1 downto log2Up(wordBytes) //useless for now
    val memToCpuRange = log2Up(cache.memDataWidth/8)-1 downto log2Up(wordBytes)

    val rescheduling = commit.reschedulingPort(onCommit = true)

    val translationWake = Bool()
    case class LqRegType(id : Int) extends Area{
      val allocation = False
      val valid = RegInit(False)
      val redo = RegInit(False)
      val redoSet = False
      val delete = False



      val waitOn = new Area {
        val cacheRefill    = Reg(Bits(cache.refillCount bits)) init(0)
        val cacheRefillAny = RegInit(False)
        val mmuRefillAny   = RegInit(False)

        val cacheRefillSet = cacheRefill.getZero
        val mmuRefillAnySet   = False

        cacheRefill    := cacheRefill  | cacheRefillSet
        mmuRefillAny   := mmuRefillAny | mmuRefillAnySet
        redoSet.setWhen(
          (cacheRefill  & cache.refillCompletions).orR ||
           mmuRefillAny & translationWake
        )

        val commitSet = False
        val commit = Reg(Bool()) setWhen(commitSet)

        //        val sq             = Reg(Bool())
//        val sqCompletion = Reg(Bool())
//        val sqId   = Reg(SQ_ID)
//        val sqPredicted = withHazardPrediction generate Reg(Bool())


      }



      when(delete){
        valid := False
      }
      when(allocation){
        valid := True
        waitOn.commit := False
      }
      when(redoSet){
        redo := True
      }
      when(redoSet || delete){
        waitOn.cacheRefill    := 0
        waitOn.cacheRefillAny := False
        waitOn.mmuRefillAny   := False
      }
    }

    case class SqRegType(id : Int) extends Area{
      val allocation = False
      val valid = RegInit(False)
      val redo = RegInit(False)
      val redoSet = False
      val delete = False

      val commited = RegInit(False) //Warning, commited is meaning full even when valid == False !
      val commitedNext = CombInit(commited)
      commited := commitedNext

      val waitOn = new Area {
        val mmuRefillAny     = RegInit(False)
        val mmuRefillAnySet  = False

        mmuRefillAny   := mmuRefillAny | mmuRefillAnySet
        redoSet.setWhen(mmuRefillAny & translationWake)
      }

      when(delete){
        valid := False
      }
      when(allocation){
        valid := True
        commited := False
      }
      when(redoSet){
        redo := True
      }
      when(redoSet || delete){
        waitOn.mmuRefillAny   := False
      }
    }

    val lq = new Area{
      val regs = List.tabulate(lqSize)(LqRegType)


      val mem = new Area{
        def create[T <: Data](t : HardType[T]) = Mem.fill(lqSize)(t)
        val addressPre  = create(UInt(VIRTUAL_EXT_WIDTH bits))
        val addressPost = create(UInt(PHYSICAL_WIDTH bits))
        val size        = create(SIZE)
        val physRd      = create(decoder.PHYS_RD)
        val regfileRd   = create(decoder.REGFILE_RD)
        val robId       = create(ROB.ID)
        val robIdMsb    = create(ROB.MSB)
        val pc          = create(PC)
        val sqAlloc     = create(UInt(log2Up(sqSize) bits))
        val io          = create(Bool())
        val writeRd     = create(Bool())
        val lr          = create(Bool())
        val unsigned    = create(Bool())
        val needTranslation = create(Bool())
      }

      val ptr = new Area{
        val priority = Reg(Bits(lqSize-1 bits)) init(0) //TODO
        val alloc, free = Reg(UInt(log2Up(lqSize) + 1 bits)) init (0)
        val allocReal = U(alloc.dropHigh(1))
        val freeReal = U(free.dropHigh(1))
      }

      val tracker = new Area{
        val freeNext = UInt(log2Up(lqSize+1) bits)
        val free = RegNext(freeNext) init (lqSize)
        val freeReduced = RegNext(freeNext.sat(widthOf(freeNext)-log2Up(DECODE_COUNT.get+1)))
        val add = UInt(log2Up(COMMIT_COUNT.get+1) bits)
        val sub = UInt(log2Up(DECODE_COUNT.get+1) bits)
        freeNext := free + add - sub

        val clear = False
        when(clear){
          freeNext := lqSize
        }
      }

      val onCommit = new Area{
        val event = commit.onCommit()
        val lqAlloc = rob.readAsync(LQ_ALLOC, Global.COMMIT_COUNT, event.robId)
        val lqCommits = (0 until Global.COMMIT_COUNT).map(slotId => event.mask(slotId) && lqAlloc(slotId))
        val lqCommitCount = CountOne(lqCommits)
        var free = CombInit(ptr.free)
        var priority = CombInit(ptr.priority)
        for(inc <- lqCommits){
          for(reg <- regs) when(free.resize(log2Up(lqSize)) === reg.id && inc){
            reg.delete := True
          }
          when(inc) {
            priority \= (priority === 0) ? B(widthOf(priority) bits, default -> true).resized | (priority |<< 1)
          }
          free \= free + U(inc)
        }
        ptr.priority := priority
        ptr.free := ptr.free + lqCommitCount
        tracker.add := lqCommitCount
      }
    }


    val sq = new Area{
      val regs = List.tabulate(sqSize)(SqRegType)

      val mem = new Area{
        def create[T <: Data](t : HardType[T]) = Mem.fill(sqSize)(t)
        val robId       = create(ROB.ID)
        val robIdMsb    = create(ROB.MSB)
        val addressPre  = create(UInt(VIRTUAL_EXT_WIDTH bits))
        val addressPost = create(UInt(PHYSICAL_WIDTH bits))
        val size        = create(SIZE)
        val io          = create(Bool())
        val amo         = create(Bool())
        val sc          = create(Bool())
        val data        = create(Bits(wordWidth bits))
        val needTranslation = create(Bool())

        //Only one AMO/SC can be schedule at once, so we can store things in simple registers
        val swap = Reg(Bool())
        val op  = Reg(Bits(3 bits))
        val physRd = Reg(decoder.PHYS_RD)
        val regfileRd = Reg(decoder.REGFILE_RD)
        val writeRd = Reg(Bool())

      }

      val ptr = new Area{
        val priority = Reg(Bits(sqSize-1 bits)) init(0) //TODO
        val alloc, commit, writeBack, free = Reg(UInt(log2Up(sqSize) + 1 bits)) init(0)
        val allocReal = U(alloc.dropHigh(1))
        val freeReal = U(free.dropHigh(1))
        val writeBackReal = U(writeBack.dropHigh(1))
        val commitReal = U(commit.dropHigh(1))
        val commitNext = cloneOf(commit)
        commit := commitNext
//        def isFull(ptr : UInt) = (ptr ^ free) === sqSize
//        def isFree(ptr : UInt) = (free - ptr) < sqSize

        val onFree = Flow(UInt(log2Up(sqSize) bits))
        val onFreeLast = onFree.stage()

        setup.postCommitBusy setWhen(commit =/= free)
      }

      val tracker = new Area{
        val freeNext = UInt(log2Up(sqSize+1) bits)
        val free = RegNext(freeNext) init (sqSize)
        val freeReduced = RegNext(freeNext.sat(widthOf(freeNext)-log2Up(DECODE_COUNT.get+1)))
        val add = UInt(1 bits)
        val sub = UInt(log2Up(DECODE_COUNT.get+1) bits)
        freeNext := sqSize + ptr.free - ptr.alloc - sub + add
      }

    }


    val allocation = new Area{
      val allocStage = frontend.pipeline.dispatch
      for(slotId <- 0 until Frontend.DISPATCH_COUNT){
        allocStage(LSU_ID, slotId).assignDontCare()
      }

      import allocStage._

      val loads = new Area {
        val requests = (0 until Frontend.DISPATCH_COUNT).map(id => (DISPATCH_MASK, id) && (LQ_ALLOC, id))
        val requestsCount = CountOne(requests)
        val full = lq.tracker.freeReduced < requestsCount
        var alloc = CombInit(lq.ptr.alloc)
      }
      val stores = new Area {
        val requests = (0 until Frontend.DISPATCH_COUNT).map(id => (DISPATCH_MASK, id) && (SQ_ALLOC, id))
        val requestsCount = CountOne(requests)
        val full = sq.tracker.freeReduced < requestsCount
        var alloc = CombInit(sq.ptr.alloc)
      }

      haltIt(isValid && (loads.full || stores.full) )

      for(slotId <- 0 until Frontend.DISPATCH_COUNT){
        implicit val _ = StageableOffset(slotId)
        LQ_ID := loads.alloc.resized
        SQ_ID := stores.alloc.resized
        when(loads.requests(slotId)){
          LSU_ID := loads.alloc
          when(isFireing) {
            lq.regs.onSel(LQ_ID){ e =>
              e.allocation := True
            }
            lq.mem.sqAlloc.write(
              address = LQ_ID,
              data    = allocStage(SQ_ID, slotId)
            )
          }
          loads.alloc \= loads.alloc + 1
        }
        when(stores.requests(slotId)){
          LSU_ID := stores.alloc
          when(isFireing) {
            def wr[T <: Data](mem: Mem[T], data : T) = mem.write(SQ_ID, data)

          }
          stores.alloc \= stores.alloc + 1
        }
      }
      lq.tracker.sub := 0
      sq.tracker.sub := 0
      when(isFireing){
        lq.ptr.alloc := loads.alloc
        sq.ptr.alloc := stores.alloc
        lq.tracker.sub := loads.requestsCount
        sq.tracker.sub := stores.requestsCount
      }

      def writeLine[T <: Data](key : Stageable[T]) : Unit = {
        rob.write(
          key = key,
          size = DISPATCH_COUNT,
          value = allocStage(0 until DISPATCH_COUNT)(key),
          robId = allocStage(ROB.ID),
          enable = allocStage.isFireing
        )
      }
      writeLine(LSU_ID)
      writeLine(SQ_ALLOC)
      writeLine(LQ_ALLOC)
    }

    val aguPush = for(s <- aguPorts) yield new Area{
      val spec = s
      import spec._
      val pushLq = port.valid &&  port.load
      val pushSq = port.valid && !port.load

      def writeLq[T <: Data](mem : Mem[T], value : T) = mem.write(port.aguId.resized, value, pushLq)
      def writeSq[T <: Data](mem : Mem[T], value : T) = mem.write(port.aguId.resized, value, pushSq)

      writeLq(lq.mem.addressPre       , port.address)
      writeLq(lq.mem.physRd           , port.physicalRd)
      writeLq(lq.mem.regfileRd        , port.regfileRd)
      writeLq(lq.mem.robId            , port.robId)
      writeLq(lq.mem.robIdMsb         , port.robIdMsb)
      writeLq(lq.mem.pc               , port.pc)
      writeLq(lq.mem.writeRd          , port.writeRd)
      writeLq(lq.mem.lr               , port.lr)
      writeLq(lq.mem.size             , port.size)
      writeLq(lq.mem.unsigned         , port.unsigned)
      writeLq(lq.mem.needTranslation  , True)

      writeSq(sq.mem.addressPre, port.address)
      writeSq(sq.mem.robId     , port.robId)
      writeSq(sq.mem.robIdMsb  , port.robIdMsb)
      writeSq(sq.mem.amo, port.amo)
      writeSq(sq.mem.sc, port.sc)
      writeSq(sq.mem.size, port.size)
      writeSq(sq.mem.needTranslation, True)
      writeSq(sq.mem.data, port.data)
      when(pushSq && (port.sc || port.amo)){
        sq.mem.swap      := port.swap
        sq.mem.op        := port.op
        sq.mem.physRd    := port.physicalRd
        sq.mem.regfileRd := port.regfileRd
        sq.mem.writeRd   := port.writeRd
      }
    }

    def load = "load"
    def store = "store"
    val sharedPip = new Pipeline{
      val stages = Array.fill(sharedCtrlAt+1)(newStage())
      connect(stages)(List(M2S()))
      stages.last.flushIt(rescheduling.valid, root = false)

      val translationPort = translation.newTranslationPort(
        stages = this.stages,
        preAddress = ADDRESS_PRE_TRANSLATION,
        allowRefill = NEED_TRANSLATION,
        usage = LOAD_STORE,
        portSpec = sharedTranslationParameter,
        storageSpec = setup.translationStorage
      )
      val tpk = translationPort.keys
      translationWake := translationPort.wake

      val arbitration = new Area{
        val stage = stages(0)
        import stage._

        val lqRedo = B(lq.regs.map(reg => reg.redo))
        val sqRedo = B(sq.regs.map(reg => reg.redo))

        LQ_SEL_OH := OHMasking.roundRobinMasked(lqRedo, lq.ptr.priority)
        SQ_SEL_OH := OHMasking.roundRobinMasked(sqRedo, sq.ptr.priority)
        LQ_HIT := lqRedo.orR
        SQ_HIT := sqRedo.orR
        LQ_SEL_ARBI := OHToUInt(LQ_SEL_OH)
        SQ_SEL_ARBI := OHToUInt(SQ_SEL_OH)

        isValid := LQ_HIT || SQ_HIT

        LQ_ROB_FULL := lq.mem.robIdMsb.readAsync(LQ_SEL_ARBI) @@ lq.mem.robId.readAsync(LQ_SEL_ARBI)
        SQ_ROB_FULL := sq.mem.robIdMsb.readAsync(SQ_SEL_ARBI) @@ sq.mem.robId.readAsync(SQ_SEL_ARBI)

        val cmp = (LQ_ROB_FULL - SQ_ROB_FULL).msb
        LQ_OLDER_THAN_SQ := !SQ_HIT || cmp
        when(isReady) {
          when(LQ_HIT && (!SQ_HIT || cmp)) {
            lq.regs.onMask(LQ_SEL_OH)(_.redo := False)
          }
          when(SQ_HIT && (!LQ_HIT || !cmp)) {
            sq.regs.onMask(SQ_SEL_OH)(_.redo := False)
          }
        }
      }

      val feed = new Area {
        val stage = stages(sharedAguAt)
        import stage._

        HIT_SPECULATION := False //TODO
        BYPASS_TOO_EARLY := False //TODO

        val agu = aguPorts.head.port
        val takeAgu = (LQ_OLDER_THAN_SQ ? (agu.robIdFull - LQ_ROB_FULL).msb | (agu.robIdFull - SQ_ROB_FULL).msb)
        takeAgu.setWhen(!LQ_HIT && !SQ_HIT)
        takeAgu.clearWhen(!agu.valid)

        ROB.ID := Mux[UInt](LQ_OLDER_THAN_SQ, LQ_ROB_FULL, SQ_ROB_FULL).resized
        decoder.PHYS_RD    := lq.mem.physRd.readAsync(LQ_SEL_ARBI)
        decoder.REGFILE_RD := lq.mem.regfileRd.readAsync(LQ_SEL_ARBI)

        def readQueues[T <: Data](key : Stageable[T], lqMem : Mem[T], sqMem : Mem[T]) : Unit = {
          stage(key, load)  := lqMem.readAsync(LQ_SEL_ARBI)
          stage(key, store) := sqMem.readAsync(SQ_SEL_ARBI)
          stage(key) := Mux[T](LQ_OLDER_THAN_SQ, (key, load), (key, store))
        }
        def readLq[T <: Data](key : Stageable[T], lqMem : Mem[T]) : Unit = {
          stage(key) := lqMem.readAsync(LQ_SEL_ARBI)
        }
        readQueues(ADDRESS_PRE_TRANSLATION , lq.mem.addressPre , sq.mem.addressPre)
        readQueues(ADDRESS_POST_TRANSLATION, lq.mem.addressPost, sq.mem.addressPost)
        readQueues(SIZE, lq.mem.size, sq.mem.size)
        readQueues(NEED_TRANSLATION, lq.mem.needTranslation, sq.mem.needTranslation)
        readLq(WRITE_RD, lq.mem.writeRd)
        readLq(UNSIGNED, lq.mem.unsigned)
        readLq(LR, lq.mem.lr)

        IS_LOAD := LQ_OLDER_THAN_SQ

        LQ_SEL := LQ_SEL_ARBI
        SQ_SEL := SQ_SEL_ARBI
        when(takeAgu){
          spawnIt()
          forkIt()

          NEED_TRANSLATION := True
          IS_LOAD := agu.load
          ROB.ID := agu.robIdFull.resized
          ADDRESS_PRE_TRANSLATION := agu.address
          SIZE := agu.size
          WRITE_RD := agu.writeRd
          UNSIGNED := agu.unsigned
          LR := agu.lr
          decoder.PHYS_RD := agu.physicalRd
          decoder.REGFILE_RD := agu.regfileRd
          LQ_SEL := agu.aguId.resized
          SQ_SEL := agu.aguId.resized
        } otherwise {
          when(agu.valid){
            when(agu.load) {
              lq.regs.onSel(agu.aguId.resized)(_.redoSet := True)
            } otherwise {
              sq.regs.onSel(agu.aguId.resized)(_.redoSet := True)
            }
          }
        }
      }

      val feedCache = new Area{
        val stage = stages(sharedFeedAt)
        import stage._

        val cmd = setup.cacheLoad.cmd
        cmd.valid            := (stage.isValid +: stage.internals.request.spawns).orR
        cmd.virtual          := ADDRESS_PRE_TRANSLATION
        cmd.size             := SIZE
        cmd.redoOnDataHazard := False

        haltIt(!cmd.ready)
      }

      val feedTranslation = new Area{
        val stage = stages(sharedFeedAt + setup.cacheLoad.translatedAt)
        import stage._

        setup.cacheLoad.translated.physical := tpk.TRANSLATED
        when(!NEED_TRANSLATION){
          setup.cacheLoad.translated.physical := ADDRESS_POST_TRANSLATION
        }
        setup.cacheLoad.translated.abord := stage(tpk.IO) || tpk.PAGE_FAULT || tpk.ACCESS_FAULT || !tpk.ALLOW_READ || tpk.REDO
      }

      val cancels = for(stageId <- 0 to cache.loadRspLatency){
        setup.cacheLoad.cancels(stageId) := rescheduling.valid
      }

      val cacheRsp = new Area {
        val stage = stages(sharedFeedAt + cache.loadRspLatency)
        import stage._

        val specialOverride = False //Allow the peripheral ctrl to cannibalise this data path logic <3

        def rsp = stage(LOAD_CACHE_RSP)
        rsp := setup.cacheLoad.rsp

        val rspSize    = CombInit(stage(SIZE))
        val rspAddress = CombInit(stage(ADDRESS_PRE_TRANSLATION))
        val rspUnsigned = CombInit(stage(UNSIGNED))
        val rspRaw     = rsp.data //CombInit(rsp.data.subdivideIn(wordWidth bits).read(ADDRESS_PRE_TRANSLATION(memToCpuRange)))
        val rspSplits  = rspRaw.subdivideIn(8 bits)
        val rspShifted = Bits(wordWidth bits)

        //Generate minimal mux to move from a wide aligned memory read to the register file shifter representation
        for(i <- 0 until wordBytes){
          val srcSize = 1 << (log2Up(wordBytes) - log2Up(i+1))
          val srcZipped = rspSplits.zipWithIndex.filter{case (v, b) => b % (wordBytes/srcSize) == i}
          val src = srcZipped.map(_._1)
          val range = cpuWordToRfWordRange.high downto cpuWordToRfWordRange.high+1-log2Up(srcSize)
          val sel = rspAddress(range)
          rspShifted(i*8, 8 bits) := src.read(sel)
        }

        //TODO bypass
//        when(!specialOverride && OLDER_STORE_HIT){
//          rspShifted := checkSqArbi.bypass.data
//        }

        val sizeMax = log2Up(LSLEN/8)
        val rspFormated = rspSize.muxListDc((0 to sizeMax).map{i =>
          val off = (1 << i) * 8
          i -> B((LSLEN - 1 downto off) -> (rspShifted(off-1) && !rspUnsigned), (off-1 downto 0) -> rspShifted(off-1 downto 0))
        })

        val doIt = loadWriteRfOnPrivilegeFail match {
          case false => isValid && IS_LOAD && WRITE_RD && tpk.ALLOW_READ && !tpk.PAGE_FAULT && !tpk.ACCESS_FAULT
          case true  => isValid && IS_LOAD && WRITE_RD
        }
        for((spec, regfile) <- setup.regfilePorts) {
          regfile.write.valid   := doIt && decoder.REGFILE_RD === decoder.REGFILE_RD.rfToId(spec)
          regfile.write.address := decoder.PHYS_RD
          regfile.write.data    := rspFormated.resized
          regfile.write.robId   := ROB.ID

          if(RVD && spec == FloatRegFile) when(stage(SIZE) === 2){
            regfile.write.data(63 downto 32).setAll()
          }
        }

        LOAD_WRITE_FAILURE := IS_LOAD && specialOverride && !tpk.IO
      }

      //Bypass load rsp refillSlotxxx
      for(stageId <- sharedFeedAt + cache.loadRspLatency to sharedCtrlAt){
        val stage = stages(stageId)
        def o = stage.overloaded(LOAD_CACHE_RSP)
        def i = stage(LOAD_CACHE_RSP)
        o := i
        o.refillSlotAny.removeAssignments() := i.refillSlotAny && !cache.refillCompletions.orR
        o.refillSlot.removeAssignments()    := i.refillSlot     & ~cache.refillCompletions
      }

      val ctrl = new Area{
        val stage = stages(sharedCtrlAt)
        import stage._

        def rsp = stage.resulting(LOAD_CACHE_RSP)

        setup.sharedCompletion.valid := False
        setup.sharedCompletion.id := ROB.ID

        val wakeRob = Flow(WakeRob())
        wakeRob.valid := False
        wakeRob.robId := ROB.ID

        val wakeRf = Flow(WakeRegFile(decoder.REGFILE_RD, decoder.PHYS_RD, needBypass = false, withRfBypass = true, rfLatency = -1))
        wakeRf.valid := False
        wakeRf.physical := decoder.PHYS_RD
        wakeRf.regfile := decoder.REGFILE_RD

        setup.sharedTrap.valid      := False
        setup.sharedTrap.robId      := ROB.ID
        setup.sharedTrap.tval       := B(ADDRESS_PRE_TRANSLATION).resized //TODO addr sign extends ?
        setup.sharedTrap.skipCommit := True
        setup.sharedTrap.cause      := EnvCallPlugin.CAUSE_REDO
        setup.sharedTrap.reason     := ScheduleReason.LOAD_HIT_MISS_PREDICTED


        MISS_ALIGNED := (1 to log2Up(wordWidth/8)).map(i => SIZE === i && ADDRESS_PRE_TRANSLATION(i-1 downto 0) =/= 0).orR
        PAGE_FAULT   := (IS_LOAD ? !tpk.ALLOW_READ | !tpk.ALLOW_WRITE) || tpk.PAGE_FAULT
//        ACCESS_FAULT := rsp.fault || tpk.ACCESS_FAULT
        REGULAR      := !tpk.IO && !AMO && !SC

        def onLq(body : LqRegType => Unit) = lq.regs.onMask(lqMask)(body)
        def onSq(body : SqRegType => Unit) = sq.regs.onMask(sqMask)(body)

        val lqMask = UIntToOh(LQ_SEL)
        val sqMask = UIntToOh(SQ_SEL)
        
        val hitSpeculationTrap = True
        val redoTrigger = False
        when(redoTrigger){
          when(IS_LOAD) {
            onLq(_.redoSet := True)
          } otherwise {
            onSq(_.redoSet := True)
          }
        }

        val doCompletion = False
        val refillMask = rsp.refillSlot.orMask(rsp.refillSlotAny)


        when(isFireing) {
          setup.sharedTrap.valid := HIT_SPECULATION && hitSpeculationTrap
          when(NEED_TRANSLATION) {
            when(IS_LOAD) {
              lq.mem.addressPost.write(LQ_SEL, tpk.TRANSLATED)
              lq.mem.io.write(LQ_SEL, tpk.IO)
              lq.mem.needTranslation.write(LQ_SEL, tpk.REDO)
            } otherwise {
              sq.mem.addressPost.write(SQ_SEL, tpk.TRANSLATED)
              sq.mem.io.write(SQ_SEL, tpk.IO)
              sq.mem.needTranslation.write(SQ_SEL, tpk.REDO)
            }
          }

          when(MISS_ALIGNED){
            setup.sharedTrap.valid := True
            setup.sharedTrap.reason := ScheduleReason.TRAP
            setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_MISALIGNED
            setup.sharedTrap.cause(2) := IS_LOAD
          }.elsewhen(NEED_TRANSLATION && tpk.REDO){
            onLq(_.waitOn.mmuRefillAnySet := True)
            redoTrigger := translationWake
          }.elsewhen(NEED_TRANSLATION && (tpk.ACCESS_FAULT || PAGE_FAULT)){
            setup.sharedTrap.valid := True
            setup.sharedTrap.reason := ScheduleReason.TRAP
            setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
            setup.sharedTrap.cause(2) := IS_LOAD
          }.elsewhen(BYPASS_TOO_EARLY){
            report("Implement me BYPASS_TOO_EARLY")
//            onRegs{r =>
//              r.waitOn.sq setWhen(!stage.resulting(OLDER_STORE_COMPLETED))
//              r.waitOn.sqId := OLDER_STORE_ID
//              r.waitOn.sqCompletion := !OLDER_STORE_MAY_BYPASS
//            }
          }.elsewhen(rsp.redo) {
//            hadSpeculativeHitTrap setWhen(HIT_SPECULATION)
            onLq(_.waitOn.cacheRefillSet := refillMask)
            redoTrigger := !refillMask.orR
          }.elsewhen(LOAD_WRITE_FAILURE){
            redoTrigger := True
          }.otherwise {
            onLq(_.waitOn.commitSet := True)
            when(rsp.fault) {
              setup.sharedTrap.valid := True
              setup.sharedTrap.reason := ScheduleReason.TRAP
              setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
              setup.sharedTrap.cause(2) := IS_LOAD
            }.otherwise {
              doCompletion := True
            }
          }
        }

        //Critical path extracted to help synthesis
        KeepAttribute(doCompletion)
        when(doCompletion){
//          speculateHitTrapRecovered := LQ_SEL === 0 //Assume LQ_ID restart at 0 after a trap
//          hitSpeculationTrap := False
          when(IS_LOAD) {
            when(!LR && !tpk.IO) {
              setup.sharedCompletion.valid := True
            }
            //          when(LR){
            //            lq.reservation.valid   := True
            //            lq.reservation.address := tpk.TRANSLATED
            //          }
            when(WRITE_RD && !HIT_SPECULATION) {
              wakeRob.valid := True
              wakeRf.valid := True
            }
          }
        }

//        val hitPrediction = new Area{
//          def onSuccess = S(-1)
//          def onFailure = S(hitPredictionErrorPenality)
//          val next = HIT_SPECULATION_COUNTER +^ (success ? onSuccess | onFailure)
//
//          val writePort = lq.hitPrediction.writePort
//          writePort.valid    := isFireing && LOAD_FRESH
//          writePort.address  := lq.hitPrediction.index(LOAD_FRESH_PC)
//          writePort.data.counter := next.sat(widthOf(next) - hitPredictionCounterWidth bits)
//          when(!tpk.REDO && !tpk.PAGE_FAULT && !tpk.ACCESS_FAULT && tpk.IO && tpk.ALLOW_READ){
//            writePort.data.counter := writePort.data.counter.maxValue
//          }
//        }
      }
    }
    hardFork{
      sharedPip.translationPort.pipelineLock.await()
      sharedPip.build()
    }


    when(rescheduling.valid){
      lq.regs.foreach(_.delete := True)
      lq.ptr.free := 0
      lq.ptr.alloc := 0
      lq.ptr.priority := 0
      lq.tracker.clear := True
      for(reg <- sq.regs){
        reg.valid clearWhen(!reg.commitedNext) //TODO
//        reg.allLqIsYounger := True
      }
      sq.ptr.alloc := sq.ptr.commitNext
//      special.enabled := False //TODO
    }

    //TODO
//    store.writeback.feed.holdPrefetch setWhen(flush.busy)
//    store.writeback.feed.holdPrefetch setWhen(special.enabled)

    //TODO
    sq.tracker.add := 0
    sq.ptr.commitNext := 0
    setup.cache.lockPort.valid := False
    setup.cache.lockPort.address := 0
    setup.cacheLoad.cmd.unlocked := True
    peripheralBus.cmd.setIdle()
    assert(setup.flushPort.cmd.valid === False,"Please implement lsu flush port")

    val whitebox = new AreaRoot{
      val stage = frontend.pipeline.dispatch
      Verilator.public(stage(ROB.ID))

      val sqAlloc = for(slotId <- 0 until DECODE_COUNT) yield new Area{
        val valid = Verilator.public(stage.isFireing && stage(DISPATCH_MASK, slotId) && stage(SQ_ALLOC, slotId))
        val id = Verilator.public(CombInit(stage(SQ_ID, slotId)))
      }

      val sqFree = Verilator.public(sq.ptr.onFree.combStage())
    }


    rob.release()
    decoder.release()
    frontend.release()
    fetch.release()
    translation.release()
  }
}
