package naxriscv.lsu2

import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.execute.EnvCallPlugin
import naxriscv.fetch.FetchPlugin
import naxriscv.frontend.{DispatchPlugin, FrontendPlugin}
import naxriscv.interfaces.AddressTranslationPortUsage.LOAD_STORE
import naxriscv.{Fetch, Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.lsu.{DataCachePlugin, LsuFlushPayload, LsuFlusher, LsuPeripheralBus, LsuUtils, PrefetchPredictor}
import naxriscv.misc.RobPlugin
import naxriscv.riscv.{AtomicAlu, CSR, FloatRegFile, IntRegFile, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{AddressToMask, DocPlugin, Plugin, WithRfWriteSharedSpec}
import spinal.core.fiber.hardFork
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.pipeline.Connection._
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
                 var sharedAguAt  : Int = 0,
                 var sharedFeedAt : Int = 0, //Stage at which the d$ cmd is sent
                 var sharedCheckSqAt : Int = 1,
                 var sharedCtrlAt : Int = 3,
                 var loadWriteRfOnPrivilegeFail : Boolean = true,
                 var lqToCachePipelined : Boolean = true,
                 var windowFilterWays : Int = 2,
                 var windowFilterEntries : Int = 8, //TODO
                 var windowFilterTagWidth : Int = 10, //TODO
                 var windowFilterIdWidth : Int = 9,
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
  def windowFilterAddressWidth = log2Up(windowFilterEntries)


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

    val HIT_SPECULATION, HIT_SPECULATION_WRITE_RD = Stageable(Bool())
//    val HIT_SPECULATION_COUNTER = Stageable(SInt(hitPredictionCounterWidth bits))

    val IS_LOAD = Stageable(Bool())

    val SQ_ALLOC_ID = Stageable(UInt(windowFilterIdWidth bits))

    val SF_BYPASS = Stageable(Bool())
    val SF_DELTA  = Stageable(SQ_ID)
    val SF_SQ_ALLOC = Stageable(UInt(log2Up(sqSize)+1 bits))

    val OLDER_STORE_MAY_BYPASS  = Stageable(Bool())
    val OLDER_STORE_BYPASS_SUCCESS  = Stageable(Bool())
    val OLDER_STORE_HIT  = Stageable(Bool())
    val OLDER_STORE_ID = Stageable(SQ_ID)
    val OLDER_STORE_COMPLETED = Stageable(Bool()) //Used to avoid LQ waiting on SQ which just fired
    val OLDER_STORE_OH = Stageable(Bits(sqSize bits))

    val LQCHECK_START_ID = Stageable(UInt(log2Up(lqSize) + 1 bits))
    //      val LQCHECK_HITS_EARLY = Stageable(Bits(lqSize bits))
    val LQCHECK_HITS = Stageable(Bits(lqSize bits))
    val LQCHECK_NO_YOUNGER = Stageable(Bool())

    val SQCHECK_END_ID = Stageable(UInt(log2Up(sqSize) + 1 bits))
    val SQCHECK_HITS = Stageable(Bits(sqSize bits))
    val SQCHECK_NO_OLDER = Stageable(Bool())
    val SQ_YOUNGER_MASK = Stageable(UInt(sqSize bits))
    val SQ_YOUNGER_MASK_EMPTY = Stageable(Bool())


    val YOUNGER_LOAD_PC         = Stageable(PC)
    val YOUNGER_LOAD_ROB        = Stageable(ROB.ID)
    val YOUNGER_LOAD_RESCHEDULE = Stageable(Bool())
  }
  import keys._

  override def wakeRobs    = List(logic.get.sharedPip.ctrl.wakeRob, logic.get.special.wakeRob)
  override def wakeRegFile = List(logic.get.sharedPip.ctrl.wakeRf , logic.get.special.wakeRf)
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
    val sharedCompletion = rob.newRobCompletion()
    val sharedTrap = commit.newSchedulePort(canTrap = true, canJump = true)
    val specialTrap = commit.newSchedulePort(canTrap = true, canJump = false)
    val specialCompletion = rob.newRobCompletion()
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
    val sqWritebackEvent = Flow(SQ_ID)
    val sqDataEvent    = Flow(SQ_ID)
    case class LqRegType(id : Int) extends Area{
      val allocation = False
      val valid      = RegInit(False)
      val redo       = RegInit(False)
      val redoSet    = False
      val delete     = False

      val sqChecked = Reg(Bool())

      val address = new Area {
        val pageOffset = Reg(UInt(pageOffsetWidth bits))
        val mask = Reg(DATA_MASK)
      }

      val waitOn = new Area {
        val cacheRefill    = Reg(Bits(cache.refillCount bits)) init(0)
        val cacheRefillAny = RegInit(False)
        val mmuRefillAny   = RegInit(False)
        val sqWriteback    = RegInit(False)
        val sqBypass       = RegInit(False)
        val sqId           = Reg(SQ_ID)

        val cacheRefillSet  = cacheRefill.getZero
        val mmuRefillAnySet = False
        val sqWritebackSet  = False
        val sqBypassSet     = False

        cacheRefill  := cacheRefill  | cacheRefillSet
        mmuRefillAny := mmuRefillAny | mmuRefillAnySet
        sqWriteback  := sqWriteback  | sqWritebackSet
        sqBypass     := sqBypass     | sqBypassSet

        redoSet.setWhen(
          (cacheRefill  &  cache.refillCompletions).orR ||
           mmuRefillAny && translationWake ||
           sqWriteback  && sqWritebackEvent.valid && sqWritebackEvent.payload === sqId ||
           sqBypass     && sqDataEvent.valid    && sqDataEvent.payload    === sqId
        )
      }



      when(delete){
        valid := False
      }
      when(allocation){
        valid := True
        sqChecked := False
      }
      when(redoSet){
        redo := True
      }
      when(redoSet || delete){
        waitOn.cacheRefill    := 0
        waitOn.cacheRefillAny := False
        waitOn.mmuRefillAny   := False
        waitOn.sqBypass       := False
        waitOn.sqWriteback    := False
      }
    }

    case class SqRegType(id : Int) extends Area{
      val allocation = False
      val valid = RegInit(False)
      val redo = RegInit(False)
      val redoSet = False
      val delete = False
      val dataValid = Reg(Bool())
      def virtualValid = dataValid
      val address = new Area {
        val pageOffset = Reg(UInt(pageOffsetWidth bits))
        val mask = Reg(DATA_MASK)
      }

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
        dataValid := False
      }
      when(redoSet){
        redo := True
      }
      when(redoSet || delete){
        waitOn.mmuRefillAny   := False
      }
    }

    val storeForwarding = new Area{
      val loadBypassPredEntries = 128
      val loadBypassPredTagWidth = 10
      def addressOf(pc: UInt): UInt = pc(Fetch.SLICE_RANGE_LOW, log2Up(loadBypassPredEntries) bits)
      def tagOf(pc: UInt): UInt = pc(Fetch.SLICE_RANGE_LOW + log2Up(loadBypassPredEntries), loadBypassPredTagWidth bits)
      case class Entry() extends Bundle{
        val tag = UInt(loadBypassPredTagWidth bits)
        val delta = UInt(log2Up(sqSize) bits)
        val allowBypass = Bool()
        val valid = Bool()
      }
      val mem = Mem.fill(loadBypassPredEntries)(Entry())

      val write = mem.writePort()
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
        val sqAlloc     = create(UInt(log2Up(sqSize)+1 bits))
        val io          = create(Bool())
        val writeRd     = create(Bool())
        val lr          = create(Bool())
        val unsigned    = create(Bool())
        val doSpecial   = create(Bool())
        val doWindowFilter  = create(Bool())
        val data            = create(Bits(wordWidth bits))
        val needTranslation = create(Bool())
        val sqAllocId = create(SQ_ALLOC_ID)
        val sqAllocIdOnRead = create(SQ_ALLOC_ID)
        val sqOnRead = create(UInt(log2Up(sqSize) + 1 bits))
        val sqMask   = create(Bits(sqSize bits))
        val sf = new Area{
          val writeback = create(Bool())
          val bypass = create(Bool())
          val delta  = create(SF_DELTA)
        }
//        val bypass = new Area{
//          val done = create(Bool())
//          val sqId  = create(SQ_ID)
//        }
      }

      val ptr = new Area{
        val priority = Reg(Bits(lqSize-1 bits)) init(0) //TODO
        val priorityLast = RegNext(priority)
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

      val reservation = new Area{
        val valid = Reg(Bool()) init(False)
        val address = Reg(UInt(PHYSICAL_WIDTH bits))
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
        val doSpecial       = create(Bool())
        val doNotBypass = create(Bool())
        val allocId = create(SQ_ALLOC_ID)
        val lqAlloc = Mem.fill(sqSize)(UInt(log2Up(lqSize) + 1 bits))

        //Only one AMO/SC can be schedule at once, so we can store things in simple registers
        val swap = Reg(Bool())
        val op  = Reg(Bits(3 bits))
        val physRd = Reg(decoder.PHYS_RD)
        val regfileRd = Reg(decoder.REGFILE_RD)
        val writeRd = Reg(Bool())

      }

      val ptr = new Area{
        val priority = Reg(Bits(sqSize-1 bits)) init(0) //TODO
        val priorityLast = RegNext(priority)
        val alloc, commit, writeBack, free = Reg(UInt(log2Up(sqSize) + 1 bits)) init(0)
        val allocReal = U(alloc.dropHigh(1))
        val freeReal = U(free.dropHigh(1))
        val writeBackReal = U(writeBack.dropHigh(1))
        val commitReal = U(commit.dropHigh(1))
        val commitNext = cloneOf(commit)
        commit := commitNext

        val onFree = Flow(UInt(log2Up(sqSize) bits))
        val onFreeLast = onFree.stage()
        sqWritebackEvent << onFree
        setup.postCommitBusy setWhen(commit =/= free)

        val allocId = Reg(SQ_ALLOC_ID) init(0)
      }

      val tracker = new Area{
        val freeNext = UInt(log2Up(sqSize+1) bits)
        val free = RegNext(freeNext) init (sqSize)
        val freeReduced = RegNext(freeNext.sat(widthOf(freeNext)-log2Up(DECODE_COUNT.get+1)))
        val add = UInt(1 bits)
        val sub = UInt(log2Up(DECODE_COUNT.get+1) bits)
        freeNext := sqSize + ptr.free - ptr.alloc - sub + add
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
        var allocId = CombInit(sq.ptr.allocId)
      }

      haltIt(isValid && (loads.full || stores.full) )

      for(slotId <- 0 until Frontend.DISPATCH_COUNT){
        implicit val _ = StageableOffset(slotId)
        LQ_ID := loads.alloc.resized
        SQ_ID := stores.alloc.resized
        when(loads.requests(slotId)){
          LSU_ID := loads.alloc.resized
          when(isFireing) {
            lq.regs.onSel(LQ_ID){ _.allocation := True }
            lq.mem.sqAlloc.write(LQ_ID, stores.alloc)
            lq.mem.doWindowFilter.write(LQ_ID, False)
            lq.mem.doSpecial.write(LQ_ID, False)
            lq.mem.sqAllocId.write(LQ_ID, stores.allocId)
          }
          loads.alloc \= loads.alloc + 1
        }
        when(stores.requests(slotId)){
          LSU_ID := stores.alloc.resized
          when(isFireing) {
            sq.regs.onSel(SQ_ID){ _.allocation := True }
            sq.mem.doSpecial.write(SQ_ID, False)
            sq.mem.allocId.write(SQ_ID, stores.allocId)
            sq.mem.lqAlloc.write(SQ_ID, loads.alloc)
          }
          stores.alloc \= stores.alloc + 1
          stores.allocId \= stores.allocId + 1
        }
      }
      lq.tracker.sub := 0
      sq.tracker.sub := 0
      when(isFireing){
        lq.ptr.alloc := loads.alloc
        sq.ptr.alloc := stores.alloc
        sq.ptr.allocId := stores.allocId
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

      val dataMask = AddressToMask(port.address, port.size, wordBytes)

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

      when(pushLq) {
        lq.regs.onSel(port.aguId.resized) { r =>
          r.address.pageOffset := port.address(pageOffsetRange)
          r.address.mask       := dataMask
        }
      }

      val lqsqAlloc = lq.mem.sqAlloc.readAsync(port.aguId.resized)

      val sf = new Area{
        val read = storeForwarding.mem.readSyncPort
        read.cmd.valid := port.earlySample
        read.cmd.payload := storeForwarding.addressOf(port.earlyPc)

        val bypassHit  = RegNext(read.cmd.payload === storeForwarding.write.address && storeForwarding.write.valid)
        val bypassData = RegNext(storeForwarding.write.data)
        when(bypassHit){
          read.rsp := bypassData
        }

        val tagHit = read.rsp.tag === storeForwarding.tagOf(port.pc)

        val sqId = (lqsqAlloc - read.rsp.delta - 1)
        val sqWritebackDone = (sqId - sq.ptr.free).msb
        val sqDataDone      = sq.regs.map(_.dataValid).read(sqId.resized)

        val canBypass       = tagHit && read.rsp.valid && read.rsp.allowBypass
        val waitOnWriteback = tagHit && read.rsp.valid && !sqWritebackDone
        val waitOnData      = canBypass && !sqWritebackDone && !sqDataDone
//        val waitIt          = waitOnWriteback || waitOnData

        waitOnWriteback clearWhen(sqWritebackEvent.valid && sqWritebackEvent.payload === sqId)
        waitOnData      clearWhen(sqDataEvent.valid      && sqDataEvent.payload      === sqId)
        waitOnWriteback clearWhen(read.rsp.allowBypass && !waitOnData)

        writeLq(lq.mem.sf.writeback, tagHit && read.rsp.valid)
        writeLq(lq.mem.sf.bypass   , tagHit && read.rsp.valid && read.rsp.allowBypass)
        writeLq(lq.mem.sf.delta    , read.rsp.delta)

//        when(pushLq){
//          lq.regs.onSel(port.aguId.resized) { r =>
//            r.sqChecked := True
//          }
//        }
      }
//          lq.regs.onSel(port.aguId.resized){r =>
//            r.waitOn.sqWritebackSet setWhen(waitOnWriteback)
//            r.waitOn.sqBypassSet    setWhen(waitOnData)
//            r.waitOn.sqId := sqId.resized
//          }
//        }
//      }

      writeSq(sq.mem.addressPre, port.address)
      writeSq(sq.mem.robId     , port.robId)
      writeSq(sq.mem.robIdMsb  , port.robIdMsb)
      writeSq(sq.mem.amo, port.amo)
      writeSq(sq.mem.sc, port.sc)
      writeSq(sq.mem.size, port.size)
      writeSq(sq.mem.needTranslation, True)
      writeSq(sq.mem.data, port.data)
      writeSq(sq.mem.doNotBypass, port.amo || port.sc)
      when(pushSq && (port.sc || port.amo)){
        sq.mem.swap      := port.swap
        sq.mem.op        := port.op
        sq.mem.physRd    := port.physicalRd
        sq.mem.regfileRd := port.regfileRd
        sq.mem.writeRd   := port.writeRd
      }
      when(pushSq) {
        sq.regs.onSel(port.aguId.resized) { r =>
          r.dataValid := True
          r.address.pageOffset := port.address(pageOffsetRange)
          r.address.mask       := dataMask
        }
      }
      sqDataEvent.valid := pushSq
      sqDataEvent.payload := port.aguId.resized
    }



    def load = "load"
    def store = "store"

    val lqSqArbitration = new Pipeline {
      val s0 = new Stage {
        val lqRedo = B(lq.regs.map(reg => reg.redo))
        val sqRedo = B(sq.regs.map(reg => reg.redo))

        LQ_SEL_OH := OHMasking.roundRobinMasked(lqRedo, lq.ptr.priority)
        SQ_SEL_OH := OHMasking.roundRobinMasked(sqRedo, sq.ptr.priority)
        LQ_HIT := lqRedo.orR
        SQ_HIT := sqRedo.orR
        LQ_SEL := OHToUInt(LQ_SEL_OH)
        SQ_SEL := OHToUInt(SQ_SEL_OH)

        isValid := LQ_HIT || SQ_HIT

        LQ_ROB_FULL := lq.mem.robIdMsb.readAsync(LQ_SEL) @@ lq.mem.robId.readAsync(LQ_SEL)
        SQ_ROB_FULL := sq.mem.robIdMsb.readAsync(SQ_SEL) @@ sq.mem.robId.readAsync(SQ_SEL)

        when(isReady) {
          when(LQ_HIT) {
            lq.regs.onMask(LQ_SEL_OH)(_.redo := False)
          }
          when(SQ_HIT) {
            sq.regs.onMask(SQ_SEL_OH)(_.redo := False)
          }
        }
      }

      val s1 = new Stage(M2S()) {
        val cmp = (LQ_ROB_FULL - SQ_ROB_FULL).msb
        LQ_OLDER_THAN_SQ := !SQ_HIT || LQ_HIT && cmp
        flushIt(rescheduling.valid, root = false)
      }
    }




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



      val feed = new Area {
        val stage = stages(sharedAguAt)
        import stage._

        val lqSqFeed = lqSqArbitration.s1

        HIT_SPECULATION := False //TODO

        val lqSelArbi = lqSqFeed(LQ_SEL)
        val sqSelArbi = lqSqFeed(SQ_SEL)
        val TAKE_LQ = insert(lqSqFeed(LQ_OLDER_THAN_SQ))
        LQ_ROB_FULL := lqSqFeed(LQ_ROB_FULL)
        SQ_ROB_FULL := lqSqFeed(SQ_ROB_FULL)

        val agu = aguPorts.head.port
        val takeAgu = (TAKE_LQ ? (agu.robIdFull - LQ_ROB_FULL).msb | (agu.robIdFull - SQ_ROB_FULL).msb)
        takeAgu.setWhen(!lqSqFeed.isValid)
        takeAgu.clearWhen(!agu.valid)

        isValid := agu.valid || lqSqFeed.isValid
        lqSqFeed.haltWhen(takeAgu)

        val lqSqSerializer = new Area {
          val lqMask, sqMask = RegInit(True)
          TAKE_LQ.clearWhen(!lqMask)
          TAKE_LQ.setWhen(!sqMask)

          when(lqSqFeed(SQ_HIT) && sqMask && lqSqFeed(LQ_HIT) && lqMask) {
            lqSqFeed.haltIt()
            when(!takeAgu && isReady) {
              lqMask clearWhen ( TAKE_LQ)
              sqMask clearWhen (!TAKE_LQ)
            }
          }

          when(lqSqFeed.isRemoved || lqSqFeed.isReady || !lqSqFeed.isValid) {
            lqMask := True
            sqMask := True
          }
        }

        ROB.ID := Mux[UInt](TAKE_LQ || !lqSqSerializer.sqMask, LQ_ROB_FULL, SQ_ROB_FULL).resized
        decoder.PHYS_RD    := lq.mem.physRd.readAsync(lqSelArbi)
        decoder.REGFILE_RD := lq.mem.regfileRd.readAsync(lqSelArbi)

        def readQueues[T <: Data](key : Stageable[T], lqMem : Mem[T], sqMem : Mem[T]) : Unit = {
          stage(key, load)  := lqMem.readAsync(lqSelArbi)
          stage(key, store) := sqMem.readAsync(sqSelArbi)
          stage(key) := Mux[T](TAKE_LQ, (key, load), (key, store))
        }
        def readLq[T <: Data](key : Stageable[T], lqMem : Mem[T]) : Unit = {
          stage(key) := lqMem.readAsync(lqSelArbi)
        }
        def readSq[T <: Data](key : Stageable[T], sqMem : Mem[T]) : Unit = {
          stage(key) := sqMem.readAsync(sqSelArbi)
        }
        readQueues(ADDRESS_PRE_TRANSLATION , lq.mem.addressPre , sq.mem.addressPre)
        readQueues(ADDRESS_POST_TRANSLATION, lq.mem.addressPost, sq.mem.addressPost)
        readQueues(SIZE, lq.mem.size, sq.mem.size)
        readQueues(NEED_TRANSLATION, lq.mem.needTranslation, sq.mem.needTranslation)
        readLq(WRITE_RD, lq.mem.writeRd)
        readLq(UNSIGNED, lq.mem.unsigned)
        readLq(LR, lq.mem.lr)
        readLq(SF_BYPASS, lq.mem.sf.bypass)
        readLq(SF_DELTA, lq.mem.sf.delta)
        readLq(SF_SQ_ALLOC, lq.mem.sqAlloc)
        readSq(AMO, sq.mem.amo)
        readSq(SC, sq.mem.sc)

        IS_LOAD := TAKE_LQ

        LQ_SEL := lqSelArbi
        SQ_SEL := sqSelArbi

        when(takeAgu){
          forkIt()
          NEED_TRANSLATION := True
          IS_LOAD := agu.load
          ROB.ID := agu.robIdFull.resized
          ADDRESS_PRE_TRANSLATION := agu.address
          SIZE := agu.size
          WRITE_RD := agu.writeRd
          UNSIGNED := agu.unsigned
          LR := agu.lr
          SC := agu.sc
          AMO := agu.amo
          decoder.PHYS_RD := agu.physicalRd
          decoder.REGFILE_RD := agu.regfileRd
          LQ_SEL := agu.aguId.resized
          SQ_SEL := agu.aguId.resized
          SF_BYPASS := aguPush(0).sf.canBypass
          SF_DELTA  := aguPush(0).sf.read.rsp.delta
          SF_SQ_ALLOC := aguPush(0).lqsqAlloc
        } otherwise {
          when(agu.valid){
            when(agu.load) {
              lq.regs.onSel(agu.aguId.resized)(_.redoSet := True)
            } otherwise {
              sq.regs.onSel(agu.aguId.resized)(_.redoSet := True)
            }
          }
        }

        DATA_MASK := AddressToMask(ADDRESS_PRE_TRANSLATION, SIZE, wordBytes)
        LQCHECK_START_ID := sq.mem.lqAlloc.readAsync(SQ_SEL)
        SQCHECK_END_ID  := lq.mem.sqAlloc.readAsync(LQ_SEL)
        val sqCheck = new Area {
          val startId = CombInit(sq.ptr.free)
          val startMask = U(UIntToOh(U(startId.dropHigh(1)))) - 1
          val endMask = U(UIntToOh(U(SQCHECK_END_ID.dropHigh(1)))) - 1
          val loopback = startId.msb =/= SQCHECK_END_ID.msb
          val youngerMask = loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask)
          val olderMaskEmpty = startId === SQCHECK_END_ID

          SQ_YOUNGER_MASK := youngerMask
          SQCHECK_NO_OLDER := olderMaskEmpty
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

        when(isValid && IS_LOAD){
          lq.mem.sqOnRead.write(LQ_SEL, sq.ptr.free)
        }

        val SQ_FREE_ID = insert(sq.ptr.free)

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

      val checkSqMask = new Area{
        val stage = stages(sharedCheckSqAt) //TODO WARNING, SQ delay between writeback and entry.valid := False should not be smaller than the delay of reading the cache and checkSq !!
        import stage._

        val hits = Bits(sqSize bits)
        val entries = for(sqReg <- sq.regs) yield new Area {
          val pageHit = sqReg.address.pageOffset === ADDRESS_PRE_TRANSLATION(pageOffsetRange)
          val wordHit = (sqReg.address.mask & DATA_MASK) =/= 0
          hits(sqReg.id) := sqReg.valid && sqReg.virtualValid && pageHit && wordHit && SQ_YOUNGER_MASK(sqReg.id)
        }

        SQCHECK_HITS := hits

        val olderHit = !SQCHECK_NO_OLDER && SQCHECK_HITS =/= 0
        val olderOh   = if(sqSize == 1) B(1) else OHMasking.roundRobinMaskedInvert(stage(SQCHECK_HITS), sq.ptr.priorityLast)
        val olderSel  = OHToUInt(olderOh)

        OLDER_STORE_HIT := olderHit
        OLDER_STORE_ID := olderSel
        OLDER_STORE_OH := olderOh

        when(isFireing && IS_LOAD) {
          lq.regs.onSel(LQ_SEL) { r =>
            r.sqChecked := True
          }
        }
      }

      val checkSqArbi = new Area{
        val stage = stages(sharedCheckSqAt + 1) //Warning, if you remove the +1 remove some of the OLDER_STORE_COMPLETED bypass
        import stage._

        OLDER_STORE_COMPLETED := sq.ptr.onFreeLast.valid && sq.ptr.onFreeLast.payload === OLDER_STORE_ID
        for(s <- stages.dropWhile(_ != stage)){
          s.overloaded(OLDER_STORE_COMPLETED) := s(OLDER_STORE_COMPLETED) || sq.ptr.onFree.valid && sq.ptr.onFree.payload === s(OLDER_STORE_ID)
        }

        val bypass = new Area{
          val addressMatch = sq.mem.addressPost.readAsync(OLDER_STORE_ID) === tpk.TRANSLATED //TODO could check less
          val fullMatch = addressMatch && sq.mem.size.readAsync(OLDER_STORE_ID) === SIZE && !sq.mem.doNotBypass.readAsync(OLDER_STORE_ID)
          val translationFailure = sq.mem.needTranslation.readAsync(OLDER_STORE_ID)
          val data = sq.mem.data.readAsync(OLDER_STORE_ID)

          OLDER_STORE_BYPASS_SUCCESS     := !translationFailure && fullMatch
          OLDER_STORE_MAY_BYPASS         :=  translationFailure
        }

        when(isFireing && NEED_TRANSLATION) {
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
      }


      val checkLqHits = new Area{
        val stage = stages(sharedCheckSqAt)
        import stage._

        val endId = CombInit(lq.ptr.alloc)
        val startMask = U(UIntToOh(U(LQCHECK_START_ID.dropHigh(1))))-1
        val endMask   = U(UIntToOh(U(endId.dropHigh(1))))-1
        val loopback = LQCHECK_START_ID.msb =/= endId.msb
        val youngerMask = loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask)
        val youngerMaskEmpty = LQCHECK_START_ID === endId

        val entries = for(lqReg <- lq.regs) yield new Area {
          val pageHit = lqReg.address.pageOffset === ADDRESS_PRE_TRANSLATION(pageOffsetRange)
          val wordHit = (lqReg.address.mask & DATA_MASK) =/= 0
          LQCHECK_HITS(lqReg.id) := lqReg.valid && pageHit && wordHit && lqReg.sqChecked && youngerMask(lqReg.id)
        }

        LQCHECK_NO_YOUNGER := youngerMaskEmpty
      }

      val checkLqPrio = new Area{
        val stage = stages(sharedCheckSqAt+1)
        import stage._

        val youngerHit  = LQCHECK_HITS =/= 0 && !LQCHECK_NO_YOUNGER
        val youngerOh   = OHMasking.roundRobinMasked(stage(LQCHECK_HITS), lq.ptr.priorityLast)
        val youngerSel  = OHToUInt(youngerOh)
        //TODO refine it to avoid false positive ?

        YOUNGER_LOAD_PC := lq.mem.pc(youngerSel)
        YOUNGER_LOAD_ROB := lq.mem.robId.readAsync(youngerSel)
        YOUNGER_LOAD_RESCHEDULE := youngerHit
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

        when(!specialOverride && OLDER_STORE_HIT){
          rspShifted := checkSqArbi.bypass.data
        }
//        when(isValid && IS_LOAD){
//          lq.mem.bypass.done.write(LQ_ID, bypassCheck.DO_BYPASS)
//          lq.mem.bypass.sqId.write(LQ_ID, bypassCheck.SQ_ID.resize(log2Up(sqSize)))
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
          regfile.write.valid   := doIt && IS_LOAD && decoder.REGFILE_RD === decoder.REGFILE_RD.rfToId(spec)
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
        setup.sharedTrap.trap       := True
        setup.sharedTrap.cause.assignDontCare()
        setup.sharedTrap.reason.assignDontCare()
        setup.sharedTrap.pcTarget   := YOUNGER_LOAD_PC


        MISS_ALIGNED := (1 to log2Up(wordWidth/8)).map(i => SIZE === i && ADDRESS_PRE_TRANSLATION(i-1 downto 0) =/= 0).orR
        PAGE_FAULT   := (IS_LOAD ? !tpk.ALLOW_READ | !tpk.ALLOW_WRITE) || tpk.PAGE_FAULT
//        ACCESS_FAULT := rsp.fault || tpk.ACCESS_FAULT
        REGULAR      := !tpk.IO && !AMO && !SC

        def onLq(body : LqRegType => Unit) = lq.regs.onMask(lqMask)(body)
        def onSq(body : SqRegType => Unit) = sq.regs.onMask(sqMask)(body)
        def onLqSq(bodyLq : LqRegType => Unit, bodySq : SqRegType => Unit){
          when(IS_LOAD) {
            onLq(bodyLq)
          } otherwise {
            onSq(bodySq)
          }
        }

        val lqMask = UIntToOh(LQ_SEL)
        val sqMask = UIntToOh(SQ_SEL)
        
        val hitSpeculationTrap = True
        val redoTrigger = False
        when(redoTrigger){
          onLqSq(_.redoSet := True, _.redoSet := True)
        }

        val doCompletion = False
        val refillMask = rsp.refillSlot.orMask(rsp.refillSlotAny)

        when(isFireing) {
          setup.sharedTrap.valid := HIT_SPECULATION && hitSpeculationTrap

          when(MISS_ALIGNED){
            setup.sharedTrap.valid := True
            setup.sharedTrap.reason := ScheduleReason.TRAP
            setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_MISALIGNED
            setup.sharedTrap.cause(2) := !IS_LOAD
          }.elsewhen(NEED_TRANSLATION && tpk.REDO){
            onLqSq(_.waitOn.mmuRefillAnySet := True, _.waitOn.mmuRefillAnySet := True)
            redoTrigger := translationWake
          }.elsewhen(NEED_TRANSLATION && (tpk.ACCESS_FAULT || PAGE_FAULT)){
            setup.sharedTrap.valid := True
            setup.sharedTrap.reason := ScheduleReason.TRAP
            setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
            setup.sharedTrap.cause(2) := !IS_LOAD
          }.elsewhen(IS_LOAD && OLDER_STORE_HIT && !OLDER_STORE_BYPASS_SUCCESS){
            onLq{r =>
              r.waitOn.sqWritebackSet setWhen(!stage.resulting(OLDER_STORE_COMPLETED))
              r.waitOn.sqBypassSet setWhen(!OLDER_STORE_MAY_BYPASS)
              r.waitOn.sqId := OLDER_STORE_ID
            }
          }.elsewhen(IS_LOAD && rsp.redo) {
//            hadSpeculativeHitTrap setWhen(HIT_SPECULATION)
            onLq(_.waitOn.cacheRefillSet := refillMask)
            redoTrigger := !refillMask.orR
          }.elsewhen(IS_LOAD && LOAD_WRITE_FAILURE){
            redoTrigger := True
          }.otherwise {
            when(rsp.fault) {
              setup.sharedTrap.valid := True
              setup.sharedTrap.reason := ScheduleReason.TRAP
              setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
              setup.sharedTrap.cause(2) := !IS_LOAD
            }.otherwise {
              doCompletion := True
            }
          }
        }

        //Critical path extracted to help synthesis
        KeepAttribute(doCompletion)
        when(doCompletion){
          onSq(_.commited := True)
//          speculateHitTrapRecovered := LQ_SEL === 0 //Assume LQ_ID restart at 0 after a trap
//          hitSpeculationTrap := False
          when(IS_LOAD) {
            when(!tpk.IO) {
              setup.sharedCompletion.valid := True
              when(WRITE_RD && !HIT_SPECULATION) {
                wakeRob.valid := True
                wakeRf.valid := True
              }
              when(LR){
                lq.reservation.valid   := True
                lq.reservation.address := tpk.TRANSLATED
              }
            } otherwise {
              lq.mem.doSpecial.write(LQ_SEL, True)
            }

          } otherwise {
            when(YOUNGER_LOAD_RESCHEDULE){
              setup.sharedTrap.valid    := True
              setup.sharedTrap.trap     := False
              setup.sharedTrap.reason   := ScheduleReason.STORE_TO_LOAD_HAZARD
              setup.sharedTrap.robId    := YOUNGER_LOAD_ROB
            }
            when(!SC && !AMO && !tpk.IO) {
              setup.sharedCompletion.valid := True
            } otherwise {
              sq.mem.doSpecial.write(SQ_SEL, True)
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

    val prefetch = new Area{
      val predictor = new PrefetchPredictor(
        lineSize = cache.lineSize,
        addressWidth = PHYSICAL_WIDTH
      )
      //        predictor.io.prediction.ready := True //TODO
    }

    val writeback = new Area{
      import sq._

      val generation = RegInit(False)

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
        val holdPrefetch = False
        val prediction = prefetch.predictor.io.prediction.cmd.stage().haltWhen(holdPrefetch).toFlow
        //          prediction.valid clearWhen(True) //TODO remove

        //WARNING, setupCacheStore is also used by the peripheral controller to know what to do
        val io = sq.mem.io.readAsync(ptr.writeBackReal)
        val size = mem.size.readAsync(ptr.writeBackReal)
        val data = mem.data.readAsync(ptr.writeBackReal)
        val skip = False //Used for store conditional
        val doit = ptr.writeBack =/= ptr.commit && waitOn.ready && !prediction.valid
        val fire = CombInit(doit)

        setup.cacheStore.cmd.valid := doit
        setup.cacheStore.cmd.address := mem.addressPost.readAsync(ptr.writeBackReal)
        setup.cacheStore.cmd.mask :=  AddressToMask(setup.cacheStore.cmd.address, size, widthOf(setup.cacheStore.cmd.mask))
        setup.cacheStore.cmd.generation := generation
        setup.cacheStore.cmd.data.assignDontCare()
        setup.cacheStore.cmd.io := io
        setup.cacheStore.cmd.prefetch := False
        switch(size){
          for(s <- 0 to log2Up(widthOf(setup.cacheStore.cmd.data)/8)) is(s){
            val w = (1 << s)*8
            setup.cacheStore.cmd.data.subdivideIn(w bits).foreach(_ := data(0, w bits))
          }
        }

        when(prediction.valid){
          setup.cacheStore.cmd.valid := True
          setup.cacheStore.cmd.address := prediction.payload
          setup.cacheStore.cmd.io := False
          setup.cacheStore.cmd.prefetch := True
        }



        ptr.writeBack := ptr.writeBack + U(fire)
      }

      val rsp = new Area{
        val delay = sharedCheckSqAt - (sharedFeedAt + cache.loadCmdHazardFreeLatency) + cache.storeRspHazardFreeLatency - 1 // -1 because sq regs update is sequancial
        val delayed = Vec.fill(delay+1)(cloneOf(setup.cacheStore.rsp))
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

        prefetch.predictor.io.learn.valid := False
        prefetch.predictor.io.learn.allocate := False
        prefetch.predictor.io.learn.physical := delayed.last.address
        when(delayed.last.valid && !delayed.last.generationKo && !delayed.last.flush && !delayed.last.prefetch){
          prefetch.predictor.io.learn.valid := True
          when(delayed.last.redo) {
            waitOn.refillSlotSet := delayed.last.refillSlot
            waitOn.refillSlotAnySet := delayed.last.refillSlotAny
            generation := !generation
            ptr.writeBack := ptr.free
            prefetch.predictor.io.learn.allocate := True
          } otherwise {
            sq.ptr.onFree.valid := True
          }
        }

        prefetch.predictor.io.prediction.rsp.valid   := delayed.last.valid && delayed.last.prefetch
        prefetch.predictor.io.prediction.rsp.payload := delayed.last.refillSlot.orR
      }


      when(feed.doit && feed.skip){
        setup.cacheStore.cmd.valid := False
        feed.fire := True
        sq.ptr.onFree.valid := True
        ptr.writeBack := ptr.writeBack + 1
      }

      tracker.add := 0
      when(sq.ptr.onFree.valid) {
        tracker.add := 1
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

    val flush = new Area{
      val busy = RegInit(False)
      val doit = RegInit(False)
      val withFree = Reg(Bool())
      val cmdPtr, rspPtr = Reg(UInt(cache.lineRange.size+1 bits))
      def cmd = setup.cacheStore.cmd
      def rsp = setup.cacheStore.rsp

      doit := sq.ptr.commit === sq.ptr.free
      when(setup.flushPort.cmd.valid){
        cmdPtr := 0
        rspPtr := 0
        busy   := True
        doit   := False
        withFree := setup.flushPort.cmd.withFree
      }

      when(busy){
        fetch.getStage(0).haltIt()
      }

      cmd.flush := False
      cmd.flushFree := withFree
      when(busy && doit){
        cmd.valid := !cmdPtr.msb
        cmd.flush := True
        cmd.address(cache.lineRange) := cmdPtr.resized

        when(cmd.fire){
          cmdPtr := cmdPtr + 1
        }
        when(rsp.fire && !rsp.generationKo){
          when(rsp.redo){
            cmdPtr := rspPtr
            writeback.generation := !writeback.generation
          } otherwise {
            rspPtr := rspPtr + 1
          }
        }
        when(rspPtr.msb && !cache.writebackBusy){
          busy := False
          setup.flushPort.rsp.valid := True
        }
      }
    }

    val special = new Area{
      val lqOnTop = lq.mem.robId.readAsync(lq.ptr.freeReal) === commit.currentCommitRobId
      val sqOnTop = sq.mem.robId.readAsync(sq.ptr.commitReal) === commit.currentCommitRobId
      val storeWriteBackUsable = sq.ptr.writeBack === sq.ptr.commit

      val storeSpecial = sq.mem.doSpecial.readAsync(sq.ptr.commitReal)
      val loadSpecial = lq.mem.doSpecial.readAsync(lq.ptr.freeReal)
      val storeHit = sqOnTop && storeWriteBackUsable && sq.ptr.commit =/= sq.ptr.alloc && storeSpecial
      val loadHit = lqOnTop && lq.ptr.free =/= lq.ptr.alloc  && loadSpecial
      val hit = storeHit || loadHit

      val fire = CombInit(RegNext(peripheralBus.rsp.fire) init(False))
      val enabled = RegInit(False) setWhen(hit) clearWhen(fire)
      val isStore = RegNextWhen(storeHit, hit)
      val isLoad = RegNextWhen(!storeHit, hit)
      val cmdSent = RegInit(False) setWhen(peripheralBus.cmd.fire) clearWhen(fire)

      val robId = RegNext(commit.currentCommitRobId)
      val loadPhysRd = RegNext(lq.mem.physRd.readAsync(lq.ptr.freeReal))
      val loadRegfileRd = RegNext(lq.mem.regfileRd.readAsync(lq.ptr.freeReal))
      val loadAddress = RegNext(lq.mem.addressPost.readAsync(lq.ptr.freeReal))
      val loadAddressVirt = RegNext(lq.mem.addressPre.readAsync(lq.ptr.freeReal))
      val loadSize = RegNext(lq.mem.size.readAsync(lq.ptr.freeReal))
      val loadUnsigned =RegNext(lq.mem.unsigned.readAsync(lq.ptr.freeReal))
      val loadWriteRd = RegNext(isLoad && lq.mem.writeRd.readAsync(lq.ptr.freeReal))
      val storeAddress = RegNextWhen(setup.cacheStore.cmd.address, hit)
      val storeAddressVirt = RegNext(sq.mem.addressPre.readAsync(sq.ptr.commitReal))
      val storeSize = RegNext(sq.mem.size.readAsync(sq.ptr.commitReal))
      val storeData = RegNextWhen(setup.cacheStore.cmd.data, hit)
      val storeMask = RegNextWhen(setup.cacheStore.cmd.mask, hit)
      val storeAmo = RegNextWhen(sq.mem.amo.readAsync(sq.ptr.commitReal), hit)
      val storeSc = RegNextWhen(sq.mem.sc.readAsync(sq.ptr.commitReal), hit)
      val address = isStore ? storeAddress otherwise loadAddress
      val addressVirt = isStore ? storeAddressVirt otherwise loadAddressVirt

      val isIo = !(isStore && (storeAmo || storeSc))
      val isAtomic = !isIo

      val wakeRob = Flow(WakeRob())
      wakeRob.valid := False
      wakeRob.robId := robId

      val wakeRf = Flow(WakeRegFile(decoder.REGFILE_RD, decoder.PHYS_RD, needBypass = false, withRfBypass = true, rfLatency = 1)) //rfLatency 1 to relax timings even if that's likely 0
      wakeRf.valid := False
      wakeRf.physical := loadPhysRd
      wakeRf.regfile := loadRegfileRd

      peripheralBus.cmd.valid   := enabled && !cmdSent && isIo
      peripheralBus.cmd.write   := isStore
      peripheralBus.cmd.address := address
      peripheralBus.cmd.size    := isStore ? storeSize otherwise loadSize
      peripheralBus.cmd.data    := storeData
      peripheralBus.cmd.mask    := storeMask

      setup.specialTrap.valid      := False
      setup.specialTrap.robId      := robId
      setup.specialTrap.cause      := (isStore ? U(CSR.MCAUSE_ENUM.STORE_ACCESS_FAULT) otherwise U(CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT)).resized
      setup.specialTrap.tval       := B(addressVirt) //TODO PC sign extends ?
      setup.specialTrap.skipCommit := True
      setup.specialTrap.reason     := ScheduleReason.TRAP

      setup.specialCompletion.valid := False
      setup.specialCompletion.id    := robId


      when(peripheralBus.rsp.fire) {
        sharedPip.cacheRsp.specialOverride := True
        setup.specialCompletion.valid := True

        setup.specialTrap.valid := peripheralBus.rsp.error

        for(((spec, regfile), idx) <- setup.regfilePorts.zipWithIndex) {
          regfile.write.valid   := loadWriteRd && loadRegfileRd === decoder.REGFILE_RD.rfToId(spec)
          regfile.write.address := loadPhysRd
          regfile.write.robId   := robId
        }

        sharedPip.cacheRsp.rspAddress  := loadAddress.resized
        sharedPip.cacheRsp.rspSize     := loadSize
        sharedPip.cacheRsp.rspRaw      := peripheralBus.rsp.data
        sharedPip.cacheRsp.rspUnsigned := loadUnsigned

        when(loadWriteRd) {
          wakeRob.valid := True
          wakeRf.valid  := True
        }
      }

      val atomic = new StateMachine{
        val IDLE, LOAD_CMD, LOAD_RSP, ALU, COMPLETION, SYNC, TRAP = new State
        setEntry(IDLE)

        setEncoding(binaryOneHot)

        val readed = Reg(Bits(XLEN bits))
        val alu = new AtomicAlu(
          op   = sq.mem.op,
          swap = sq.mem.swap,
          mem  = readed(0, XLEN bits),
          rf   = storeData(0, XLEN bits),
          isWord = storeSize === 2
        )

        val result = Reg(Bits(XLEN bits))
        val reservationHit = RegNext(lq.reservation.valid && lq.reservation.address === storeAddress)

        when(enabled && isAtomic){
          setup.cacheLoad.translated.physical := storeAddress
          setup.cacheLoad.translated.abord    := False
        }

        val lockPort = setup.cache.lockPort
        lockPort.valid := False
        lockPort.address := storeAddress.resized
        setup.cacheLoad.cmd.unlocked := True //As we already fenced on the dispatch stage
        when(!isActive(IDLE)) {
          lockPort.valid := True
        }

        IDLE whenIsActive {
          when(enabled && isAtomic){
            when(sq.ptr.commit === sq.ptr.free){
              when(storeSc){
                goto(ALU)
              } otherwise {
                goto(LOAD_CMD)
              }
            }
          }
        }

        LOAD_CMD whenIsActive{
          val cmd = setup.cacheLoad.cmd //If you move that in another stage, be carefull to update loadFeedAt usages (sq d$ writeback rsp delay)
          cmd.valid   := True
          cmd.virtual := storeAddress.resized
          cmd.size    := storeSize
          cmd.redoOnDataHazard := False
          when(cmd.fire){
            goto(LOAD_RSP)
          }
        }

        val comp = new Area{
          val wakeRf, rfWrite = RegInit(False)

          when(wakeRf){
            sharedPip.ctrl.wakeRf.valid := True
            sharedPip.ctrl.wakeRf.physical := sq.mem.physRd
            sharedPip.ctrl.wakeRf.regfile  := sq.mem.regfileRd
            wakeRob.valid := True
          }

          val writePort = setup.regfilePorts(IntRegFile).write
          when(rfWrite) {
            writePort.valid := True
            writePort.address := sq.mem.physRd
            writePort.robId := robId
          }
        }

        LOAD_RSP onEntry{
          comp.rfWrite := True
        }

        //TODO lock the cache line ! (think that's already done ?)
        LOAD_RSP whenIsActive{
          val rsp = setup.cacheLoad.rsp
          sharedPip.cacheRsp.specialOverride := True
          readed := sharedPip.cacheRsp.rspFormated(0, XLEN bits)

          sharedPip.cacheRsp.rspAddress  := storeAddress.resized
          sharedPip.cacheRsp.rspSize     := storeSize
          if(XLEN.get == 64) sharedPip.cacheRsp.rspUnsigned := False

          when(rsp.fire){
            comp.rfWrite := False
            when(rsp.redo){
              goto(IDLE)
            } elsewhen (rsp.fault) {
              goto(TRAP)
            } otherwise {
              goto(ALU)
            }
          }
        }

        TRAP whenIsActive{
          setup.specialTrap.valid := True
          goto(IDLE)
        }

        ALU whenIsActive{
          result := alu.result
          goto(COMPLETION)
        }

        COMPLETION.onEntry{
          comp.wakeRf  := sq.mem.writeRd
          comp.rfWrite := storeSc && sq.mem.writeRd
        }
        COMPLETION whenIsActive{
          setup.specialCompletion.valid := True
          comp.wakeRf := False
          comp.rfWrite := False
          comp.writePort.data := 0
          comp.writePort.data(0) := !reservationHit
          goto(SYNC)
        }

        SYNC.whenIsActive{
          when(storeAmo) {
            writeback.feed.data(0, XLEN bits) := result
          }
          when(storeSc && !reservationHit){
            writeback.feed.skip := True
          }

          when(sq.ptr.onFree.valid) {
            fire := True
            when(storeSc) {
              lq.reservation.valid := False
            }
            goto(IDLE)
          }
        }

        frontend.pipeline.dispatch.haltIt(isActive(SYNC))
      }
    }

    hardFork{
      sharedPip.translationPort.pipelineLock.await()
      sharedPip.build()
      lqSqArbitration.build()
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
      special.enabled := False


    }

    //TODO
//    store.writeback.feed.holdPrefetch setWhen(flush.busy)
//    store.writeback.feed.holdPrefetch setWhen(special.enabled)

    //TODO
    storeForwarding.write.setIdle()
//    setup.cache.lockPort.valid := False
//    setup.cache.lockPort.address := 0
//    setup.cacheLoad.cmd.unlocked := True
//    peripheralBus.cmd.setIdle()
    //todo remove LOAD_CHECK ?
    //TODO check interaction between windowFilter bypass and IO access
//    assert(setup.flushPort.cmd.valid === False,"Please implement lsu flush port")

    //TODO WARNING, AMO / SC should write the windowfilter
//    setup.cacheStore.cmd.assignDontCare()
//    slave(setup.cacheStore)

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

//make compile && obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/rv32ima/play.elf --pass-symbol pass --trace --trace-ref --trace-gem5
//make test-clean output/riscv_tests/rv32ui-p-sw/PASS ARGS="--trace --trace-ref --trace-gem5 --spike-debug --output-dir output"
//make test-fast -j && find . -name PASS