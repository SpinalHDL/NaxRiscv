package naxriscv.lsu2

import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.execute.EnvCallPlugin
import naxriscv.fetch.FetchPlugin
import naxriscv.frontend.{DispatchPlugin, FrontendPlugin}
import naxriscv.interfaces.AddressTranslationPortUsage.LOAD_STORE
import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.lsu.{DataCachePlugin, LsuFlushPayload, LsuFlusher, LsuPeripheralBus, LsuUtils, PrefetchPredictor}
import naxriscv.misc.RobPlugin
import naxriscv.riscv.{CSR, FloatRegFile, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{AddressToMask, DocPlugin, Plugin, WithRfWriteSharedSpec}
import spinal.core.fiber.hardFork
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


    val LOAD_CHECK_DATA = Stageable(Bits(wordWidth bits))
    val LOAD_CHECK, LOAD_CHECK_MATCHED = Stageable(Bool())
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

    val SQ_ALLOC_ID = Stageable(UInt(windowFilterIdWidth bits))
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
    val sharedTrap = commit.newSchedulePort(canTrap = true, canJump = false)
    val specialTrap = commit.newSchedulePort(canTrap = true, canJump = false)
    val specialCompletion = rob.newRobCompletion()
    val windowFilter = new Area{
      val trap = commit.newSchedulePort(canTrap = true, canJump = false)
      val completion = rob.newRobCompletion()
    }
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
    val sqBypassEvent    = Flow(SQ_ID)
    case class LqRegType(id : Int) extends Area{
      val allocation = False
      val valid      = RegInit(False)
      val redo       = RegInit(False)
      val redoSet    = False
      val delete     = False

      val waitOn = new Area {
        val cacheRefill    = Reg(Bits(cache.refillCount bits)) init(0)
        val cacheRefillAny = RegInit(False)
        val mmuRefillAny   = RegInit(False)
        val sqWriteback    = RegInit(False)
        val sqBypass       = RegInit(False)
//        val sqWritebackId  = Reg(SQ_ID)
//        val sqBypassId     = Reg(SQ_ID)

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
           mmuRefillAny && translationWake /*||
           sqWriteback  && sqWritebackEvent.valid && sqWritebackEvent.payload === sqWritebackId ||
           sqBypass     && sqBypassEvent.valid    && sqBypassEvent.payload    === sqBypassId*/
        )

//        val commitSet = False
//        val commit = Reg(Bool()) setWhen(commitSet)


      }



      when(delete){
        valid := False
      }
      when(allocation){
        valid := True
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
//        commited := False
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
      val mem = Mem.fill(loadBypassPredEntries)(new Bundle{
        val tag = Bits(loadBypassPredTagWidth bits)
        val delta = UInt(log2Up(sqSize) bits)
        val bypass = Bool()
        val writeback = Bool()
      })
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
        val doSpecial       = create(Bool())
        val doWindowFilter  = create(Bool())
        val allocId = create(SQ_ALLOC_ID)

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

        val onFree = Flow(UInt(log2Up(sqSize) bits))
        val onFreeLast = onFree.stage()

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
            sq.mem.doWindowFilter.write(SQ_ID, False)
            sq.mem.doSpecial.write(SQ_ID, False)
            sq.mem.allocId.write(SQ_ID, stores.allocId)
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

    /** Will take in programm oder the store and load which are ready for commit
     *  For store => no execution side effects
     *  For loads :
     *  - No conflict
     *    - Didn't bypass => OK, notify commit
     *    - Did bypass => KO, trap redo, unlearn stuff
     *  - Bypassable conflict
     *    - Didn't bypassed,or bypassed wrong => need re-execution load check (hopping for false positive) or trap redo (pessimistic)
     *    - Did the bypass right => notify commit
     *  - Not bypassable conflict / overflow
     *    - trap redo
     *  - Do training of bypass prediction
     *
     *  So overall here is a load lifecycle
     *  - AGU
     *  - Shared pipe provide load data
     *  - window filter check store to load hazard in a pessimistic manner
     *    - Make store to load bypass predictor learn
     *
     *  Learning is easy, and unlearning can be done when no conflict was detected while using the load allocation store id
     *
     *  Datapath for load :
     *  - Read context
     *  - Read store filter
     *  - Hit process
     *  - If all ok => commit
     *  - If conflict
     *    - Read physical store address/size and check it match
     */
    val windowFilter = new Pipeline {
      val ways = Mem.fill(windowFilterWays)(new Bundle{
        val tag = Bits(windowFilterTagWidth bits)
        val id = UInt(windowFilterIdWidth bits)
      })
      val feed = new Stage(){
        val l = new Area{
          val ptr             = Reg(UInt(log2Up(lqSize) + 1 bits)) init(0)
          val olderSqId       = lq.mem.sqAlloc.readAsync(ptr.resized)
          val sqAllocId       = lq.mem.sqAllocId.readAsync(ptr.resized)
          val sqOnRead        = lq.mem.sqOnRead.readAsync(ptr.resized)
          val sqAllocIdOnRead = lq.mem.sqAllocIdOnRead.readAsync(ptr.resized)
          val ready           = lq.mem.doWindowFilter.readAsync(ptr.resized)
          val special         = lq.mem.doSpecial.readAsync(ptr.resized)
          val address         = lq.mem.addressPost.readAsync(ptr.resized)
          val size            = lq.mem.size.readAsync(ptr.resized)
          val robId           = lq.mem.robId.readAsync(ptr.resized)
          val mask            = AddressToMask(address, size, wordBytes)
//          val hashed          = hash(address)
          val valid           = ptr =/= lq.ptr.alloc
        }
        val s = new Area{
          val ptr     = Reg(UInt(log2Up(sqSize) + 1 bits)) init(0)
          val ready   = sq.mem.doWindowFilter.readAsync(ptr.resized)
          val special = sq.mem.doSpecial.readAsync(ptr.resized)
          val address = sq.mem.addressPost.readAsync(ptr.resized)
          val size    = sq.mem.size.readAsync(ptr.resized)
          val allocId = sq.mem.allocId.readAsync(ptr.resized)
          val mask    = AddressToMask(address, size, wordBytes)
//          val hashed  = hash(address)
          val valid   = ptr =/= sq.ptr.alloc

          val backup = for(_ <- 0 until sqSize) yield new Area{
            val address = Reg(UInt(windowFilterTagWidth bits))
            val mask    = Reg(Bits(wordBytes bits))
          }
        }

        val cmp = s.ptr === l.olderSqId
        val lqIsOlder = l.valid &&  cmp
        val sqIsOlder = s.valid && !lqIsOlder

        //TODO handle AMO LR SC

//        when(sqIsOlder && s.ready){
//          ways.write.valid := True
//          ways.write.address := s.hashed
//          ways.write.data.foreach(_ := s.allocId)
//          ways.write.mask := B(s.mask)
//        }


        val DO_LQ = insert(lqIsOlder && l.ready)
        val DO_SQ = insert(sqIsOlder && s.ready)
        isValid := DO_LQ && !l.special || DO_SQ && !s.special
        when(isReady) {
          when(DO_LQ) {
            l.ptr := l.ptr + 1
          }
          when(DO_SQ) {
            s.ptr := s.ptr + 1
            s.backup.onSel(s.ptr.resized){r =>
              r.address := (s.address >> wordSizeWidth).resized
              r.mask    := s.mask
            }
          }
        }

        val hits = Bits(sqSize bits)
        val entries = for((e, hit) <- (s.backup, hits.asBools).zipped) yield new Area{
          val pageHit =  e.address === (l.address >> wordSizeWidth).resized
          val wordHit = (e.mask & l.mask) =/= 0
          hit := pageHit && wordHit
        }

        val startId = CombInit(l.sqOnRead)
        val endId = CombInit(s.ptr)
        val startMask = U(UIntToOh(U(startId.dropHigh(1)))) - 1
        val endMask = U(UIntToOh(U(endId.dropHigh(1)))) - 1
        val loopback = startId.msb =/= endId.msb
        val olderMask = B(loopback ? ~(endMask ^ startMask) otherwise (endMask & ~startMask))
        val olderMaskEmpty = startId === endId

        val hazards = olderMask & hits
        val hazard = !olderMaskEmpty && hazards.orR
        val youngerOh = OHMasking.lastV2((hazards & B(endMask)) ## hazards)
        val youngerSel = U(OHToUInt(youngerOh).dropHigh(1))

        val addressMatch = sq.mem.addressPost.readAsync(youngerSel) === l.address
        val sizeMatch = sq.mem.size.readAsync(youngerSel) === l.size
        val fullMatch = addressMatch && sizeMatch
        val shouldBypass = hazard && fullMatch
        val shouldWaitWriteback = hazard && !fullMatch
        val bypass = new Area{
//          val valid = lq.mem.bypass.valid.readAsync(l.ptr)
//          val id    = lq.mem.bypass.id.readAsync(l.ptr)
        }

        val trap = setup.windowFilter.trap
//        trap.valid      := False
//        trap.robId      := l.robId
//        trap.skipCommit := True
//        trap.cause      := EnvCallPlugin.CAUSE_REDO
//        trap.reason     := ScheduleReason.STORE_TO_LOAD_HAZARD
//        trap.tval.assignDontCare()

        val completion = setup.windowFilter.completion
//        completion.valid := False
//        completion.id    := l.robId
//
//        when(isFireing && DO_LQ){
//          when(hazard){
//            trap.valid := True
//          } otherwise {
//            completion.valid := True
//          }
//        }

        in(trap, completion)
      }

      this.stagesSet.last.flushIt(rescheduling.valid, root = false)
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
        BYPASS_TOO_EARLY := False //TODO

        val agu = aguPorts.head.port
        val takeAgu = (LQ_OLDER_THAN_SQ ? (agu.robIdFull - LQ_ROB_FULL).msb | (agu.robIdFull - SQ_ROB_FULL).msb)
        takeAgu.setWhen(!lqSqFeed.isValid)
        takeAgu.clearWhen(!agu.valid)

        isValid := agu.valid || lqSqFeed.isValid
        lqSqFeed.haltWhen(takeAgu)

        val lqSelArbi = lqSqFeed(LQ_SEL)
        val sqSelArbi = lqSqFeed(SQ_SEL)
        LQ_OLDER_THAN_SQ := lqSqFeed(LQ_OLDER_THAN_SQ)
        LQ_ROB_FULL := lqSqFeed(LQ_ROB_FULL)
        SQ_ROB_FULL := lqSqFeed(SQ_ROB_FULL)

        val lqSqSerializer = new Area {
          val lqMask, sqMask = RegInit(True)
          LQ_OLDER_THAN_SQ.clearWhen(!lqMask)

          when(lqSqFeed(SQ_HIT) && sqMask && lqSqFeed(LQ_HIT) && lqMask) {
            lqSqFeed.haltIt()
            when(!takeAgu && isReady) {
              lqMask clearWhen (lqSqFeed(LQ_OLDER_THAN_SQ))
              sqMask clearWhen (!lqSqFeed(LQ_OLDER_THAN_SQ))
            }
          }

          when(lqSqFeed.isRemoved || lqSqFeed.isReady || !lqSqFeed.isValid) {
            lqMask := True
            sqMask := True
          }
        }

        ROB.ID := Mux[UInt](LQ_OLDER_THAN_SQ, LQ_ROB_FULL, SQ_ROB_FULL).resized
        decoder.PHYS_RD    := lq.mem.physRd.readAsync(lqSelArbi)
        decoder.REGFILE_RD := lq.mem.regfileRd.readAsync(lqSelArbi)

        def readQueues[T <: Data](key : Stageable[T], lqMem : Mem[T], sqMem : Mem[T]) : Unit = {
          stage(key, load)  := lqMem.readAsync(lqSelArbi)
          stage(key, store) := sqMem.readAsync(sqSelArbi)
          stage(key) := Mux[T](LQ_OLDER_THAN_SQ, (key, load), (key, store))
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
        readLq(LOAD_CHECK, lq.mem.doWindowFilter)
        readLq(LOAD_CHECK_DATA, lq.mem.data)
        readSq(AMO, sq.mem.amo)
        readSq(SC, sq.mem.sc)

        IS_LOAD := LQ_OLDER_THAN_SQ

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
          LOAD_CHECK := False
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

        when(isValid && IS_LOAD){
          lq.mem.sqOnRead.write(LQ_SEL, sq.ptr.free)
        }

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
          regfile.write.valid   := doIt && IS_LOAD && !LOAD_CHECK && decoder.REGFILE_RD === decoder.REGFILE_RD.rfToId(spec)
          regfile.write.address := decoder.PHYS_RD
          regfile.write.data    := rspFormated.resized
          regfile.write.robId   := ROB.ID

          if(RVD && spec == FloatRegFile) when(stage(SIZE) === 2){
            regfile.write.data(63 downto 32).setAll()
          }
        }

        LOAD_WRITE_FAILURE := IS_LOAD && specialOverride && !tpk.IO
        LOAD_CHECK_MATCHED := LOAD_CHECK_DATA === rspFormated
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
        setup.sharedTrap.cause.assignDontCare()
        setup.sharedTrap.reason.assignDontCare()


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
            setup.sharedTrap.cause(2) := !IS_LOAD
          }.elsewhen(NEED_TRANSLATION && tpk.REDO){
            onLqSq(_.waitOn.mmuRefillAnySet := True, _.waitOn.mmuRefillAnySet := True)
            redoTrigger := translationWake
          }.elsewhen(NEED_TRANSLATION && (tpk.ACCESS_FAULT || PAGE_FAULT)){
            setup.sharedTrap.valid := True
            setup.sharedTrap.reason := ScheduleReason.TRAP
            setup.sharedTrap.cause := CSR.MCAUSE_ENUM.LOAD_ACCESS_FAULT
            setup.sharedTrap.cause(2) := !IS_LOAD
          }.elsewhen(IS_LOAD && BYPASS_TOO_EARLY){
            report("Implement me BYPASS_TOO_EARLY")
//            onRegs{r =>
//              r.waitOn.sq setWhen(!stage.resulting(OLDER_STORE_COMPLETED))
//              r.waitOn.sqId := OLDER_STORE_ID
//              r.waitOn.sqCompletion := !OLDER_STORE_MAY_BYPASS
//            }
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
            lq.mem.doWindowFilter.write(LQ_SEL, True)
            when(!LR && !tpk.IO) {
              when(LOAD_CHECK) {
                setup.sharedCompletion.valid := True
                when(!LOAD_CHECK_MATCHED){
                  setup.sharedTrap.valid      := False
                  setup.sharedTrap.skipCommit := True
                  setup.sharedTrap.cause      := EnvCallPlugin.CAUSE_REDO
                  setup.sharedTrap.reason     := ScheduleReason.STORE_TO_LOAD_HAZARD
                }
              }
              when(WRITE_RD && !HIT_SPECULATION && !LOAD_CHECK) {
                wakeRob.valid := True
                wakeRf.valid := True
              }
            } otherwise {
              lq.mem.doSpecial.write(LQ_SEL, True)
            }
            //          when(LR){
            //            lq.reservation.valid   := True
            //            lq.reservation.address := tpk.TRANSLATED
            //          }

          } otherwise {
            sq.mem.doWindowFilter.write(SQ_SEL, True)
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
        val delayed = Vec.fill(1)(cloneOf(setup.cacheStore.rsp))
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
    }

    hardFork{
      sharedPip.translationPort.pipelineLock.await()
      sharedPip.build()
      lqSqArbitration.build()
      windowFilter.build()
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

      windowFilter.feed.l.ptr := 0
      windowFilter.feed.s.ptr := sq.ptr.commitNext.resized
    }

    //TODO
//    store.writeback.feed.holdPrefetch setWhen(flush.busy)
//    store.writeback.feed.holdPrefetch setWhen(special.enabled)

    //TODO
    setup.cache.lockPort.valid := False
    setup.cache.lockPort.address := 0
    setup.cacheLoad.cmd.unlocked := True
//    peripheralBus.cmd.setIdle()
    //todo remove LOAD_CHECK ?
    assert(setup.flushPort.cmd.valid === False,"Please implement lsu flush port")
    sqWritebackEvent.setIdle()
    sqBypassEvent.setIdle()
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