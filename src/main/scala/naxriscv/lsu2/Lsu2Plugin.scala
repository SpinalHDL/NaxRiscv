package naxriscv.lsu2

import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.fetch.FetchPlugin
import naxriscv.frontend.{DispatchPlugin, FrontendPlugin}
import naxriscv.interfaces.AddressTranslationPortUsage.LOAD_STORE
import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.lsu.{DataCachePlugin, LsuUtils}
import naxriscv.misc.RobPlugin
import naxriscv.riscv.Rvi
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin, WithRfWriteSharedSpec}
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
}


class Lsu2Plugin(var lqSize: Int,
                 var sqSize : Int,
                 var translationStorageParameter : Any,
                 var loadTranslationParameter : Any,
                 var sharedFeedAt : Int = 1, //Stage at which the d$ cmd is sent
                 var sharedCtrlAt : Int = 4,
                 var lqToCachePipelined : Boolean = true,
                 var storeReadRfWithBypass : Boolean = false) extends Plugin with PostCommitBusy with WithRfWriteSharedSpec{


  def wordWidth = LSLEN
  def wordBytes = wordWidth/8
  def wordSizeWidth = LsuUtils.sizeWidth(wordWidth)
  def pageOffsetRange = 11 downto log2Up(wordBytes)
  def pageNumberRange = Global.XLEN.get-1 downto 12
  def pageOffsetWidth = pageOffsetRange.size
  def pageNumberWidth = pageNumberRange.size
  override def postCommitBusy = setup.postCommitBusy

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
    val DATA_MASK = Stageable(Bits(wordBytes bits))


    val LQ_HIT = Stageable(Bool())
    val SQ_HIT = Stageable(Bool())

    val LQ_ID = Stageable(UInt(log2Up(lqSize + 1) bits))
    val SQ_ID = Stageable(UInt(log2Up(sqSize + 1) bits))


    val LQ_SQ_ID = Stageable(UInt(log2Up(sqSize) + 1 bits))
    val LQ_OLDER_THAN_SQ = Stageable(Bool())
  }
  import keys._


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
    val postCommitBusy = Bool()


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
    val translationStorage = translation.newStorage(translationStorageParameter)
  }

  val logic = create late new Area{
    val imp = setup.get
    import imp._

    val rescheduling = commit.reschedulingPort(onCommit = true)

    val lq = new Area{
      val regs = List.tabulate(lqSize)(RegType)
      case class RegType(id : Int) extends Area{
        val allocation = False
        val valid = RegInit(False)
        val redo = RegInit(False)
//        val waitOn = new Area {
//          val cacheRefill    = Reg(Bits(cache.refillCount bits))
//          val cacheRefillAny = Reg(Bool())
//          val mmuRefill      = Reg(Bool())
//          val sq             = Reg(Bool())
//          val sqCompletion = Reg(Bool())
//          val sqId   = Reg(SQ_ID)
//          val sqPredicted = withHazardPrediction generate Reg(Bool())
//        }

        when(allocation){
          valid := True
        }
      }

      val mem = new Area{
        def create[T <: Data](t : HardType[T]) = Mem.fill(lqSize)(t)
        val addressPre  = create(UInt(VIRTUAL_EXT_WIDTH bits))
        val addressPost = create(UInt(PHYSICAL_WIDTH bits))
        val physRd      = create(decoder.PHYS_RD)
        val regfileRd   = create(decoder.REGFILE_RD)
        val robId       = create(ROB.ID)
        val pc          = create(PC)
        val sqAlloc     = create(UInt(log2Up(sqSize)+1 bits))
        val io          = create(Bool())
        val writeRd     = create(Bool())
        val lr          = create(Bool())
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
    }


    val sq = new Area{
      val regs = List.tabulate(sqSize)(RegType)
      case class RegType(id : Int) extends Area{
        val allocation = False
        val valid = Bool()
        val redo = RegInit(False)
        //        val waitOn = new Area {
        //          val cacheRefill    = Reg(Bits(cache.refillCount bits))
        //          val cacheRefillAny = Reg(Bool())
        //          val mmuRefill      = Reg(Bool())
        //          val sq             = Reg(Bool())
        //          val sqCompletion = Reg(Bool())
        //          val sqId   = Reg(SQ_ID)
        //          val sqPredicted = withHazardPrediction generate Reg(Bool())
        //        }

        when(allocation){
          valid := True
        }
      }


      val mem = new Area{
        def create[T <: Data](t : HardType[T]) = Mem.fill(sqSize)(t)
        val sqIdMsb          = create(Bool())
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
        LQ_ID := loads.alloc
        SQ_ID := stores.alloc
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
            wr(sq.mem.sqIdMsb, allocStage(SQ_ID, slotId).msb)

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
    }

    val sharedPip = new Pipeline{
      val stages = Array.fill(sharedCtrlAt+1)(newStage())
      connect(stages)(List(M2S()))
      stages.last.flushIt(rescheduling.valid, root = false)

      val translationPort = translation.newTranslationPort(
        stages = this.stages,
        preAddress = ADDRESS_PRE_TRANSLATION,
        usage = LOAD_STORE,
        portSpec = loadTranslationParameter,
        storageSpec = setup.translationStorage
      )
      val tpk = translationPort.keys

      val arbitration = new Area{
        val stage = stages(0)
        import stage._

        val lqRedo = B(lq.regs.map(reg => reg.redo))
        val sqRedo = B(sq.regs.map(reg => reg.redo))

        LQ_SEL_OH := OHMasking.roundRobinMasked(lqRedo, lq.ptr.priority)
        SQ_SEL_OH := OHMasking.roundRobinMasked(sqRedo, sq.ptr.priority)
        LQ_HIT := LQ_SEL_OH.orR
        SQ_HIT := SQ_SEL_OH.orR
        LQ_SEL := OHToUInt(LQ_SEL_OH)
        SQ_SEL := OHToUInt(SQ_SEL_OH)

        isValid := LQ_HIT || SQ_HIT

        LQ_SQ_ID := lq.mem.sqAlloc.readAsync(LQ_SEL)
        SQ_ID := U(sq.mem.sqIdMsb.readAsync(LQ_SEL) ## SQ_ID)
        LQ_OLDER_THAN_SQ := LQ_HIT && !(SQ_ID - LQ_SQ_ID).msb

      }

      val feed = new Area {
        val stage = stages(sharedFeedAt)
        import stage._

        val agu = aguPorts.head.port
        val takeAgu = agu.robId

        //Get the oldest load in the LQ ready for the cache access
        val arbitration = new Area {
          def area(hits : Bits, priority : Bits)(onFire : Bits => Unit) = new Area{
            case class Payload() extends Bundle {
              val selOh = Bits(lqSize bits)
              val sel = UInt(log2Up(lqSize) bits)
            }

            val selOh = OHMasking.roundRobinMasked(hits, priority)
            val output = Stream(Payload())
            output.valid := hits.orR
            output.selOh := selOh
            output.sel := OHToUInt(selOh)
            when(output.fire) {
              onFire(selOh)
            }
          }
          val lqSource = area(B(lq.regs.map(reg => reg.redo)), lq.ptr.priority){mask =>
            lq.regs.onMask(mask){ reg =>
              reg.redo := False
            }
          }
          val sqSource = area(B(sq.regs.map(reg => reg.redo)), sq.ptr.priority){mask =>
            sq.regs.onMask(mask){ reg =>
              reg.redo := False
            }
          }

//          when(output.fire) {
//            lq.regs.onMask(selOh){ reg =>
//              reg.redo := False
//            }
//          }
//
//          val output = if (lqToCachePipelined) early.m2sPipe(flush = stages.head.isFlushed) else early.combStage()
//
//          def outputSel = output.sel //OHToUInt(output.payload)
        }

//        isValid := arbitration.output.valid
      }
    }

    rob.release()
    decoder.release()
    frontend.release()
    fetch.release()
    translation.release()
  }
}
