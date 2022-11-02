package naxriscv.lsu2

import naxriscv.Frontend._
import naxriscv.Global._
import naxriscv.fetch.FetchPlugin
import naxriscv.frontend.{DispatchPlugin, FrontendPlugin}
import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.lsu.{DataCachePlugin, LsuUtils}
import naxriscv.misc.RobPlugin
import naxriscv.riscv.Rvi
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin, WithRfWriteSharedSpec}
import spinal.lib.pipeline._
import scala.collection.mutable.ArrayBuffer

/*

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
      aguIdSize       = sqSize max lqSize,
      wordWidth       = wordWidth,
      physicalRdWidth = widthOf(getService[DecoderService].PHYS_RD),
      regfileRdWidth  = widthOf(getService[DecoderService].REGFILE_RD),
      pcWidth         = VIRTUAL_EXT_WIDTH
    )))).port
  }

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
    val ADDRESS_PRE_TRANSLATION = Stageable(UInt(VIRTUAL_EXT_WIDTH bits))
    val DATA_MASK = Stageable(Bits(wordBytes bits))


    val LQ_ID = Stageable(UInt(log2Up(lqSize + 1) bits))
    val SQ_ID = Stageable(UInt(log2Up(sqSize + 1) bits))
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
  }

  val logic = create late new Area{
    val imp = setup.get
    import imp._

    val lq = new Area{
      val replay = Reg(Bits(lqSize bits)) init(0)
      val regs = List.tabulate(lqSize)(RegType)
      case class RegType(id : Int) extends Area{
        val allocation = False
        val valid = Bool()
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

      val ptr = new Area{
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
      val replay = Reg(Bits(sqSize bits)) init(0)
      val regs = List.tabulate(sqSize)(RegType)
      case class RegType(id : Int) extends Area{
        val allocation = False
        val valid = Bool()
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

      val ptr = new Area{
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
            lq.regs.map(_.allocation).write(LQ_ID, True)
//            mem.sqAlloc.write(
//              address = LQ_ID,
//              data = U(SQ_ID_CARRY ## allocStage(SQ_ID, slotId))
//            )
          }
          loads.alloc \= loads.alloc + 1
        }
        when(stores.requests(slotId)){
          LSU_ID := stores.alloc
          when(isFireing) {
            //...
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

    }

    rob.release()
    decoder.release()
    frontend.release()
    fetch.release()
    translation.release()
  }
}
