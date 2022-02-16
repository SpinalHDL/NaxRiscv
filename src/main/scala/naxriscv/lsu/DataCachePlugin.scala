package naxriscv.lsu

import naxriscv.Global
import naxriscv.Global.XLEN
import naxriscv.interfaces.{AddressTranslationService, LockedImpl, PerformanceCounterService}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.lib.pipeline.Pipeline

import scala.collection.mutable.ArrayBuffer



class DataCachePlugin(val memDataWidth : Int,
                      val cacheSize: Int,
                      val wayCount: Int,
                      val refillCount : Int,
                      val writebackCount : Int,
                      val lineSize: Int = 64,
                      val loadRefillCheckEarly : Boolean = true,
                      val storeRefillCheckEarly : Boolean = true,
                      val loadReadBanksAt: Int = 0,
                      val loadReadTagsAt: Int = 1,
                      val loadTranslatedAt : Int = 1,
                      val loadHitsAt: Int = 1,
                      val loadHitAt: Int = 2,
                      val loadBankMuxesAt: Int = 1,
                      val loadBankMuxAt: Int = 2,
                      val loadControlAt: Int = 2,
                      val loadRspAt: Int = 2,
                      val storeReadBanksAt: Int = 0,
                      val storeReadTagsAt: Int = 1,
                      val storeHitsAt: Int = 1,
                      val storeHitAt: Int = 1,
                      val storeControlAt: Int = 2,
                      val storeRspAt: Int = 2,
                      val tagsReadAsync : Boolean = true,
                      val reducedBankWidth : Boolean = false
                     ) extends Plugin with LockedImpl{
  def loadRspLatency = loadRspAt
  def storeRspLatency = storeRspAt

  def storeRspHazardFreeLatency = (storeControlAt+1)-storeRspAt
  def loadCmdHazardFreeLatency = (loadReadBanksAt)

  def waySize = cacheSize/wayCount
  def linePerWay = waySize/lineSize
  def lineRange = log2Up(linePerWay*lineSize) -1 downto log2Up(lineSize)

  def cpuDataWidth = XLEN.get
  def preTranslationWidth : Int = getService[AddressTranslationService].preWidth
  def postTranslationWidth : Int = getService[AddressTranslationService].postWidth

  def writebackBusy = setup.writebackBusy

  case class LoadPortSpec(port : DataLoadPort, priority : Int)
  val loadPorts = ArrayBuffer[LoadPortSpec]()
  def newLoadPort(priority : Int): DataLoadPort = {
    loadPorts.addRet(LoadPortSpec(
      DataLoadPort(
        preTranslationWidth  = preTranslationWidth,
        postTranslationWidth = postTranslationWidth,
        dataWidth     = cpuDataWidth,
        refillCount   = refillCount,
        rspAt         = loadRspAt,
        translatedAt  = loadTranslatedAt
      ),
      priority
    )).port
  }

  case class StorePortSpec(port : DataStorePort)
  val storePorts = ArrayBuffer[StorePortSpec]()
  def newStorePort(): DataStorePort = {
    storePorts.addRet(StorePortSpec(DataStorePort(
      postTranslationWidth = postTranslationWidth,
      dataWidth     = cpuDataWidth,
      refillCount   = refillCount
    ))).port
  }
  
  def refillCompletions = setup.refillCompletions

  def lockPort = setup.lockPort

  val setup = create early new Area{

    val doc = getService[DocPlugin]
    doc.property("DATA_MEM_DATA_BITS", memDataWidth)
    doc.property("DATA_LINE_BYTES", lineSize)
    doc.property("DATA_CACHE_REFILL_COUNT", refillCount)
    doc.property("DATA_CACHE_WRITEBACK_COUNT", writebackCount)

    val writebackBusy = Bool()
    val perf = getServiceOption[PerformanceCounterService]
    val refillEvent = perf.map(_.createEventPort(PerformanceCounterService.DCACHE_REFILL))
    val writebackEvent = perf.map(_.createEventPort(PerformanceCounterService.DCACHE_WRITEBACK))

    val refillCompletions = Bits(refillCount bits)

    val lockPort = LockPort()
  }

  val logic = create late new Area{
    lock.await()


    val cache = new DataCache(
      cacheSize       = cacheSize,
      wayCount        = wayCount,
      memDataWidth    = memDataWidth,
      cpuDataWidth    = cpuDataWidth,
      refillCount     = refillCount,
      writebackCount  = writebackCount,
      preTranslationWidth    = preTranslationWidth,
      postTranslationWidth   = postTranslationWidth,
      lineSize         = lineSize,
      loadRefillCheckEarly  = loadRefillCheckEarly,
      storeRefillCheckEarly = storeRefillCheckEarly,
      loadReadBanksAt  = loadReadBanksAt,
      loadReadTagsAt   = loadReadTagsAt,
      loadTranslatedAt = loadTranslatedAt,
      loadHitsAt       = loadHitsAt,
      loadHitAt        = loadHitAt,
      loadBankMuxesAt  = loadBankMuxesAt,
      loadBankMuxAt    = loadBankMuxAt,
      loadControlAt    = loadControlAt,
      loadRspAt        = loadRspAt,
      storeReadBanksAt = storeReadBanksAt,
      storeReadTagsAt  = storeReadTagsAt,
      storeHitsAt      = storeHitsAt,
      storeHitAt       = storeHitAt,
      storeControlAt   = storeControlAt,
      storeRspAt       = storeRspAt,
      tagsReadAsync    = tagsReadAsync,
      reducedBankWidth = reducedBankWidth
    )

    setup.writebackBusy <> cache.io.writebackBusy
    setup.lockPort <> cache.io.lock
    setup.refillEvent.map(_ := RegNext(cache.io.refillEvent) init(False))
    setup.writebackEvent.map(_ := RegNext(cache.io.writebackEvent) init(False))

    setup.refillCompletions := cache.io.refillCompletions

    val load = new Area{
      assert(loadPorts.map(_.priority).distinct.size == loadPorts.size)
      val sorted = loadPorts.sortBy(_.priority).reverse //High priority first
      val hits = B(sorted.map(_.port.cmd.valid))
      val hit = hits.orR
      val oh = OHMasking.firstV2(hits)
      val ohHistory = History(oh, 0 to loadRspAt, init= B(0, sorted.size bits))

      cache.io.load.cmd.valid := hit
      cache.io.load.cmd.payload := OhMux(oh, sorted.map(_.port.cmd.payload))
      (sorted, oh.asBools).zipped.foreach(_.port.cmd.ready := _ )

      cache.io.load.cancels := sorted.map(_.port.cancels).reduceBalancedTree(_ | _)
      cache.io.load.translated := OhMux(ohHistory(loadTranslatedAt), sorted.map(_.port.translated))

      for((spec, sel) <- (sorted, ohHistory(loadRspAt).asBools).zipped){
        spec.port.rsp.valid := cache.io.load.rsp.valid && sel
        spec.port.rsp.payload := cache.io.load.rsp.payload
      }
    }

    val store = new Area{
      assert(storePorts.size == 1)
      cache.io.store <> storePorts.head.port
    }
  }

  val mem = create late logic.cache.io.mem.toIo()
}
