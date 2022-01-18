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
                      val loadReadAt: Int = 0,
                      val loadTranslatedAt : Int = 1,
                      val loadHitsAt: Int = 1,
                      val loadHitAt: Int = 1,
                      val loadBankMuxesAt: Int = 1,
                      val loadBankMuxAt: Int = 2,
                      val loadControlAt: Int = 2,
                      val loadRspAt: Int = 2,
                      val storeReadAt: Int = 0,
                      val storeHitsAt: Int = 1,
                      val storeHitAt: Int = 1,
                      val storeControlAt: Int = 2,
                      val storeRspAt: Int = 2,
                      val reducedBankWidth : Boolean = false
                     ) extends Plugin with LockedImpl{
  def loadRspLatency = loadRspAt
  def storeRspLatency = storeRspAt

  def storeRspHazardFreeLatency = (storeControlAt+1)-storeRspAt
  def loadCmdHazardFreeLatency = (loadReadAt)

  val cpuDataWidth = XLEN.get
  def preTranslationWidth : Int = getService[AddressTranslationService].preWidth
  def postTranslationWidth : Int = getService[AddressTranslationService].postWidth

  case class LoadPortSpec(port : DataLoadPort)
  val loadPorts = ArrayBuffer[LoadPortSpec]()
  def newLoadPort(): DataLoadPort = {
    loadPorts.addRet(LoadPortSpec(DataLoadPort(
      preTranslationWidth  = preTranslationWidth,
      postTranslationWidth = postTranslationWidth,
      dataWidth     = cpuDataWidth,
      refillCount   = refillCount,
      rspAt         = loadRspAt,
      translatedAt  = loadTranslatedAt
    ))).port
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
      loadTranslatedAt = loadTranslatedAt,
      loadReadAt       = loadReadAt,
      loadHitsAt       = loadHitsAt,
      loadHitAt        = loadHitAt,
      loadBankMuxesAt  = loadBankMuxesAt,
      loadBankMuxAt    = loadBankMuxAt,
      loadControlAt    = loadControlAt,
      loadRspAt        = loadRspAt,
      storeReadAt      = storeReadAt,
      storeHitsAt      = storeHitsAt,
      storeHitAt       = storeHitAt,
      storeControlAt   = storeControlAt,
      storeRspAt       = storeRspAt,
      reducedBankWidth = reducedBankWidth
    )

    setup.lockPort <> cache.io.lock
    setup.refillEvent.map(_ := RegNext(cache.io.refillEvent) init(False))
    setup.writebackEvent.map(_ := RegNext(cache.io.writebackEvent) init(False))

    setup.refillCompletions := cache.io.refillCompletions

    val load = new Area{
      assert(loadPorts.size == 1)//for now, dev
      cache.io.load <> loadPorts.head.port
    }

    val store = new Area{
      assert(storePorts.size == 1)
      cache.io.store <> storePorts.head.port
    }
  }

  val mem = create late logic.cache.io.mem.toIo()

}
