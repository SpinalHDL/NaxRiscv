package naxriscv.units.lsu

import naxriscv.Global
import naxriscv.Global.XLEN
import naxriscv.interfaces.{AddressTranslationService, LockedImpl}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.lib.pipeline.Pipeline

import scala.collection.mutable.ArrayBuffer



class DataCachePlugin(val memDataWidth : Int,
                      val cacheSize: Int,
                      val wayCount: Int,
                      val refillCount : Int,
                      val lineSize: Int = 64,
                      val loadReadAt: Int = 0,
                      val loadHitsAt: Int = 1,
                      val loadHitAt: Int = 1,
                      val loadBankMuxesAt: Int = 1,
                      val loadBankMuxAt: Int = 2,
                      val loadControlAt: Int = 2,
                      val loadRspAt: Int = 2) extends Plugin with LockedImpl{
  def loadRspLatency = loadRspAt

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
      translatedAt  = loadHitsAt
    ))).port
  }

  def refillCompletions = setup.refillCompletions

  val setup = create early new Area{

    val doc = getService[DocPlugin]
    doc.property("DATA_MEM_DATA_BITS", memDataWidth)
    doc.property("DATA_LINE_BYTES", lineSize)

    val refillCompletions = Bits(refillCount bits)
  }

  val logic = create late new Area{
    lock.await()


    val cache = new DataCache(
      cacheSize       = cacheSize,
      wayCount        = wayCount,
      memDataWidth    = memDataWidth,
      cpuDataWidth    = cpuDataWidth,
      refillCount     = refillCount,
      preTranslationWidth    = preTranslationWidth,
      postTranslationWidth   = postTranslationWidth,
      lineSize        = lineSize,
      loadReadAt      = loadReadAt,
      loadHitsAt      = loadHitsAt,
      loadHitAt       = loadHitAt,
      loadBankMuxesAt = loadBankMuxesAt,
      loadBankMuxAt   = loadBankMuxAt,
      loadControlAt   = loadControlAt,
      loadRspAt       = loadRspAt
    )

    setup.refillCompletions := cache.io.refillCompletions

    val load = new Area{
      assert(loadPorts.size == 1)//for now, dev
//        val cmd = loadPorts.head.port.cmd
      cache.io.load <> loadPorts.head.port
    }


  }

  val mem = create late logic.cache.io.mem.toIo()

}
