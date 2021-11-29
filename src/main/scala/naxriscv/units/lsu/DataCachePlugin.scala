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
  def virtualWidth : Int = getService[AddressTranslationService].virtualWidth
  def physicalWidth : Int = getService[AddressTranslationService].physicalWidth

  case class LoadPortSpec(port : DataLoadPort)
  val loadPorts = ArrayBuffer[LoadPortSpec]()
  def newLoadPort(): DataLoadPort = {
    loadPorts.addRet(LoadPortSpec(DataLoadPort(
      virtualWidth  = virtualWidth,
      physicalWidth = physicalWidth,
      dataWidth     = cpuDataWidth,
      rspAt         = loadRspAt,
      translatedAt  = loadHitsAt
    ))).port
  }

  val setup = create early new Area{

    val doc = getService[DocPlugin]
    doc.property("DATA_MEM_DATA_BITS", memDataWidth)
    doc.property("DATA_LINE_BYTES", lineSize)
  }

  val logic = create late new Area{
    lock.await()


    val cache = new DataCache(
      cacheSize       = cacheSize,
      wayCount        = wayCount,
      memDataWidth    = memDataWidth,
      cpuDataWidth    = cpuDataWidth,
      virtualWidth    = virtualWidth,
      physicalWidth   = physicalWidth,
      lineSize        = lineSize,
      loadReadAt      = loadReadAt,
      loadHitsAt      = loadHitsAt,
      loadHitAt       = loadHitAt,
      loadBankMuxesAt = loadBankMuxesAt,
      loadBankMuxAt   = loadBankMuxAt,
      loadControlAt   = loadControlAt,
      loadRspAt       = loadRspAt
    )


    val load = new Area{
      assert(loadPorts.size == 1)//for now, dev
//        val cmd = loadPorts.head.port.cmd
      cache.io.load <> loadPorts.head.port
    }


  }

  val mem = create late logic.cache.io.mem.toIo()

}
