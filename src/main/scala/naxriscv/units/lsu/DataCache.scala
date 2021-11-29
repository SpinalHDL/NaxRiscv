package naxriscv.units.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.Pipeline

import scala.collection.mutable.ArrayBuffer

case class DataLoadPort(preTranslationWidth : Int,
                        physicalWidth : Int,
                        dataWidth : Int,
                        rspAt : Int,
                        translatedAt : Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataLoadCmd(preTranslationWidth, dataWidth))
  val translated = DataLoadTranslated(physicalWidth)
  val cancels = Bits(rspAt+1 bits)
  val rsp = Flow(DataLoadRsp(dataWidth)) //The rsp.valid is fondamentaly necessary, as it has a fixed latency

  override def asMaster() = {
    master(cmd)
    out(translated)
    out(cancels)
    slave(rsp)
  }
}

case class DataLoadCmd(preTranslationWidth : Int, dataWidth : Int) extends Bundle {
  val virtual = UInt(preTranslationWidth bits)
  val size = UInt(log2Up(log2Up(dataWidth/8)+1) bits)
}

case class DataLoadTranslated(physicalWidth : Int) extends Bundle {
  val physical   = UInt(physicalWidth bits)
  val peripheral = Bool()
}

case class DataLoadRsp(dataWidth : Int) extends Bundle {
  val data = Bits(dataWidth bits)
  val fault = Bool()
  val miss = Bool()
}

case class DataStorePort(physicalWidth: Int,
                         dataWidth: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataStoreCmd(physicalWidth, dataWidth))
  val rsp = Flow(NoData)

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}
case class DataStoreCmd(physicalWidth: Int,
                        dataWidth: Int) extends Bundle {
  val address = UInt(physicalWidth bits)
  val data = Bits(dataWidth bits)
  val mask = Bits(dataWidth/8 bits)
}

case class DataMemCmd(addressWidth: Int,
                      dataWidth: Int) extends Bundle {
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)
  val mask = Bits(dataWidth/8 bits)
}

case class DataMemRsp(dataWidth: Int) extends Bundle {
  val data = Bits(dataWidth bits)
  val error = Bool()
}

case class DataMemBus(addressWidth: Int,
                      dataWidth: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataMemCmd(addressWidth, dataWidth))
  val rsp = Flow(DataMemRsp(dataWidth))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}


class DataCache(val cacheSize: Int,
                val wayCount: Int,
                val memDataWidth: Int,
                val cpuDataWidth: Int,
                val preTranslationWidth: Int,
                val physicalWidth: Int,
                val lineSize: Int = 64,
                val loadReadAt: Int = 0,
                val loadHitsAt: Int = 1,
                val loadHitAt: Int = 1,
                val loadBankMuxesAt: Int = 1,
                val loadBankMuxAt: Int = 2,
                val loadControlAt: Int = 2,
                val loadRspAt: Int = 2) extends Component {
  val io = new Bundle {
    val load = slave(DataLoadPort(
      preTranslationWidth  = preTranslationWidth,
      physicalWidth = physicalWidth,
      dataWidth     = cpuDataWidth,
      rspAt         = loadRspAt,
      translatedAt  = loadHitsAt
    ))
    val store = slave(Stream(DataStoreCmd(
      physicalWidth = physicalWidth,
      dataWidth     = cpuDataWidth
    )))
    val mem = master(DataMemBus(physicalWidth, memDataWidth))
  }

  val pipeline = new Pipeline{
    val fetches = Array.fill(loadRspAt+1)(newStage())
    connect(fetches)(List(M2S()))

  }
  pipeline.build()

  io.flatten.filter(_.isOutput).map(_.assignDontCare())
}