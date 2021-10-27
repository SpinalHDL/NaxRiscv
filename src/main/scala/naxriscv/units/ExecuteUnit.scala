package naxriscv.units

import naxriscv.interfaces.{DecoderService, Encoding, ExecuteUnitService}
import naxriscv.utilities.Plugin

class ExecuteUnit(euId : Any) extends Plugin with ExecuteUnitService{

  override def uniqueIds = List(euId)

  override def hasFixedLatency = ???

  override def getFixedLatency = ???

  override def getIssuePort() = ???

  override def addFunction(enc: Encoding) = {
    val decoder = getService[DecoderService]()
    decoder.addFunction(this, enc)
  }

}
