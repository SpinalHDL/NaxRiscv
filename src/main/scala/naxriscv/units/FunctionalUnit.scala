package naxriscv.units

import naxriscv.interfaces.{DecoderService, Encoding, FunctionalUnitService}
import naxriscv.utilities.Plugin

class FunctionalUnit(fuId : Any) extends Plugin with FunctionalUnitService{

  override def uniqueIds = List(fuId)

  override def hasFixedLatency = ???

  override def getFixedLatency = ???

  override def getIssuePort() = ???

  override def addFunction(enc: Encoding) = {
    val decoder = getService[DecoderService]()
    decoder.addFunction(this, enc)
  }

}
