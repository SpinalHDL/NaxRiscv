package naxriscv.backend

import naxriscv.interfaces.{RegfileService, RegfileSpec}
import naxriscv.utilities.Plugin

class RegFilePlugin(spec : RegfileSpec,
                    physicalDepth : Int) extends Plugin with RegfileService{
  override def uniqueIds = List(spec)
  override def getPhysicalDepth = physicalDepth
}
