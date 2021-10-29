package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{RegfileService, RegfileSpec, RfAllocationService}
import naxriscv.utilities.{AllocatorMultiPortMem, Plugin}
import spinal.core._

import scala.collection.mutable.ArrayBuffer


class RfAllocationPlugin(rf : RegfileSpec) extends Plugin with RfAllocationService{
  override def getAllocPort() = logic.allocator.io.pop
  override def getFreePort() = logic.allocator.io.push
  var freePorts = ArrayBuffer

  val logic = create late new Area{
    val entryCount = getService[RegfileService](rf).getPhysicalDepth
    val entryIdWidth = log2Up(entryCount)
    val entryType = HardType(UInt(entryIdWidth bits))
    val allocator = new AllocatorMultiPortMem(
      dataType = entryType,
      depth = entryCount,
      pushCount = Global.COMMIT_COUNT,
      popCount = Frontend.DISPATCH_COUNT
    )
  }
}
