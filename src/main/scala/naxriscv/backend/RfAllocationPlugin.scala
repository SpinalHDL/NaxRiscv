package naxriscv.backend

import naxriscv.Global
import naxriscv.frontend.Frontend
import naxriscv.interfaces.{AllocationPort, RegfileSpec, RfAllocationService}
import naxriscv.utilities.{AllocatorMultiPortMem, Plugin}
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer


class RfAllocationPlugin(rf : RegfileSpec) extends Plugin with RfAllocationService{
  override def getAllocPort() = logic.allocator.io.pop
  override def getFreePort() = logic.allocator.io.push
  var freePorts = ArrayBuffer

  val logic = create late new Area{
    val entryCount = rf.sizePhys
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
