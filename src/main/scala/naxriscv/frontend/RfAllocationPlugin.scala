package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{RegfileService, RegfileSpec, RfAllocationService, Riscv}
import naxriscv.utilities.{AllocatorMultiPortMem, Plugin}
import spinal.core._

import scala.collection.mutable.ArrayBuffer


class RfAllocationPlugin(rf : RegfileSpec) extends Plugin with RfAllocationService{
  override def getAllocPort() = logic.allocator.io.pop
  override def getFreePort() = logic.allocator.io.push
  var freePorts = ArrayBuffer

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val decoder = getService[DecoderPlugin]
    val stage = frontend.pipeline.renamed
    import stage._

    val entryCount = getService[RegfileService](rf).getPhysicalDepth
    val entryIdWidth = log2Up(entryCount)
    val entryType = HardType(UInt(entryIdWidth bits))

    val allocator = new AllocatorMultiPortMem(
      dataType = entryType,
      depth = entryCount,
      pushCount = Global.COMMIT_COUNT,
      popCount = Frontend.DISPATCH_COUNT
    )

    val pop = new Area {
      val blocked = !allocator.io.pop.ready
      haltIt(blocked)

      allocator.io.pop.fire := stage.isFireing
      for (slotId <- 0 until DISPATCH_COUNT) {
        allocator.io.pop.mask(slotId) := (DISPATCH_MASK, slotId) && (decoder.WRITE_RD, slotId)
        (decoder.PHYSICAL_RD, slotId) := allocator.io.pop.values(slotId)
      }
    }

    frontend.release()
  }
}
