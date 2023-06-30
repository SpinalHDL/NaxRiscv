// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{CommitService, InitCycles, RegfileService, RegfileSpec, RfAllocationService, RobService}
import naxriscv.utilities.{AllocatorMultiPortMem, Plugin}
import spinal.core._
import spinal.lib.pipeline.StageableOffset

import scala.collection.mutable.ArrayBuffer


class RfAllocationPlugin(var rf : RegfileSpec,
                         var pessimisticReady : Boolean = true) extends Plugin with RfAllocationService with InitCycles {
  withPrefix(rf.getName())

  override def initCycles = logic.entryCount

  override def getAllocPort() = logic.allocator.io.pop
  override def getFreePort() = ??? //logic.push.external

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
    getService[RobService].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val decoder = getService[DecoderPlugin]
    val commit = getService[CommitService]
    val rob = getService[RobService]
    val stage = frontend.pipeline.allocated
    import stage._

    val entryCount = findService[RegfileService](_.rfSpec == rf).getPhysicalDepth
    val entryIdWidth = log2Up(entryCount)
    val entryType = HardType(UInt(entryIdWidth bits))

    val allocator = new AllocatorMultiPortMem(
      dataType = entryType,
      depth = entryCount,
      pushCount = Global.COMMIT_COUNT,
      popCount = Frontend.DISPATCH_COUNT,
      pessimisticReady = pessimisticReady
    )
    Verilator.public(allocator.io)

    val pop = new Area {
      val blocked = !allocator.io.pop.ready
      haltIt(blocked)

      allocator.io.pop.fire := stage.isFireing
      for (slotId <- 0 until DISPATCH_COUNT) {
        implicit val offset = StageableOffset(slotId)
        val hit = decoder.REGFILE_RD === decoder.REGFILE_RD.rfToId(rf)
        allocator.io.pop.mask(slotId) := DISPATCH_MASK && decoder.WRITE_RD && hit
        if(!decoder.PHYS_RD.hasAssignement) decoder.PHYS_RD.assignDontCare()
        when(hit) {
          decoder.PHYS_RD := allocator.io.pop.values(slotId)
        }
      }
    }

    val push = new Area {
      val event         = commit.freePort()
      val mask          = rob.readAsync(DISPATCH_MASK, COMMIT_COUNT, event.robId)
      val writeRd       = rob.readAsync(decoder.WRITE_RD, COMMIT_COUNT, event.robId)
      val regfileRd     = rob.readAsync(decoder.REGFILE_RD, COMMIT_COUNT, event.robId)
      val physicalRdNew = rob.readAsync(decoder.PHYS_RD, COMMIT_COUNT, event.robId)
      val physicalRdOld = rob.readAsync(decoder.PHYS_RD_FREE, COMMIT_COUNT, event.robId)
      for (slotId <- 0 until Global.COMMIT_COUNT) {
        allocator.io.push(slotId).valid := event.valid && mask(slotId) && writeRd(slotId) && regfileRd(slotId) === decoder.REGFILE_RD.rfToId(rf)
        allocator.io.push(slotId).payload := event.commited(slotId) ? physicalRdOld(slotId) | physicalRdNew(slotId)

        //Protect 0
        if(rf.x0AlwaysZero) when(allocator.io.push(slotId).payload === 0) {
          allocator.io.push(slotId).valid := False
        }
      }
    }


    val init = new Area {
      assert(isPow2(entryCount))
      val counter = Reg(UInt(log2Up(entryCount*2) bits)) init (if(rf.x0AlwaysZero) 1 else rf.sizeArch)
      val busy = !counter.msb
      
      when(busy) {
        val port = allocator.io.push(0)
        port.valid := True
        port.payload := counter.resized

        counter := counter + 1
      }
    }

    frontend.release()
    rob.release()
  }
}
