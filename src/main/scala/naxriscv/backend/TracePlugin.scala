package naxriscv.backend

import naxriscv.{Global, ROB}
import naxriscv.interfaces.{CommitService, DecoderService, RegfileService, RobService}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.IMasterSlave

case class TraceIoElement() extends Bundle {
  val robId = ROB.ID_TYPE()
  val pc = Global.PC()
  val write = Bool()
  val data = Bits(Global.XLEN bits)
}



class TracePlugin extends Plugin{

  val logic = create late new Area{
    val commit = new Area {
      val port = Vec.fill(Global.COMMIT_COUNT)(master(Flow(TraceIoElement())))
      val event = getService[CommitService].onCommit()
      val rob = getService[RobService]
      val decoder = getService[DecoderService]
      for ((slot, slotId) <- port.zipWithIndex) {
        slot.payload.setCompositeName(slot)
        slot.valid := event.mask(slotId)
        slot.robId := event.robId + slotId
        slot.pc := rob.readAsyncSingle(Global.PC, slot.robId)
      }
    }

    val rf = new Area{
      val writes = getService[RegfileService].getWrites()
      val port = writes.map(e => master(e.asWithoutReady()))
    }
  }
}
