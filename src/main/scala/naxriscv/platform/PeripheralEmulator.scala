package naxriscv.platform

import spinal.core._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim.{Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionD}
import spinal.core.sim._

class PeripheralEmulator(bus : tilelink.Bus, cd : ClockDomain) extends MonitorSubscriber{
  val monitor = new Monitor(bus, cd).add(this)
  val driver = new SlaveDriver(bus, cd)

  val PUTC = 0
  val PUT_HEX = 0x8
  val CLINT_BASE = 0x10000
  val CLINT_TIME = 0x0BFF8
  val MACHINE_EXTERNAL_INTERRUPT_CTRL = 0x10
  val SUPERVISOR_EXTERNAL_INTERRUPT_CTRL = 0x18
  val GETC = 0x40
  val STATS_CAPTURE_ENABLE = 0x50
  val PUT_DEC = 0x60
  val INCR_COUNTER = 0x70
  val FAILURE_ADDRESS = 0x80

  override def onA(a: TransactionA) = {
    a.address.toInt match {
      case PUTC => print(a.data(0).toChar)
      case _ => {
        println(a)
        simFailure()
      }
    }

    if(a.opcode == Opcode.A.PUT_FULL_DATA) {
      val d = TransactionD(a)
      d.opcode = Opcode.D.ACCESS_ACK
      driver.scheduleD(d)
    }
  }
}

