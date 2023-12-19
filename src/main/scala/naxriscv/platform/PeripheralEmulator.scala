package naxriscv.platform

import spinal.core._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim.{Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionD}
import spinal.core.sim._

import scala.util.Random

class PeripheralEmulator(bus : tilelink.Bus, mei : Bool, sei : Bool, cd : ClockDomain) extends MonitorSubscriber{
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
  val IO_FAULT_ADDRESS = 0x0FFFFFF0
  val RANDOM = 0xA8

  mei #= false
  sei #= false

  override def onA(a: TransactionA) = {
    val d = TransactionD(a)
    a.opcode match {
      case Opcode.A.PUT_FULL_DATA => {
        d.opcode = Opcode.D.ACCESS_ACK
        a.address.toInt match {
          case PUTC => print(a.data(0).toChar)
          case PUT_HEX => print(a.data.reverse.map(v => f"$v%02x").mkString(""))
          case MACHINE_EXTERNAL_INTERRUPT_CTRL => mei #= a.data(0).toBoolean
          case SUPERVISOR_EXTERNAL_INTERRUPT_CTRL => sei #= a.data(0).toBoolean
          case IO_FAULT_ADDRESS => {
            d.denied = true
          }
          case _ => {
            println(a)
            simFailure()
          }
        }
      }
      case Opcode.A.GET => {
        d.opcode = Opcode.D.ACCESS_ACK_DATA
        d.data = Array.fill(a.bytes)(0)
        a.address.toInt match {
          case IO_FAULT_ADDRESS => {
            d.denied = true
            simRandom.nextBytes(d.data)
          }
          case GETC => {
            if(System.in.available() != 0) {
              d.data(0) = System.in.read().toByte
            } else {
              for(i <- 0 until d.bytes) d.data(i) = 0xFF.toByte
            }
          }
          case RANDOM => {
            simRandom.nextBytes(d.data)
          }
          case _ => {
            println(a)
            simFailure()
          }
        }
      }
    }

    driver.scheduleD(d)
  }
}

