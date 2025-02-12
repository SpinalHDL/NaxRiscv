package naxriscv.platform

import scala.collection.mutable.Queue

import java.util.Arrays
import java.io.{PrintWriter, FileWriter, File}
import java.io.IOException

import spinal.core._
import spinal.lib.misc.test

import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim.{Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionD}
import spinal.core.sim._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

trait TestSchedule {
  def activate(): Unit
}

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

  val testScheduleQueue: Queue[TestSchedule] = Queue()
  val customCin: Queue[Char] = Queue()
  var putcHistory: String = ""
  var putcTarget: String = null.asInstanceOf[String]

  mei #= false
  sei #= false

def testScheduleQueueNext(): Unit = {
    if (testScheduleQueue.isEmpty) return
    val e = testScheduleQueue.dequeue()
    e.activate()
}

class WaitPutc(val putc: String) extends TestSchedule {
  //println("TestSchedule: WaitPutc")
  def activate(): Unit = {
    putcTarget = putc
    //println("WaitPutc: "+putcTarget)
  }
}

class DoSuccess(filePassPath: String, passTests: ArrayBuffer[String]) extends TestSchedule {
  //println("TestSchedule: DoSuccess")
  def activate(): Unit = {
    // Create the “PASS” file
    val passFile = new File(filePassPath, "PASS")
    println("PASS FILE " + passFile)
    passFile.createNewFile()
    passTests += "buildroot_" + passFile.getParentFile.getName
    simSuccess()
  }
}

class DoGetc(val getc: String) extends TestSchedule {
  //println("TestSchedule: DoGetc")
  def activate(): Unit = {
    getc.foreach(e => customCin.enqueue(e))
    customCin.enqueue('\n')
    testScheduleQueueNext()
  }
}

def endsWith(value: String, ending: String): Boolean = {
  if (ending.length > value.length) {
    false
  } else {
    value.takeRight(ending.length).reverseIterator.zip(ending.reverseIterator).forall {
      case (v, e) => v == e
    }
  }
}
  override def onA(a: TransactionA) = {
    val d = TransactionD(a)
    a.opcode match {
      case Opcode.A.PUT_FULL_DATA => {
        d.opcode = Opcode.D.ACCESS_ACK
        a.address.toInt match {
          case PUTC => {
            putcHistory += a.data(0).toChar
            print(a.data(0).toChar)
            if (putcTarget != null) {
              if (endsWith(putcHistory, putcTarget)) {
                putcTarget = null
                testScheduleQueueNext()
                putcHistory = ""
              }
            }
          }
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
            } 
            else if (!customCin.isEmpty) {
              val customCinFront = customCin.dequeue()
              for(i <- 0 until d.bytes) d.data(i) = 0x0.toByte
              //java.util.Arrays.fill(d.data, 0, d.bytes)
              d.data(0) = customCinFront.toByte
            }
            else {
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

