package naxriscv.units.lsu

import naxriscv.Global
import naxriscv.interfaces.LockedImpl
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

case class LsuLoadPort(lqSize : Int, wordWidth : Int) extends Bundle {
  val id = UInt(log2Up(lqSize) bits)
  val address = UInt(Global.XLEN bits)
  val size = Bits(2 bits)
}

case class LsuStorePort(sqSize : Int, wordWidth : Int) extends Bundle {
  val id = UInt(log2Up(sqSize) bits)
  val address = UInt(Global.XLEN bits)
  val data = Bits(wordWidth bits)
  val size = Bits(2 bits)
}

class LsuQueuePlugin() extends Plugin with LockedImpl{
  val lqSize = 16
  val sqSize = 16
  val wordWidth = Global.XLEN.get
  val wordBytes = wordWidth/8
  val pageOffsetRange = 11 downto log2Up(wordBytes)
  val pageNumberRange = Global.XLEN.get downto 12
  val pageOffsetWidth = pageOffsetRange.size
  val pageNumberWidth = pageNumberRange.size

  case class StorePortSpec(port : Flow[LsuStorePort])
  val storePorts = ArrayBuffer[StorePortSpec]()
  def newStorePort(): Flow[LsuStorePort] = {
    storePorts.addRet(StorePortSpec(Flow(LsuStorePort(sqSize, wordWidth)))).port
  }

  case class LoadPortSpec(port : Flow[LsuLoadPort])
  val loadPorts = ArrayBuffer[LoadPortSpec]()
  def newLoadPort(): Flow[LsuLoadPort] = {
    loadPorts.addRet(LoadPortSpec(Flow(LsuLoadPort(sqSize, wordWidth)))).port
  }


  val setup = create early new Area{

  }

  val logic = create late new Area{
    lock.await()

    val load = new Area{
      val regs = for(i <- 0 until lqSize) yield new Area{
        val id = i
        val address = new Area {
          val valid = Reg(Bool())
          val pageOffset = Reg(UInt(pageOffsetWidth bits))
        }
        val event = new Area{
          val valid = Reg(Bool())
          val source = Reg(???)
          val arg = Reg(???)
        }
        val executed = Reg(Bool())
      }
      val mem = new Area{
        val page = Mem.fill(sqSize)(UInt(pageNumberWidth bits))
      }
      for(spec <- loadPorts){
        import spec._
        when(port.valid){
          mem.page.write(
            enable = port.valid,
            address = port.id,
            data = port.address(pageNumberRange)
          )

          for(entry <- regs){
            when(port.id === entry.id){
              entry.address.valid := True
              entry.address.pageOffset := port.address(pageOffsetRange)
            }
          }
        }
      }
    }


    val store = new Area{
      val regs = for(i <- 0 until sqSize) yield new Area{
        val id = i
        val address = new Area{
          val valid       = Reg(Bool())
          val pageOffset  = Reg(UInt(pageOffsetWidth bits))
        }
        val data = new Area{
          val valid = Reg(Bool())
        }
        val commited = Reg(Bool())
      }

      val mem = new Area{
        val page = Mem.fill(sqSize)(UInt(pageNumberWidth bits))
        val word = Mem.fill(sqSize)(Bits(wordWidth bits))
      }

      for(spec <- storePorts){
        import spec._
        when(port.valid){
          mem.page.write(
            enable = port.valid,
            address = port.id,
            data = port.address(pageNumberRange)
          )
          mem.word.write(
            enable = port.valid,
            address = port.id,
            data = port.data
          )
          for(entry <- regs){
            when(port.id === entry.id){
              entry.address.valid := True
              entry.address.pageOffset := port.address(pageOffsetRange)
            }
          }
        }
      }

      val ptr = new Area{
        val alloc, commit, free = Reg(UInt(log2Up(sqSize) + 1 bits)) init (0)
        val full = ??? //(alloc ^ free) === sqSize
        val empty = alloc === commit
        val canFree = free =/= commit
      }

    }




  }


}
