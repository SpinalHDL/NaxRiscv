package naxriscv.units.lsu

import naxriscv.Frontend.{DISPATCH_COUNT, ROB_ID}
import naxriscv.backend.RobPlugin
import naxriscv.{Frontend, Global}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable.ArrayBuffer

case class LsuLoadPort(lqSize : Int, wordWidth : Int) extends Bundle {
  val id = UInt(log2Up(lqSize) bits)
  val address = UInt(Global.XLEN bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
  val unsigned = Bool()
}

case class LsuStorePort(sqSize : Int, wordWidth : Int) extends Bundle {
  val id = UInt(log2Up(sqSize) bits)
  val address = UInt(Global.XLEN bits)
  val data = Bits(wordWidth bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
}


class LsuQueuePlugin(lqSize: Int,
                     sqSize : Int) extends Plugin with LockedImpl{

  val wordWidth = Global.XLEN.get
  val wordBytes = wordWidth/8
  val pageOffsetRange = 11 downto log2Up(wordBytes)
  val pageNumberRange = Global.XLEN.get-1 downto 12
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

  val keys = new Area{
    val SQ_ALLOC = Stageable(Bool())
    val LQ_ALLOC = Stageable(Bool())
    val LSU_ID = Stageable(UInt(log2Up(lqSize max sqSize) bits))
    def LQ_ID = LSU_ID
    def SQ_ID = LSU_ID
  }.setName("")

  val setup = create early new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    rob.retain()
    decoder.retain()
    frontend.retain()

    decoder.addResourceDecoding(naxriscv.interfaces.LQ, keys.LQ_ALLOC)
    decoder.addResourceDecoding(naxriscv.interfaces.SQ, keys.SQ_ALLOC)
  }

  val logic = create late new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
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
//          val source = Reg(???)
//          val arg = Reg(???)
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

      val ptr = new Area{
        val alloc, commit, free = Reg(UInt(log2Up(sqSize) + 1 bits)) init (0)
        def isFull(ptr : UInt) = (ptr ^ free) === sqSize
//        val full = ??? //(alloc ^ free) === sqSize
//        val empty = alloc === commit
//        val canFree = free =/= commit
      }

      val frontend = getService[FrontendPlugin]
      val allocate = new Area{
        val stage = frontend.pipeline.allocated
        import stage._

        val full = False
        stage.haltIt(full)

        var free = CombInit(ptr.free)
        for(slotId <- 0 until Frontend.DISPATCH_COUNT){
          when((keys.LQ_ALLOC, slotId)){
            when(ptr.isFull(free)){
              full := True
            }
            free \= free + 1
          }

          (keys.LSU_ID, slotId) := free
          when(stage.isFireing && (keys.LQ_ALLOC, slotId)){
            regs.map(_.address.valid).write(free, False)
          }
        }

        when(stage.isFireing){
          ptr.free := free
        }


        def remapped[T <: Data](key : Stageable[T]) : Seq[T] = (0 until Frontend.DISPATCH_COUNT).map(stage(key, _))
        def writeLine[T <: Data](key : Stageable[T]) : Unit = writeLine(key, remapped(key))
        def writeLine[T <: Data](key : Stageable[T], value : Seq[T]) : Unit  = {
          rob.write(
            key = key,
            size = DISPATCH_COUNT,
            value = value,
            robId = stage(ROB_ID),
            enable = stage.isFireing
          )
        }
        writeLine(keys.LSU_ID)
      }

    }


//    val store = new Area{
//      val regs = for(i <- 0 until sqSize) yield new Area{
//        val id = i
//        val address = new Area{
//          val valid       = Reg(Bool())
//          val pageOffset  = Reg(UInt(pageOffsetWidth bits))
//        }
//        val data = new Area{
//          val valid = Reg(Bool())
//        }
//        val commited = Reg(Bool())
//      }
//
//      val mem = new Area{
//        val page = Mem.fill(sqSize)(UInt(pageNumberWidth bits))
//        val word = Mem.fill(sqSize)(Bits(wordWidth bits))
//      }
//
//      for(spec <- storePorts){
//        import spec._
//        when(port.valid){
//          mem.page.write(
//            enable = port.valid,
//            address = port.id,
//            data = port.address(pageNumberRange)
//          )
//          mem.word.write(
//            enable = port.valid,
//            address = port.id,
//            data = port.data
//          )
//          for(entry <- regs){
//            when(port.id === entry.id){
//              entry.address.valid := True
//              entry.address.pageOffset := port.address(pageOffsetRange)
//            }
//          }
//        }
//      }
//
//      val ptr = new Area{
//        val alloc, commit, free = Reg(UInt(log2Up(sqSize) + 1 bits)) init (0)
//        val full = ??? //(alloc ^ free) === sqSize
//        val empty = alloc === commit
//        val canFree = free =/= commit
//      }
//
//    }



    rob.release()
    decoder.release()
    frontend.release()
  }


}
