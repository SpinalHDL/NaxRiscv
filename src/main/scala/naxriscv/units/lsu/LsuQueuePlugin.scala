package naxriscv.units.lsu

import naxriscv.Frontend.{DISPATCH_COUNT, ROB_ID}
import naxriscv.backend.RobPlugin
import naxriscv.{Frontend, Global, ROB}
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stageable}

import scala.collection.mutable.ArrayBuffer

object LsuUtils{
  def sizeWidth(wordWidth : Int) = log2Up(log2Up(wordWidth/8)+1)
}

case class LsuLoadPort(lqSize : Int, wordWidth : Int, physicalRdWidth : Int) extends Bundle {
  val robId = ROB.ID_TYPE()
  val lqId = UInt(log2Up(lqSize) bits)
  val address = UInt(Global.XLEN bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
  val unsigned = Bool()
  val physicalRd = UInt(physicalRdWidth bits)
}

case class LsuStorePort(sqSize : Int, wordWidth : Int) extends Bundle {
  val robId = ROB.ID_TYPE()
  val sqId = UInt(log2Up(sqSize) bits)
  val address = UInt(Global.XLEN bits)
  val data = Bits(wordWidth bits)
  val size = UInt(log2Up(log2Up(wordWidth/8)+1) bits)
}


class LsuQueuePlugin(lqSize: Int,
                     sqSize : Int) extends Plugin with LockedImpl with WakeRobService with WakeRegFileService {

  val wordWidth = Global.XLEN.get
  val wordBytes = wordWidth/8
  val wordSizeWidth = LsuUtils.sizeWidth(wordWidth)
  val pageOffsetRange = 11 downto log2Up(wordBytes)
  val pageNumberRange = Global.XLEN.get-1 downto 12
  val pageOffsetWidth = pageOffsetRange.size
  val pageNumberWidth = pageNumberRange.size

  def virtualAddressWidth = getService[AddressTranslationService].virtualWidth

  case class StorePortSpec(port : Flow[LsuStorePort])
  val storePorts = ArrayBuffer[StorePortSpec]()
  def newStorePort(): Flow[LsuStorePort] = {
    storePorts.addRet(StorePortSpec(Flow(LsuStorePort(sqSize, wordWidth)))).port
  }

  case class LoadPortSpec(port : Flow[LsuLoadPort])
  val loadPorts = ArrayBuffer[LoadPortSpec]()
  def newLoadPort(): Flow[LsuLoadPort] = {
    val physicalRdWidth = getService[DecoderService].PHYS_RD
    loadPorts.addRet(LoadPortSpec(Flow(LsuLoadPort(lqSize, wordWidth, widthOf(physicalRdWidth))))).port
  }


  override def wakeRobs = List(logic.get.load.pipeline.cacheRsp.wakeRob)
  override def wakeRegFile = List(logic.get.load.pipeline.cacheRsp.wakeRf)


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
    val cache = getService[DataCachePlugin]
    val regfile = getService[RegfileService]
    val commit = getService[CommitService]

    rob.retain()
    decoder.retain()
    frontend.retain()

    val rfWrite = regfile.newWrite(withReady = false, latency = 1)
    val cacheLoad = cache.newLoadPort()
    val loadCompletion = rob.newRobCompletion()
    val loadTrap = commit.newSchedulePort(canTrap = true, canJump = false)

    decoder.addResourceDecoding(naxriscv.interfaces.LQ, keys.LQ_ALLOC)
    decoder.addResourceDecoding(naxriscv.interfaces.SQ, keys.SQ_ALLOC)
  }

  val logic = create late new Area{
    val rob = getService[RobPlugin]
    val decoder = getService[DecoderService]
    val frontend = getService[FrontendPlugin]
    val cache = getService[DataCachePlugin]
    val commit = getService[CommitService]
    lock.await()

    val rescheduling = commit.reschedulingPort

    val lsuAllocationStage = frontend.pipeline.dispatch

    for(slotId <- 0 until Frontend.DISPATCH_COUNT){
      lsuAllocationStage(keys.LSU_ID, slotId).assignDontCare()
    }
    val load = new Area{
      val regs = for(i <- 0 until lqSize) yield new Area{
        val id = i
        val address = new Area {
          val valid = RegInit(False)
          val pageOffset = Reg(UInt(pageOffsetWidth bits))
          val size = Reg(UInt(wordSizeWidth bits))
        }
        val cacheMiss = RegInit(False)
        val tlbMiss = RegInit(False)
        val askCache = RegInit(False)

//        val cacheMiss = new Area{
//          val valid = Reg(Bool())
////          val source = Reg(???)
////          val arg = Reg(???)
//        }

        val clear = False
        when(clear){
          address.valid := False
          cacheMiss := False
          tlbMiss := False
          askCache := False
        }
      }
      val mem = new Area{
        val address = Mem.fill(lqSize)(UInt(virtualAddressWidth bits))
        val physRd = Mem.fill(lqSize)(decoder.PHYS_RD)
        val robId = Mem.fill(lqSize)(ROB.ID_TYPE)
      }
      for(spec <- loadPorts){
        import spec._
        when(port.valid) {
          mem.address.write(
            enable = port.valid,
            address = port.lqId,
            data = port.address
          )
          mem.physRd.write(
            enable = port.valid,
            address = port.lqId,
            data = port.physicalRd
          )
          mem.robId.write(
            enable = port.valid,
            address = port.lqId,
            data = port.robId
          )

          for (entry <- regs) when(port.lqId === entry.id) {
            entry.address.valid := True
            entry.address.pageOffset := port.address(pageOffsetRange)
            entry.address.size := port.size
            entry.askCache := True
          }
        }
      }

      val ptr = new Area{
        val alloc, commit, free = Reg(UInt(log2Up(lqSize) + 1 bits)) init (0)
        def isFull(ptr : UInt) = (ptr ^ free) === lqSize
        val priority = Reg(Bits(lqSize bits)) //TODO implement me
        free := 0
        priority := 0
        println("Please implement LSUQueuePlugin ptr.priority")
      }

      val frontend = getService[FrontendPlugin]
      val allocate = new Area{
        import lsuAllocationStage._

        val full = False
        haltIt(full)

        var alloc = CombInit(ptr.alloc)
        for(slotId <- 0 until Frontend.DISPATCH_COUNT){
//          when(isFireing && (keys.LQ_ALLOC, slotId)){
//            for(reg <- regs) when(alloc.dropHigh(1) === reg.id){
//              reg.clear := True
//            }
//          }

          when((keys.LQ_ALLOC, slotId)){
            when(ptr.isFull(alloc)){
              full := True
            }
            (keys.LSU_ID, slotId) := alloc.resized
            alloc \= alloc + 1
          }
        }

        when(isFireing){
          ptr.alloc := alloc
        }


        def remapped[T <: Data](key : Stageable[T]) : Seq[T] = (0 until Frontend.DISPATCH_COUNT).map(lsuAllocationStage(key, _))
        def writeLine[T <: Data](key : Stageable[T]) : Unit = writeLine(key, remapped(key))
        def writeLine[T <: Data](key : Stageable[T], value : Seq[T]) : Unit  = {
          rob.write(
            key = key,
            size = DISPATCH_COUNT,
            value = value,
            robId = ROB_ID,
            enable = isFireing
          )
        }
        writeLine(keys.LSU_ID)
      }

      val pipeline = new Pipeline{
        val stages = Array.fill(cache.loadRspLatency + 1)(newStage())
        connect(stages)(List(M2S()))

        val keys = new AreaRoot {
          val LQ_SEL = Stageable(UInt(log2Up(lqSize) bits))
          val LQ_SEL_OH = Stageable(Bits(lqSize bits))
          val VIRTUAL_ADDRESS = Stageable(UInt(virtualAddressWidth bits))
        }

        val feed = new Area{
          val hits = regs.map(_.askCache)
          val hit = hits.orR

          val slotsValid = B(hits)
          val doubleMask = slotsValid ## (slotsValid & ptr.priority)
          val doubleOh = OHMasking.firstV2(doubleMask, firstOrder =  LutInputs.get)
          val selOh = doubleOh.subdivideIn(2 slices).reduce(_ | _)
          val sel = OHToUInt(selOh)

          val cmd = setup.cacheLoad.cmd
          cmd.valid := hit
          cmd.virtual := mem.address.readAsync(sel)
          cmd.size := regs.map(_.address.size).read(sel)
          for(reg <- regs) when(selOh(reg.id) && cmd.ready){
            reg.askCache := False
          }

          val stage = stages.head
          import stage._

          isValid := cmd.fire
          keys.LQ_SEL := sel
          keys.LQ_SEL_OH := selOh
          decoder.PHYS_RD := mem.physRd.readAsync(sel)
          ROB.ID_TYPE := mem.robId.readAsync(sel)
          keys.VIRTUAL_ADDRESS := cmd.virtual
        }

        val cancels = for((stage, stageId) <- stages.zipWithIndex){
          setup.cacheLoad.cancels(stageId) := stage.isValid && rescheduling.valid
        }

        val cacheRsp = new Area{
          val stage = stages.last
          import stage._

          val rsp = setup.cacheLoad.rsp

          setup.rfWrite.valid   := False
          setup.rfWrite.address := decoder.PHYS_RD
          setup.rfWrite.data    := rsp.data

          setup.loadCompletion.valid := False
          setup.loadCompletion.id := ROB.ID_TYPE

          val wakeRob = Flow(WakeRob())
          wakeRob.valid := False
          wakeRob.robId := ROB.ID_TYPE

          val wakeRf = Flow(WakeRegFile(decoder.PHYS_RD, needBypass = false))
          wakeRf.valid := False
          wakeRf.physical := decoder.PHYS_RD

          setup.loadTrap.valid      := False
          setup.loadTrap.robId      := ROB.ID_TYPE
          setup.loadTrap.tval       := B(keys.VIRTUAL_ADDRESS)
          setup.loadTrap.skipCommit := True
          setup.loadTrap.cause.assignDontCare()

          when(isValid) {
            when(rsp.miss) {
              for(reg <- regs) when(keys.LQ_SEL_OH(reg.id)){
                reg.cacheMiss := True
              }
            } elsewhen (rsp.fault) {
              setup.loadTrap.valid := True
              setup.loadTrap.cause := 5
            } otherwise {
              setup.rfWrite.valid := True
              setup.loadCompletion.valid := True
              wakeRob.valid := True
              wakeRf.valid := True
            }
          }
        }
      }
      pipeline.build()
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



    when(rescheduling.valid){
      load.regs.foreach(_.clear := True)
    }
    rob.release()
    decoder.release()
    frontend.release()
  }


}
