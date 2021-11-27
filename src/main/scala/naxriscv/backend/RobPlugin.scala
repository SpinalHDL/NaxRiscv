package naxriscv.backend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{RobCompletion, RobLineMask, RobService}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.core.fiber.Lock
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RobPlugin() extends Plugin with RobService{

//  override val ROB_DEPENDENCY = Stageable(UInt(robDepth bits))
//  override def robDepth = depth
  case class Completion(bus : Flow[RobCompletion])
  val completions = ArrayBuffer[Completion]()

  val robLineMaskPort = ArrayBuffer[RobLineMask]()

  override def newRobLineValids() = { val e = RobLineMask(); robLineMaskPort += e; e }
  override def newRobCompletion() = { val c = Completion(Flow(RobCompletion())); completions += c; c.bus }

  case class Write(key: Stageable[Data], size : Int, value: Seq[Data], robId: UInt, enable: Bool)
  case class ReadAsync(key: Stageable[Data], size : Int, robId: UInt, skipFactor: Int, skipOffset: Int, rsp : Vec[Data])

  val writes = ArrayBuffer[Write]()
  val readAsyncs = ArrayBuffer[ReadAsync]()

  override def write[T <: Data](key: Stageable[T], size : Int, value: Seq[T], robId: UInt, enable: Bool) = {
    writes += Write(key.asInstanceOf[Stageable[Data]], size, value, robId, enable)
  }
  override def readAsync[T <: Data](key: Stageable[T], size : Int, robId: UInt, skipFactor: Int = 1, skipOffset: Int = 0) : Vec[T] = {
    val r = ReadAsync(key.asInstanceOf[Stageable[Data]], size, robId, skipFactor, skipOffset, Vec.fill(size)(key()))
    readAsyncs += r
    r.rsp.asInstanceOf[Vec[T]]
  }


  val lock = Lock()
  override def retain() = lock.retain()
  override def release() = lock.release()

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }


  val logic = create late new Area{
    lock.await()

    val lineCount = ROB.SIZE/ROB.COLS
    val valids = Reg(Bits(ROB.SIZE bits)) init(0)
    for(p <- robLineMaskPort){
      p.mask := valids.subdivideIn(ROB.COLS bits).read(p.line(ROB.lineRange))
    }
    for(c <- completions){
      when(c.bus.valid){
        valids(c.bus.id) := True
      }
    }

    val frontend = getService[FrontendPlugin]
    when(frontend.pipeline.allocated.isFireing){
      valids.subdivideIn(ROB.COLS bits).write(frontend.pipeline.allocated(ROB_ID) >> log2Up(ROB.COLS), B(0, ROB.COLS bits))
    }



    val storage = new Area{
      val keys = mutable.LinkedHashSet[Stageable[Data]]()
      keys ++= readAsyncs.map(_.key)
      val e = for(key <- keys) yield new Area{
        if(key.isNamed) this.setPartialName(key.getName())
        val wl = writes.filter(_.key == key)
        val ral = readAsyncs.filter(_.key == key)
        if(wl.isEmpty) SpinalError(s"RobPlugin has not writes for ${key}")
        val writeSizeMin = wl.map(_.size).min
        val writeSizeMax = wl.map(_.size).max
        val readSizeMin = ral.map(_.size).min
        val sizeMin = readSizeMin min writeSizeMin
        val bankCount = writeSizeMax/sizeMin
        val banks = Seq.fill(bankCount)(Mem.fill(ROB.SIZE/bankCount/sizeMin)(Vec.fill(sizeMin)(key())))

        for(e <- wl){
          assert(isPow2(e.size))
          val ratio = e.size / sizeMin
          val bankRange = log2Up(writeSizeMax) - 1 downto log2Up(e.size)
          val address =  e.robId >> log2Up(bankCount*sizeMin)
          val groupedData = e.value.grouped(sizeMin).toSeq
          for((bank, bankId) <- banks.zipWithIndex) {
            bank.write(
              enable = e.enable && e.robId(bankRange) === bankId/ratio,
              address = address,
              data = Vec(groupedData(bankId % groupedData.size))
            )
          }
        }

        for(e <- ral){
          assert(isPow2(e.size))
          if(e.size > writeSizeMax){
            ???
          } else if(e.skipFactor == 1){
            val address =  e.robId >> log2Up(bankCount*sizeMin)
            val reads = banks.map(_.readAsync(
              address = address
            ))
            val cat = Cat(reads)
            val sliceRange = log2Up(writeSizeMax) - 1 downto log2Up(e.size)
            e.rsp.assignFromBits(cat.subdivideIn(widthOf(e.rsp) bits).read(e.robId(sliceRange)))
          } else {
            assert(e.size == 1)
            val address =  e.robId >> log2Up(bankCount*sizeMin)
            val banksIds = (0 until bankCount).filter(v => v % e.skipFactor == e.skipOffset)
            val reads = banksIds.map(banks(_)).map(_.readAsync(
              address = address
            ))
            val cat = Cat(reads)
            val sliceRange = log2Up(writeSizeMax) - 1 downto log2Up(e.size*e.skipFactor)
            e.rsp.assignFromBits(cat.subdivideIn(widthOf(e.rsp) bits).read(e.robId(sliceRange)))
          }
        }
      }
    }


    getService[DocPlugin].property("ROB_SIZE", ROB.SIZE.get)
    frontend.release()
  }

}
