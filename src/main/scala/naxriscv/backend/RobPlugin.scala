package naxriscv.backend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{RobCompletion, RobLineMask, RobService}
import naxriscv.utilities.Plugin
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

  override def robLineValids() = { val e = RobLineMask(); robLineMaskPort += e; e }
  override def robCompletion() = { val c = Completion(Flow(RobCompletion())); completions += c; c.bus }

  case class WriteLine(key: HardType[Data], size : Int, value: Seq[Data], robId: UInt, enable: Bool)
  case class ReadAsyncLine(key: HardType[Data], size : Int, robId: UInt, rsp : Vec[Data])

  val writesLine = ArrayBuffer[WriteLine]()
  val readAsyncsLine = ArrayBuffer[ReadAsyncLine]()

  override def writeLine[T <: Data](key: HardType[T], size : Int, value: Seq[T], robId: UInt, enable: Bool) = {
    writesLine += WriteLine(key.asInstanceOf[HardType[Data]], size, value, robId, enable)
  }
  override def readAsyncLine[T <: Data](key: HardType[T], size : Int, robId: UInt) : Vec[T] = {
    val r = ReadAsyncLine(key.asInstanceOf[HardType[Data]], size, robId, Vec.fill(COMMIT_COUNT)(key()))
    readAsyncsLine += r
    r.rsp.asInstanceOf[Vec[T]]
  }

  override def readAsync[T <: Data](key: HardType[T], robId: UInt, colFactor: Int, colOffset: Int) = ???

  val lock = Lock()
  override def retain() = lock.retain()
  override def release() = lock.release()

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }


  val logic = create late new Area{
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


    lock.await()

    val storage = new Area{
      val keys = mutable.LinkedHashSet[HardType[Data]]()
      keys ++= readAsyncsLine.map(_.key)
      val e = for(key <- keys) yield new Area{
        val wl = writesLine.filter(_.key == key)
        val ral = readAsyncsLine.filter(_.key == key)
        val writeSizeMin = wl.map(_.size).min
        val ram = Mem.fill(ROB.SIZE/writeSizeMin)(Vec.fill(writeSizeMin)(key()))
        assert(wl.size == 1)
        for(e <- wl){
          assert(isPow2(e.size))
          ram.write(
            enable = e.enable,
            address = e.robId >> log2Up(e.size),
            data = Vec(e.value)
          )
        }

        for(e <- ral){
          assert(isPow2(e.size))
          assert(e.size == writeSizeMin)
          e.rsp := ram.readAsync(
            address = e.robId >> log2Up(e.size)
          )
        }
      }
    }


    frontend.release()
  }

}
