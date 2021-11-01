package naxriscv.backend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{RobCompletion, RobLineMask, RobService}
import naxriscv.pipeline.Stageable
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class RobPlugin() extends Plugin with RobService{

//  override val ROB_DEPENDENCY = Stageable(UInt(robDepth bits))
//  override def robDepth = depth
  case class Completion(bus : Flow[RobCompletion])
  val completions = ArrayBuffer[Completion]()

  val robLineMaskPort = ArrayBuffer[RobLineMask]()

  override def robLineValids() = { val e = RobLineMask(); robLineMaskPort += e; e }
  override def robCompletion() = { val c = Completion(Flow(RobCompletion())); completions += c; c.bus }

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
    frontend.release()
  }

}
