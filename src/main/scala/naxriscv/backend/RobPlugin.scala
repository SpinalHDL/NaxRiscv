package naxriscv.backend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{RobCompletion, RobLineMask, RobService}
import naxriscv.pipeline.Stageable
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class RobPlugin() extends Plugin with RobService{

//  override val ROB_DEPENDENCY = Stageable(UInt(robDepth bits))
//  override def robDepth = depth
  case class Completion(bus : Stream[RobCompletion])
  val completions = ArrayBuffer[Completion]()

  val robLineMaskPort = ArrayBuffer[RobLineMask]()

  override def robPushLine() = ???
  override def robPopLine() = ???
  override def robLineValids() = { val e = RobLineMask(); robLineMaskPort += e; e }

  override def robCompletion() = {
    val c = Completion(Stream(RobCompletion()))
    completions += c
    c.bus
  }

  val logic = create late new Area{
    val lineCount = ROB.SIZE/ROB.ROWS
    val line = for(lineId <- 0 until lineCount) yield new Area{
      val row = for(rowId <- 0 until ROB.ROWS) yield new Area{
        val valid = RegInit(False)
      }
    }

    for(p <- robLineMaskPort){
      p.mask := line.map(l => Cat(l.row.map(_.valid))).read(p.line(ROB.lineRange))
    }
  }

}
