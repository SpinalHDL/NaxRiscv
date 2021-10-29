package naxriscv.backend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{RobCompletion, RobService}
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

  override def robPushLine() = ???

  override def robPopLine() = ???

  override def robCompletion() = {
    val c = Completion(Stream(RobCompletion()))
    completions += c
    c.bus
  }

  val logic = create late new Area{
    val lineCount = ROB.SIZE/ROB.ROWS
    val line = for(lineId <- 0 until lineCount){
      val row = for(rowId <- 0 until ROB.ROWS){
        val valid = RegInit(False)
      }
    }
  }
}
