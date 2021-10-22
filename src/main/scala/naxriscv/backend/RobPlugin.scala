package naxriscv.backend

import naxriscv.Global
import naxriscv.interfaces.{RobCompletion, RobService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

//class RobPlugin extends Plugin with RobService{
//
//  case class Completion(bus : Stream[RobCompletion])
//  val completions = ArrayBuffer[Completion]()
//  override def createRobCompletion() = {
//    val c = Completion(Stream(RobCompletion()))
//    completions += c
//    c.bus
//  }
//
//  val logic = create late new Area{
//    val lineCount = Global.ROB_SIZE/Global.ROB_ROWS
//    val line = for(lineId <- 0 until lineCount){
//      val row = for(rowId <- 0 until Global.ROB_ROWS){
//        val valid = RegInit(False)
//      }
//    }
//  }
//}
