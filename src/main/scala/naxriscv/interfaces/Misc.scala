package naxriscv.interfaces

import spinal.core._
import spinal.lib._
import naxriscv.Global
import naxriscv.frontend.{Frontend}
import naxriscv.pipeline._

case class JumpPayload() extends Bundle {
  val pc = Global.PC()
  val branchHistory = Frontend.BRANCH_HISTORY()
}

object JumpService{
  object Priorities{
    val FETCH_WORD = 0
    val PREDICTOR = 100
    val COMMIT = 200
  }
}
trait JumpService{
  def createJumpInterface(priority : Int = 0) : Flow[UInt] //High priority win
}

case class FetchWord(predictorType : HardType[Data]) extends Bundle{
  val word = Frontend.WORD()
  val mask = Frontend.MASK()
  val branchHistory = Frontend.BRANCH_HISTORY()
  val predictor = predictorType()
}
trait FetchWordService{
  def getFetchWordStream() : Stream[FetchWord]
}
