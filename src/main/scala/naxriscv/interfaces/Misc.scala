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

case class JumpCmd() extends Bundle{
  val pc = Global.PC()
}
trait JumpService{
  def createJumpInterface(priority : Int = 0) : Flow[JumpCmd] //High priority win
}

trait DecoderService{
  def add(key : MaskedLiteral,values : Seq[(Stageable[_ <: BaseType],Any)])
  def add(encoding :Seq[(MaskedLiteral,Seq[(Stageable[_ <: BaseType],Any)])])
  def addDefault(key : Stageable[_ <: BaseType], value : Any)
}

trait RobService{
  def robPushLine() : Stream[RobPushLine]
  def robPopLine() : Stream[RobPopLine]
  def robCompletion() : Stream[RobCompletion]
}

trait RegfileIdentifier{
  def regfileId : RegfileConfig
}

trait AllocationService extends RegfileIdentifier{
  def newAllocPort() : Stream[UInt]
  def newFreePort() : Flow[UInt]
}

trait RenamerService extends RegfileIdentifier {
  def newTranslationPort() : Any
  def rollbackToCommit() : Unit
}

trait RegfileService extends RegfileIdentifier{
  def newRead() : Any
  def newWriteFlow() : Unit
  def newWriteStream() : Unit
}

trait RegfileConfig{
  def size : Int
  def width : Int
  def x0AlwaysZero : Boolean
}

trait WakeService {
  def newWakeOhPort() : Stream[WakeOh]
  def newWakeRobIdPort() : Stream[WakeRobId]
}

trait CommitService {
  def onCommit() : Vec[Flow[CommitEntry]]
  def newWakeRobIdPort() : Stream[WakeRobId]
}


object Riscv{
  val intRegfile = new RegfileConfig {
    override def size = 32
    override def width = ???
    override def x0AlwaysZero = true
  }
  val floatRegfile = new RegfileConfig {
    override def size = 32
    override def width = ???
    override def x0AlwaysZero = true
  }
}

case class RobCompletion() extends Bundle {
  val id = UInt(Global.ROB_ID_WIDTH bits)
}
case class RobPushLine() extends Bundle {
  val id = UInt(Global.ROB_ID_WIDTH bits)
  val entries = Vec.fill(Global.ROB_ROWS)(RobPushEntry())
}
case class RobPushEntry() extends Bundle{
  val commitTask = NoData
}

case class RobPopLine() extends Bundle {
  val id = UInt(Global.ROB_ID_WIDTH bits)
  val entries = Vec.fill(Global.ROB_ROWS)(RobPopEntry())
}
case class RobPopEntry() extends Bundle{
  val valid = Bool()
  val commitTask = NoData
}

case class WakeOh() extends Bundle{
  val oh = Bits()
}

case class WakeRobId() extends Bundle{
  val id = UInt()
}

case class CommitEntry() extends Bundle {
  val kind = ???
  val context = ???
}