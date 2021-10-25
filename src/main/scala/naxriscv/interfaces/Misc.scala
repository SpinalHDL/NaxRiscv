package naxriscv.interfaces

import spinal.core._
import spinal.lib._
import naxriscv.Global
import naxriscv.frontend.Frontend
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
  def regfileId : RegfileSpec
}

trait RfAllocationService extends RegfileIdentifier{
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


trait WakeService {
  def newWakeOhPort() : Stream[WakeOh]
  def newWakeRobIdPort() : Stream[WakeRobId]
}

trait CommitService {
  def onCommit() : Vec[Flow[CommitEntry]]
  def newCompletionPort() : Flow[CompletionCmd]
}

trait RegfileSpec{
  def size : Int
  def width : Int
  def x0AlwaysZero : Boolean

  def ->(access : RfAccess) = RfResource(this, access)
}

trait EncodingResource{

}

case class RfResource(rf : RegfileSpec, enc : RfAccess) extends EncodingResource


case class Encoding(
   key : MaskedLiteral,
   ressources : Seq[EncodingResource]
)

//trait FunctionalUnitDatabase {
////  def retain() : Unit
////  def release() : Unit
//  def register(fu)
//}

trait FunctionalUnitService{
  def getId : Any
  def hasFixedLatency : Boolean
  def getFixedLatency : Int
  def getIssuePort() : Unit
}


trait RfAccess {
  def decode(i : Bits) : UInt
}
trait RfRead extends RfAccess
trait RfWrite extends RfAccess



object Riscv{
  val RS1 = new RfRead{
    def decode(i : Bits) = i(19 downto 15).asUInt
  }
  val RS2 = new RfRead{
    def decode(i : Bits) = i(24 downto 20).asUInt
  }
  val RD = new RfWrite{
    def decode(i : Bits) = i(11 downto 7).asUInt
  }
  val integer = new Area{
    val regfile = new RegfileSpec {
      override def size = 32
      override def width = ???
      override def x0AlwaysZero = true
    }

    def TypeR(key : MaskedLiteral) = Encoding(
      key = key,
      ressources = List(RS1, RS2, RD).map(regfile -> _)
    )
    def TypeI(key : MaskedLiteral) = Encoding(
      key = key,
      ressources = List(RS1, RD).map(regfile -> _)
    )

    val ADD  = TypeR(M"0000000----------000-----0110011")
    val ADDI = TypeI(M"-----------------000-----0010011")
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

case class CompletionCmd(canTrap : Boolean, canJump : Boolean) extends Bundle {
  val robId = UInt(Global.ROB_ID_WIDTH bits)

  val jump = canJump generate Bool()
  val trap = canTrap generate Bool()
  val cause = canTrap generate UInt(Global.TRAP_CAUSE_WIDTH bits)
  val arg = (canTrap || canJump) generate Bits(Global.XLEN bits) //Target PC if jump, payload if trap
}