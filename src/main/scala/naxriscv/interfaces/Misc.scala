package naxriscv.interfaces

import spinal.core._
import spinal.lib._
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import spinal.lib.pipeline._
import naxriscv.utilities.{AllocatorMultiPortPop, Service}
import spinal.lib.pipeline.Stageable

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
trait JumpService extends Service{
  def createJumpInterface(priority : Int = 0) : Flow[JumpCmd] //High priority win
}


case class EuGroup(eus : Seq[ExecuteUnitService],
                   sel: Stageable[Bool])

trait DecoderService extends Service{
//  def add(key : MaskedLiteral,values : Seq[(Stageable[_ <: BaseType],Any)])
//  def add(encoding :Seq[(MaskedLiteral,Seq[(Stageable[_ <: BaseType],Any)])])
//  def addDefault(key : Stageable[_ <: BaseType], value : Any)
  def addFunction(fu: ExecuteUnitService,
                  enc: Encoding) : Unit

  def euGroups : Seq[EuGroup]

  def READ_RS(id : Int)  : Stageable[Bool]
  def ARCH_RS(id : Int)  : Stageable[UInt]
  def PHYS_RS(id : Int)  : Stageable[UInt]

  def WRITE_RD : Stageable[Bool]
  def PHYS_RD  : Stageable[UInt]
  def ARCH_RD  : Stageable[UInt]

  def rsCount  : Int
  def rsPhysicalDepthMax : Int
}

trait RobService extends Service{
  def robCompletion() : Flow[RobCompletion]
  def robLineValids() : RobLineMask

  def writeLine[T <: Data](key: HardType[T], size : Int, value : Seq[T], robId : UInt, enable : Bool) : Unit //robid need to be aligned on value size
  def readAsyncLine[T <: Data](key: HardType[T], size : Int, robId : UInt) : Vec[T]
  def readAsync[T <: Data](key: HardType[T], robId : UInt, colFactor : Int = 1, colOffset : Int = 0) : Seq[T] //colFactor and colOffset may allow to reduce the port area

  def retain() : Unit
  def release() : Unit
}


case class RobLineMask() extends Bundle{
  val line = ROB.ID_TYPE()
  val mask = Bits(ROB.COLS bits)
}

trait RfAllocationService extends Service {
  def getAllocPort() : AllocatorMultiPortPop[UInt]
  def getFreePort() : Vec[Flow[UInt]]
}

//trait RenamerService extends Service {
//  def newTranslationPort() : Any
//  def rollbackToCommit() : Unit
//}


case class RegFileWrite(addressWidth : Int, dataWidth : Int, withReady : Boolean) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  override def asMaster() = {
    out(valid, address, data)
    inWithNull(ready)
  }
}

case class RegFileRead(addressWidth : Int, dataWidth : Int, withReady : Boolean, latency : Int) extends Bundle with IMasterSlave{
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  override def asMaster() = {
    out(valid, address)
    inWithNull(ready, data)
  }
}

case class RegFileBypass(addressWidth : Int, dataWidth : Int) extends Bundle with IMasterSlave{
  val valid = Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  override def asMaster() = {
    out(valid, address, data)
  }
}

trait RegfileService extends Service{
  def getPhysicalDepth : Int

  def newRead(withReady : Boolean) : RegFileRead
  def newWrite(withReady : Boolean) : RegFileWrite
  def newBypass() : RegFileBypass
}


//trait WakeService extends Service{
//  def newWakeOhPort() : Stream[WakeOh]
//  def newWakeRobIdPort() : Stream[WakeRobId]
//}

case class RescheduleCmd() extends Bundle{
  val nextRob = ROB.ID_TYPE()
}

case class CommitFree() extends Bundle{
  val robId = ROB.ID_TYPE()
}
case class CommitEvent() extends Bundle{
  val robId = ROB.ID_TYPE()
  val mask = Bits(COMMIT_COUNT bits)
}

trait CommitService  extends Service{
  def onCommit() : CommitEvent
  def newCompletionPort(canTrap : Boolean, canJump : Boolean) : Flow[CompletionCmd]
  def reschedulingPort() : Flow[RescheduleCmd]
  def freePort() : Flow[CommitFree]
}

trait RegfileSpec{
  def sizeArch : Int
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

case class ExecutionUnitPush() extends Bundle{
  val robId = ROB_ID()
}

trait ExecuteUnitService extends Service{
  def euName() : String
  def hasFixedLatency : Boolean
  def getFixedLatency : Int
  def pushPort() : Stream[ExecutionUnitPush]
  def addFunction(enc : Encoding)
}


trait RfAccess {
  def decode(i : Bits) : UInt
}
trait RfRead extends RfAccess
trait RfWrite extends RfAccess



object Riscv{

  def funct7Range = 31 downto 25
  def rdRange = 11 downto 7
  def funct3Range = 14 downto 12
  def rs2Range = 24 downto 20
  def rs1Range = 19 downto 15
  def rs3Range = 31 downto 27
  def csrRange = 31 downto 20
  def rsRange(id : Int) = List(rs1Range, rs2Range,rs3Range)(id)

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
      override def sizeArch = 32
      override def width = Global.XLEN
      override def x0AlwaysZero = true
    }

    def toResource(accesses : Seq[RfAccess]) = accesses.map(regfile -> _)

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
  val id = UInt(ROB.ID_WIDTH bits)
}
case class RobPushLine() extends Bundle {
  val id = UInt(ROB.ID_WIDTH bits)
  val entries = Vec.fill(ROB.COLS)(RobPushEntry())
}
case class RobPushEntry() extends Bundle{
  val commitTask = NoData
}

case class RobPopLine() extends Bundle {
  val id = UInt(ROB.ID_WIDTH bits)
  val entries = Vec.fill(ROB.COLS)(RobPopEntry())
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
  val robId = ROB.ID_TYPE()

  val jump = canJump generate Bool()
  val trap = canTrap generate Bool()
  val cause = canTrap generate UInt(Global.TRAP_CAUSE_WIDTH bits)
  val arg = (canTrap || canJump) generate Bits(Global.XLEN bits) //Target PC if jump, payload if trap
}

case class RobWait() extends Area with OverridedEqualsHashCode {
  val ID = Stageable(ROB.ID_TYPE)
  val ENABLE = Stageable(Bool())
}

trait IssueService extends Service{
  def newRobWait() : RobWait
  def retain() : Unit
  def release() : Unit
}


trait WakeService extends Service{
  def wakeRobs : Seq[Flow[UInt]]
}