package naxriscv.interfaces

import spinal.core._
import spinal.lib._
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import spinal.lib.pipeline._
import naxriscv.utilities.{AllocatorMultiPortPop, Service}
import spinal.core.fiber.Lock
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

trait InitCycles extends Service{
  def initCycles : Int
}

//A EuGroup is composed of ExecuteUnitService which all exactly implement the same instructions
case class EuGroup(eus : Seq[ExecuteUnitService],
                   sel: Stageable[Bool],
                   microOps : Seq[MicroOp])

trait DecoderService extends Service{
  def addEuOp(fu: ExecuteUnitService, microOp : MicroOp) : Unit

  def euGroups : Seq[EuGroup]

  def READ_RS(id : Int)  : Stageable[Bool]
  def ARCH_RS(id : Int)  : Stageable[UInt]
  def PHYS_RS(id : Int)  : Stageable[UInt]

  def WRITE_RD : Stageable[Bool]
  def PHYS_RD  : Stageable[UInt]
  def PHYS_RD_FREE : Stageable[UInt]
  def ARCH_RD  : Stageable[UInt]

  def rsCount  : Int
  def rsPhysicalDepthMax : Int
}

trait RobService extends Service{
  def robCompletion() : Flow[RobCompletion]
  def robLineValids() : RobLineMask

  def write[T <: Data](key: Stageable[T], size : Int, value : Seq[T], robId : UInt, enable : Bool) : Unit //robid need to be aligned on value size
  def readAsync[T <: Data](key: Stageable[T], size : Int, robId: UInt, skipFactor: Int = 1, skipOffset: Int = 0) : Vec[T]
  def readAsyncSingle[T <: Data](key: Stageable[T], robId : UInt, skipFactor : Int = 1, skipOffset : Int = 0) : T = {
    val ret = readAsync(key, 1, robId, skipFactor, skipOffset).head
    CombInit(ret)
  }

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

case class RegFileWrite(addressWidth : Int, dataWidth : Int, withReady : Boolean) extends Bundle with IMasterSlave {
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)
  val robId = ROB.ID_TYPE()

  def fire = if(withReady) valid && ready else valid

  def asWithoutReady() = {
    val ret = RegFileWrite(addressWidth, dataWidth, false)
    ret.valid := this.fire
    ret.address := this.address
    ret.data := this.data
    ret.robId := this.robId
    ret
  }

  override def asMaster() = {
    out(valid, address, data, robId)
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

  def getWrites() : Seq[RegFileWrite]

  def retain() : Unit
  def release() : Unit
}


case class RescheduleEvent() extends Bundle{
  val nextRob = ROB.ID_TYPE()
}




case class CommitFree() extends Bundle{
  val robId = ROB.ID_TYPE()
  val commited = Bits(COMMIT_COUNT bits)
}
case class CommitEvent() extends Bundle{
  val robId = ROB.ID_TYPE()
  val mask = Bits(COMMIT_COUNT bits)
}


trait CommitService  extends Service{
  def onCommit() : CommitEvent
//  def onCommitLine() : Flow[CommitEvent]
  def newSchedulePort(canTrap : Boolean, canJump : Boolean) : Flow[ScheduleCmd]
  def reschedulingPort() : Flow[RescheduleEvent]
  def freePort() : Flow[CommitFree]
}

case class ExecutionUnitPush() extends Bundle{
  val robId = ROB_ID()
}

trait LockedService {
  def retain()
  def release()
}

trait LockedImpl extends LockedService{
  val lock = Lock()
  override def retain() = lock.retain()
  override def release() = lock.release()
}

trait ExecuteUnitService extends Service with LockedService{
  def euName() : String
  def hasFixedLatency : Boolean
  def getFixedLatency : Int
  def pushPort() : Stream[ExecutionUnitPush]
  def addMicroOp(enc : MicroOp)
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

case class ScheduleCmd(canTrap : Boolean, canJump : Boolean) extends Bundle {
  val robId    = ROB.ID_TYPE()
  val trap     = (canTrap && canJump) generate Bool()
  val pcTarget = canJump generate PC()
  val cause    = canTrap generate UInt(Global.TRAP_CAUSE_WIDTH bits)
  val tval     = canTrap generate Bits(Global.XLEN bits)
  val skipCommit = canTrap generate Bool() //when trap is set, should be set for regular exception, but cleared for let's say a ebreak

  def doesSkipCommit = (canTrap, canJump) match {
    case (false, _) => False
    case (true,  false) => skipCommit
    case (true,  true) => trap && skipCommit
  }
  def isTrap = (canTrap, canJump) match {
    case (false, true) => False
    case (true, false) => True
    case (true, true) =>  trap
  }
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