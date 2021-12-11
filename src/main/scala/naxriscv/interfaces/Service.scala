package naxriscv.interfaces

import spinal.core._
import spinal.lib._
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import spinal.lib.pipeline._
import naxriscv.utilities.{AllocatorMultiPortPop, Service}
import spinal.core.fiber.{Handle, Lock}
import spinal.lib.pipeline.Stageable

import scala.collection.mutable.ArrayBuffer



object JumpService{
  object Priorities{
    val FETCH_WORD = 0
    val PREDICTOR = 100
    val COMMIT = 200
  }
}

case class JumpCmd(pcWidth : Int) extends Bundle{
  val pc = UInt(pcWidth bits)
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

trait DecoderService extends Service with LockedService {
  def addEuOp(fu: ExecuteUnitService, microOp : MicroOp) : Unit
  def addResourceDecoding(resource : Resource, stageable : Stageable[Bool])

  def euGroups : Seq[EuGroup]

  def READ_RS(id : Int)  : Stageable[Bool]
  def ARCH_RS(id : Int)  : Stageable[UInt]
  def PHYS_RS(id : Int)  : Stageable[UInt]

  def READ_RS(id : RfRead)  : Stageable[Bool]
  def ARCH_RS(id : RfRead)  : Stageable[UInt]
  def PHYS_RS(id : RfRead)  : Stageable[UInt]

  def WRITE_RD : Stageable[Bool]
  def PHYS_RD  : Stageable[UInt]
  def PHYS_RD_FREE : Stageable[UInt]
  def ARCH_RD  : Stageable[UInt]

  def rsCount  : Int
  def rsPhysicalDepthMax : Int
}

trait RobService extends Service{
  def newRobCompletion() : Flow[RobCompletion]
  def newRobLineValids() : RobLineMask

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

case class RegFileWrite(addressWidth : Int, dataWidth : Int, withReady : Boolean, latency : Int = 1) extends Bundle with IMasterSlave {
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
  def newWrite(withReady : Boolean, latency : Int) : RegFileWrite
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
  def nextCommitRobId : UInt
}

//TODO reduce area usage if physRdType isn't needed by some execution units
case class ExecutionUnitPush(physRdType : Stageable[UInt], withReady : Boolean, withValid : Boolean = true) extends Bundle{
  val valid = withValid generate Bool()
  val ready = withReady generate Bool()
  val robId = ROB_ID()
  val physRd = physRdType()

  def toStream ={
    val ret = Stream(ExecutionUnitPush(physRdType, false, false))
    ret.valid := valid
    ready := ret.ready
    ret.payload := this
    ret
  }
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

case class StaticLatency(microOp: MicroOp, latency : Int)

trait ExecuteUnitService extends Service with LockedService{
  def euName() : String
  def hasFixedLatency : Boolean
  def getFixedLatencies : Int
  def pushPort() : ExecutionUnitPush
  def staticLatencies() : ArrayBuffer[StaticLatency] = ArrayBuffer[StaticLatency]()
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

case class ScheduleCmd(canTrap : Boolean, canJump : Boolean, pcWidth : Int) extends Bundle {
  val robId      = ROB.ID_TYPE()
  val trap       = (canTrap && canJump) generate Bool()
  val pcTarget   = canJump generate UInt(pcWidth bits)
  val cause      = canTrap generate UInt(Global.TRAP_CAUSE_WIDTH bits)
  val tval       = canTrap generate Bits(Global.XLEN bits)
  val skipCommit = Bool() //Want to skip commit for exceptions, but not for [jump, ebreak, redo]

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

trait IssueService extends Service with LockedService {
  def newRobDependency() : RobWait
}

case class WakeRob() extends Bundle {
  val robId = ROB.ID_TYPE()
}

case class WakeRegFile(physicalType : HardType[UInt], needBypass : Boolean) extends Bundle {
  val physical = physicalType()
}

trait WakeRobService extends Service{
  def wakeRobs : Seq[Flow[WakeRob]]
}

trait WakeRegFileService extends Service{
  //WARNING, do not wake some index that you do no own, for instance write into x0
  def wakeRegFile : Seq[Flow[WakeRegFile]]
}

trait WakeWithBypassService extends Service{
  def wakeRobsWithBypass : Seq[Flow[UInt]]
}

//case class AddressTranslationPort(prenWidth : Int,
//                                  postWidth : Int) extends Bundle with IMasterSlave {
//  val cmd = Flow(AddressTranslationCmd(prenWidth))
//  val rsp = Flow(AddressTranslationRsp(postWidth))
//
//  override def asMaster() = {
//    master(cmd)
//    slave(rsp)
//  }
//}
//
//case class AddressTranslationCmd(preWidth : Int) extends Bundle{
//  val virtual = UInt(preWidth bits)
//}
//
//case class AddressTranslationRsp(postWidth : Int) extends Bundle{
//  val physical = UInt(postWidth bits)
//  val peripheral = Bool()
//}

class AddressTranslationRsp(s : AddressTranslationService, wakesCount : Int, val rspStage : Stage) extends Area{
  val keys = new AreaRoot {
    val TRANSLATED = Stageable(UInt(s.postWidth bits))
    val IO = Stageable(Bool())
    val REDO = Stageable(Bool())
    val ALLOW_READ, ALLOW_WRITE, ALLOW_EXECUTE = Stageable(Bool())
    val PAGE_FAULT = Bool()
    val WAKER = Stageable(Bits(wakesCount bits))
    val WAKER_ANY = Stageable(Bool())
  }

  val wakes = Bits(wakesCount bits)
}

trait AddressTranslationService extends Service with LockedImpl {
  def preWidth : Int
  def postWidth : Int
  def newTranslationPort(stages: Seq[Stage],
                         preAddress: Stageable[UInt],
                         p: Any): AddressTranslationRsp
  def wakerCount : Int
  def wakes : Bits
  def PC : Stageable[UInt]
  def withTranslation : Boolean
}