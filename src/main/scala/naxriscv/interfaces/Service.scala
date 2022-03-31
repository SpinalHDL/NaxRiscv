package naxriscv.interfaces

import spinal.core._
import spinal.lib._
import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.riscv.CSR
import spinal.lib.pipeline._
import naxriscv.utilities.{AllocatorMultiPortPop, Service}
import spinal.core.fiber.{Handle, Lock}
import spinal.lib.logic.Masked
import spinal.lib.pipeline.Stageable

import scala.collection.mutable.ArrayBuffer



object JumpService{
  object Priorities{
    def FETCH_WORD(stage : Int, prediction : Boolean) = stage*2 + (if(prediction) -1 else 0)
    val ALIGNER           = 90
    val DECODE_PREDICTION = 100
    val COMMIT_RESCHEDULE = 200
    val COMMIT_TRAP = 201
  }
}

case class JumpCmd(pcWidth : Int) extends Bundle{
  val pc = UInt(pcWidth bits)
}
trait JumpService extends Service{
  def createJumpInterface(priority : Int, aggregationPriority : Int = 0) : Flow[JumpCmd] //High priority win
}

trait InitCycles extends Service{
  def initCycles : Int
}

//A EuGroup is composed of ExecuteUnitService which all exactly implement the same instructions
case class EuGroup(eus : Seq[ExecuteUnitService],
                   sel: Stageable[Bool],
                   microOps : Seq[MicroOp])

case class DecoderTrap() extends Bundle{
  val cause = UInt(4 bits)
  val epc   = UInt(PC_WIDTH bits)
  val tval  = Bits(TVAL_WIDTH bits)
}

trait DecoderService extends Service with LockedService {
  def addEuOp(fu: ExecuteUnitService, microOp : MicroOp) : Unit
  def addResourceDecoding(resource : Resource, stageable : Stageable[Bool])
  def covers() : Seq[Masked] //List of all instruction implemented
  def euGroups : Seq[EuGroup]
  def addMicroOpDecoding(microOp: MicroOp, decoding: DecodeListType)
  def addMicroOpDecodingDefault(key : Stageable[_ <: BaseType], value : BaseType) : Unit

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
  def getTrap() : Flow[DecoderTrap]

  //The trap interface allow the privilegied plugin to ask the decoder to produce trap
  def trapHalt() : Unit
  def trapRaise() : Unit
  def trapReady() : Bool
}

trait RobService extends Service{
  def newRobCompletion() : Flow[RobCompletion]
  def newRobLineValids(bypass : Boolean) : RobLineMask

  def write[T <: Data](key: Stageable[T], size : Int, value : Seq[T], robId : UInt, enable : Bool) : Unit //robid need to be aligned on value size
  def readAsync[T <: Data](key: Stageable[T], size : Int, robId: UInt, skipFactor: Int = 1, skipOffset: Int = 0) : Vec[T]
  def readAsyncSingle[T <: Data](key: Stageable[T], robId : UInt, skipFactor : Int = 1, skipOffset : Int = 0) : T = {
    val ret = readAsync(key, 1, robId, skipFactor, skipOffset).head
    CombInit(ret)
  }

  def retain() : Unit
  def release() : Unit
}


case class RobLineMask(bypass : Boolean) extends Bundle{
  val line = ROB.ID()
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
  val robId = ROB.ID()

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

case class RegFileRead(addressWidth : Int, dataWidth : Int, withReady : Boolean, latency : Int, forceNoBypass : Boolean) extends Bundle with IMasterSlave{
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
  def rfSpec : RegfileSpec
  def getPhysicalDepth : Int

  def newRead(withReady : Boolean, forceNoBypass : Boolean = false) : RegFileRead
  def newWrite(withReady : Boolean, latency : Int) : RegFileWrite
  def newBypass() : RegFileBypass

  def getWrites() : Seq[RegFileWrite]

  def retain() : Unit
  def release() : Unit
}


case class RescheduleEvent(causeWidth : Int) extends Bundle{
  val robId      = ROB.ID()
  val robIdNext  = ROB.ID()
  val trap       = Bool()
  val cause      = UInt(causeWidth bits)
  val tval       = Bits(TVAL_WIDTH bits)
  val reason     = ScheduleReason.hardType()
}




case class CommitFree() extends Bundle{
  val robId = ROB.ID()
  val commited = Bits(COMMIT_COUNT bits)
}
case class CommitEvent() extends Bundle{
  val robId = ROB.ID()
  val mask = Bits(COMMIT_COUNT bits)
}


trait CommitService  extends Service{
  def onCommit() : CommitEvent
//  def onCommitLine() : Flow[CommitEvent]
  def newSchedulePort(canTrap : Boolean, canJump : Boolean, causeWidth : Int = 4) : Flow[ScheduleCmd]
  def reschedulingPort() : Flow[RescheduleEvent]
  def freePort() : Flow[CommitFree]
  def nextCommitRobId : UInt
  def currentCommitRobId : UInt
  def rescheduleCauseWidth : Int
  def isRobEmpty : Bool
}

//TODO reduce area usage if physRdType isn't needed by some execution units
case class ExecutionUnitPush(physRdType : Stageable[UInt], withReady : Boolean, contextKeys : Seq[Stageable[_ <: Data]], withValid : Boolean = true) extends Bundle{
  val valid = withValid generate Bool()
  val ready = withReady generate Bool()
  val robId = ROB.ID()
  val physRd = physRdType()
  val context = contextKeys.map(_.craft())

  def getContext[T <: Data](key : Stageable[T]) : T = context(contextKeys.indexOf(key)).asInstanceOf[T]

  def toStream ={
    val ret = Stream(ExecutionUnitPush(physRdType, false, contextKeys, false))
    ret.valid := valid
    ready := ret.ready
    ret.payload := this
    ret
  }

  def toFlow = {
    val ret = Flow(ExecutionUnitPush(physRdType, false, contextKeys, false))
    ret.valid := valid && (if(withReady) ready else True)
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

object ScheduleReason{
  val hardType = Stageable(UInt(8 bits))
  val TRAP = 0x01
  val ENV = 0x02
  val LOAD_HIT_MISS_PREDICTED = 0x03
  val BRANCH = 0x10
  val JUMP = 0x11
  val STORE_TO_LOAD_HAZARD = 0x20
}

case class ScheduleCmd(canTrap : Boolean, canJump : Boolean, pcWidth : Int, causeWidth : Int, withRobID : Boolean = true) extends Bundle {
  val robId      = withRobID generate ROB.ID()
  val trap       = (canTrap && canJump) generate Bool()
  val pcTarget   = canJump generate UInt(pcWidth bits)
  val cause      = canTrap generate UInt(causeWidth bits)
  val tval       = canTrap generate Bits(TVAL_WIDTH bits)
  val skipCommit = Bool() //Want to skip commit for exceptions, but not for [jump, ebreak, redo]
  val reason     = ScheduleReason.hardType()

  def isTrap = (canTrap, canJump) match {
    case (false, true) => False
    case (true, false) => True
    case (true, true) =>  trap
    case _ => ???
  }
}

case class RobWait() extends Area with OverridedEqualsHashCode {
  val ID = Stageable(ROB.ID)
  val ENABLE = Stageable(Bool())
  val ENABLE_UNSKIPED = Stageable(Bool())
}

trait IssueService extends Service with LockedService {
  def newRobDependency() : RobWait
  def fenceOlder(microOp: MicroOp) : Unit
  def fenceYounger(microOp: MicroOp) : Unit
}

case class WakeRob() extends Bundle {
  val robId = ROB.ID()
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

class AddressTranslationRsp(s : AddressTranslationService, wakesCount : Int, val rspStage : Stage, val wayCount : Int) extends Area{
  val keys = new Area {
    setName("MMU")
    val TRANSLATED = Stageable(UInt(PHYSICAL_WIDTH bits))
    val IO = Stageable(Bool())
    val REDO = Stageable(Bool())
    val ALLOW_READ, ALLOW_WRITE, ALLOW_EXECUTE = Stageable(Bool())
    val PAGE_FAULT = Stageable(Bool())
    val ACCESS_FAULT = Stageable(Bool())
    val WAYS_OH  = Stageable(Bits(wayCount bits))
    val WAYS_PHYSICAL  = Stageable(Vec.fill(wayCount)(UInt(PHYSICAL_WIDTH bits)))
    val BYPASS_TRANSLATION = Stageable(Bool())
  }
  val wake = Bool()
  val pipelineLock = Lock().retain()
}

trait AddressTranslationPortUsage
object AddressTranslationPortUsage{
  object FETCH extends AddressTranslationPortUsage
  object LOAD_STORE extends AddressTranslationPortUsage
}

trait AddressTranslationService extends Service with LockedImpl {
  def newStorage(pAny: Any) : Any
  def newTranslationPort(stages: Seq[Stage],
                         preAddress: Stageable[UInt],
                         usage: AddressTranslationPortUsage,
                         portSpec: Any,
                         storageSpec: Any): AddressTranslationRsp

  def withTranslation : Boolean
  def invalidatePort : PulseHandshake[NoData]
}

class CsrSpec(val csrFilter : Any){
//  csrFilter match {
//    case _ : Int =>
//    case _ : Range =>
//  }
}
case class CsrOnRead (override val csrFilter : Any, onlyOnFire : Boolean, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrOnReadToWrite (override val csrFilter : Any, body : () => Unit) extends CsrSpec(csrFilter) //Allow the fancy supervisor external interrupt logic
case class CsrOnWrite(override val csrFilter : Any, onlyOnFire : Boolean, body : () => Unit) extends CsrSpec(csrFilter)
case class CsrOnReadData (override val csrFilter : Any, bitOffset : Int, value : Data) extends CsrSpec(csrFilter)
case class CsrOnDecode (override val csrFilter : Any, priority : Int, body : () => Unit) extends CsrSpec(csrFilter)

case class CsrRamSpec(override val csrFilter : Any, alloc : CsrRamAllocation) extends CsrSpec(csrFilter)

case class CsrListFilter(mapping : List[Int]) extends Nameable
trait CsrService extends Service with LockedImpl{
  val spec = ArrayBuffer[CsrSpec]()
  def onRead (csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = spec += CsrOnRead(csrFilter, onlyOnFire, () => body)
  def onReadToWrite (csrFilter : Any)(body : => Unit) = spec += CsrOnReadToWrite(csrFilter, () => body)
  def onWrite(csrFilter : Any, onlyOnFire : Boolean)(body : => Unit) = spec += CsrOnWrite(csrFilter, onlyOnFire, () => body)
  def onReadHalt() : Unit
  def onWriteHalt() : Unit
  def onWriteBits : Bits
  def onWriteAddress : UInt
  def onReadAddress : UInt
  def getCsrRam() : CsrRamService
  def onReadMovingOff : Bool
  def onWriteMovingOff : Bool
  def onDecode(csrFilter : Any, priority : Int = 0)(body : => Unit) = spec += CsrOnDecode(csrFilter, priority, () => body)
  def onDecodeTrap() : Unit
  def onDecodeUntrap() : Unit
  def onDecodeFlushPipeline() : Unit
  def onDecodeRead : Bool
  def onDecodeWrite : Bool
  def onDecodeAddress : UInt
  def allowCsr(csrFilter : Any) = onDecode(csrFilter){}

  def onReadToWriteBits : Bits

  def readWrite(alloc : CsrRamAllocation, filters : Any) = spec += CsrRamSpec(filters, alloc)
  def readWriteRam(filters : Int) = {
    val alloc = getCsrRam.ramAllocate(1)
    spec += CsrRamSpec(filters, alloc)
    alloc
  }

  def read[T <: Data](value : T, csrFilter : Any, bitOffset : Int = 0) : Unit = {
    spec += CsrOnReadData(csrFilter, bitOffset, value)
  }
//  def readOnly[T <: Data](value : T, csrFilter : Any, bitOffset : Int = 0) : Unit = {
//    read(value, csrFilter, bitOffset)
//    onDecode(csrFilter, false){ onWriteTrap() }
//  }
  def write[T <: Data](value : T, csrId : Int, bitOffset : Int = 0) : Unit = {
    onWrite(csrId, true){ value.assignFromBits(onWriteBits(bitOffset, widthOf(value) bits)) }
  }
  def writeWhen[T <: Data](value : T, cond : Bool, csrId : Int, bitOffset : Int = 0) : Unit = {
    onWrite(csrId, true){ when(cond) { value.assignFromBits(onWriteBits(bitOffset, widthOf(value) bits)) }}
  }
  def readWrite[T <: Data](value : T, csrId : Int, bitOffset : Int = 0) : Unit = {
    read(value, csrId, bitOffset)
    write(value, csrId, bitOffset)
  }

  def readToWrite[T <: Data](value : T, csrFilter : Any, bitOffset : Int = 0) : Unit = {
    onReadToWrite(csrFilter){
      onReadToWriteBits(bitOffset, widthOf(value) bits) := value.asBits
    }
  }

  def readWrite(csrAddress : Int, thats : (Int, Data)*) : Unit = for(that <- thats) readWrite(that._2, csrAddress, that._1)
  def write(csrAddress : Int, thats : (Int, Data)*) : Unit = for(that <- thats) write(that._2, csrAddress, that._1)
  def read(csrAddress : Int, thats : (Int, Data)*) : Unit = for(that <- thats) read(that._2, csrAddress, that._1)
  def isReading(csrFilter : Any): Bool ={
    val ret = False
    onRead(csrFilter, false){ ret := True }
    ret
  }
}

class CsrRamAllocation(val entries : Int){
  var at = -1
  var addressWidth = -1
  def getAddress(offset : UInt) : UInt = {
    U(at, addressWidth bits) | offset
  }
  def getAddress() = U(at, addressWidth bits)

  val entriesLog2 = 1 << log2Up(entries)
}
case class CsrRamRead(addressWidth : Int, dataWidth : Int, priority : Int) extends Bundle{
  val valid, ready = Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits) //One cycle after fired

  def fire = valid && ready
}

case class CsrRamWrite(addressWidth : Int, dataWidth : Int, priority : Int) extends Bundle{
  val valid, ready = Bool()
  val address = UInt(addressWidth bits)
  val data = Bits(dataWidth bits)

  def fire = valid && ready
}


object CsrRamService{
  //Priorities are arranged in a way to improve ports.ready timings
  val priority = new {
    val INIT    = 0
    val TRAP    = 1
    val COUNTER = 2
    val CSR     = 3  //This is the very critical path
  }
}
//usefull for, for instance, mscratch scratch mtvec stvec mepc sepc mtval stval satp pmp stuff
trait CsrRamService extends Service {
  def ramAllocate(entries : Int) : CsrRamAllocation
  def ramReadPort(priority : Int) : Handle[CsrRamRead]
  def ramWritePort(priority : Int) : Handle[CsrRamWrite]
  val allocationLock = Lock()
  val portLock = Lock()
}



trait PrivilegedService extends Service{
  def hasMachinePriv : Bool
  def hasSupervisorPriv : Bool
  def getPrivilege() : UInt
  def isUser() = getPrivilege() === 0
  def isSupervisor() = getPrivilege() === 1
  def isMachine() = getPrivilege() === 3

  def implementSupervisor : Boolean
  def implementUserTrap : Boolean

  def xretAwayFromMachine : Bool

  def addMisa(id : Int) : Unit
  def addMisa(id : Char) : Unit = addMisa(id - 'A')
}



object PerformanceCounterService{
  val ICACHE_REFILL = 1
  val DCACHE_REFILL = 2
  val DCACHE_WRITEBACK = 3
  val BRANCH_MISS = 4
}

trait PerformanceCounterService extends Service with LockedImpl{
  def createEventPort(id : Int) : Bool
}

case class PulseHandshake[T <: Data](payloadType : HardType[T] = NoData()) extends Bundle with IMasterSlave {
  val request = Bool()
  val served  = Bool()
  val payload = payloadType()


  override def asMaster() = {
    out(request, payload)
    in(served)
  }

  def setIdleAll(): this.type ={
    request := False
    served := False
    payload.assignDontCare()
    this
  }

  def setIdle(): this.type ={
    request := False
    payload.assignDontCare()
    this
  }

  def isPending() : Bool = RegInit(False) setWhen(request) clearWhen(served)
}

trait PostCommitBusy extends Service{
  def postCommitBusy : Bool
}