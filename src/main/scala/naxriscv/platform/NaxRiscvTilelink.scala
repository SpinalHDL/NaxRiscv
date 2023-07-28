package naxriscv.platform

import naxriscv.Global.XLEN
import naxriscv.utilities.Plugin
import naxriscv.lsu2._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv._
import naxriscv.execute.CsrAccessPlugin
import naxriscv.frontend.DecoderPlugin
import naxriscv.lsu.DataCachePlugin
import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader}
import spinal.core
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.{Hub, HubFabric}
import spinal.lib.bus.tilelink.sim.{MemoryAgent, Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionD}
import spinal.lib.bus.tilelink.{M2sSupport, M2sTransfers, Opcode, S2mSupport, SizeRange, fabric}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.misc.{Elf, TilelinkFabricClint}
import spinal.lib.sim.SparseMemory
import spinal.sim.{Signal, SimManagerContext}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer



class NaxRiscvTilelink extends Area with RiscvHart{
  val ibus = Node.master()
  val dbus = Node.master()
  val pbus = Node.master()

  val buses = List(ibus, dbus, pbus)


  def privPlugin = thread.core.framework.getService[PrivilegedPlugin]
  override def getHartId() = privPlugin.p.hartId
  override def getIntMachineTimer() = privPlugin.io.int.machine.timer
  override def getIntMachineSoftware() = privPlugin.io.int.machine.software

//  core.plugins.foreach{
//    case p : FetchCachePlugin => ibus.bus << p.mem.toTilelink()
//    case p : DataCachePlugin =>  dbus.bus << p.mem.toTilelink()
//    case p : Lsu2Plugin => pbus.bus << p.peripheralBus.toTilelink()
//    case p : PrivilegedPlugin => {
//      p.io.int.machine.timer := False
//      p.io.int.machine.software := False

  val thread = Fiber build new Area{
    val l = Config.plugins(
      withCoherency = false,
      withRdTime = false,
      aluCount    = 1,
      decodeCount = 1,
      ioRange = a => a(31 downto 28) === 0x1
    )

    // Add a plugin to Nax which will handle the negotiation of the tilelink parameters
    l += new Plugin {
      val setup = create early new Area{
        framework.plugins.foreach{
          case p : FetchCachePlugin => ibus.m2s.forceParameters(p.getBusParameter().toTileLinkM2sParameters())
          case p : DataCachePlugin => dbus.m2s.forceParameters(p.setup.dataCacheParameters.memParameter.toTileLinkM2sParameters())
          case p : Lsu2Plugin => pbus.m2s.forceParameters(p.getPeripheralBusParameters().toTileLinkM2sParameters())
          case _ =>
        }

        framework.plugins.foreach{
          case p : FetchCachePlugin => ibus.s2m.supported.load(S2mSupport.none())
          case p : Lsu2Plugin => pbus.s2m.supported.load(S2mSupport.none())
          case p: DataCachePlugin => p.withCoherency match {
            case false => dbus.s2m.supported.load(S2mSupport.none())
            case true => dbus.s2m.supported.load(p.setup.dataCacheParameters.toTilelinkS2mSupported(dbus.s2m.proposed))
          }
          case _ =>
        }

        framework.plugins.foreach{
          case p: DataCachePlugin =>
            if(p.withCoherency)p.setCoherencyInfo(dbus.m2s.parameters.sourceWidth, dbus.s2m.parameters.sinkWidth)
          case _ =>
        }
      }
    }


    val core = new NaxRiscv(l)
    core.plugins.foreach{
      case p : FetchCachePlugin => ibus.bus << p.mem.toTilelink()
      case p : DataCachePlugin =>  dbus.bus << p.mem.toTilelink()
      case p : Lsu2Plugin => pbus.bus << p.peripheralBus.toTilelink()
      case p : PrivilegedPlugin => {
        p.io.int.machine.external := False
        p.io.int.supervisor.external := False
      }
      case _ =>
    }
  }
}



class NaxRiscvTilelinkSoCDemo extends Component {

  val nax = new NaxRiscvTilelink()

  val filter = new fabric.TransferFilter()
  filter.up << nax.buses



  val nonCoherent = Node()

//  val hub = new HubFabric()
//  hub.up << filter.down
//  nonCoherent << hub.down

  nonCoherent << filter.down

  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node << nonCoherent

  val peripheral = new Area {
    val bus = Node()
    bus at 0x10000000l of nonCoherent

    val clint = new TilelinkFabricClint()
    clint.node at 0x10000 of bus
    clint.bindHart(nax)

    val emulated = new tilelink.fabric.SlaveBus(
      M2sSupport(
        addressWidth = 28,
        dataWidth = 32,
        transfers = M2sTransfers(
          get = SizeRange(4),
          putFull = SizeRange(4)
        )
      )
    )
    emulated.node << bus
  }
}

object NaxRiscvTilelinkGen extends App{
  SpinalVerilog(new NaxRiscvTilelinkSoCDemo())
}


/*
NaxRiscv lockstep :
- Global memory
  - Coherent masters update the global memory ordering (SQ, post commit)
  - A history of global memory updates is kept
    That way, the global memory from a little while ago can be reconstructed

- d$
  - Keep a list of committed stores not yet in the global memory ordering (still in the SQ)
  - Update global memory content when a store exit SQ
  - ref load value
    - Each CPU load instruction has to remember the global memory generation it was in, so


- i$
  - The simulation keep track of i$ lines content and the history of line refills
  - Each chunk of the instruction know the i$ line refill generation it came from
  - That ID can then be used to retrieve the expected cache content at the moment of the fetch
  - The simulation model of the i$ content is checked against the global ordering using debugId



Memory ordering tool
- Has its own memory storage
- A history of the content before writes is kept
- Can create multiple "View" allowing to model a CPU perspective
  - Has a list of inflight writes which only apply to the view.
  - Those inflight write will be written in the memory when removed

On CPU store
- On software SQ commit -> Add write to the inflight view
- On hardware SQ exit -> remove write from the inflight view, update memory storage with it

On CPU load
- On hardware D$ access -> Capture memory ordering history ID + inflight stores
- On software access -> provide history ID and overlay inflight stores that completed


CPU probes :
- Commit event
  - PC, RF writes, CSR write
- SQ entry completion
- IO bus read/writes


*/

class RobCtx{
  var pc = -1l;
  var integerWriteValid = false
  var integerWriteData = -1l
  var floatWriteValid = false
  var floatWriteData = -1l
  var floatFlags = -1

  var csrValid = false
  var csrWriteDone = false
  var csrReadDone = false
  var  csrAddress = -1
  var csrWriteData = -1l
  var csrReadData = -1l

  def clear(){
    integerWriteValid = false;
    floatWriteValid = false;
    csrValid = false;
    csrWriteDone = false;
    csrReadDone = false;
    floatFlags = 0;
  }
  clear()
};

import spinal.core.sim._
class NaxSimProbe(nax : NaxRiscv, hartId : Int){
  val alignerPlugin = nax.framework.getService[AlignerPlugin]
  val csrAccessPlugin = nax.framework.getService[CsrAccessPlugin]
  val decodePlugin = nax.framework.getService[DecoderPlugin]
  val commitPlugin = nax.framework.getService[CommitPlugin]
  val privPlugin = nax.framework.getService[PrivilegedPlugin]
  val robPlugin = nax.framework.getService[RobPlugin]
  val lsuPlugin = nax.framework.getService[Lsu2Plugin]
  val intRf = nax.framework.getServiceWhere[RegFilePlugin](_.spec == riscv.IntRegFile)

  val commitWb = commitPlugin.logic.whitebox
  val commitMask = commitWb.commit.mask.simProxy()
  val commitRobId = commitWb.commit.robId.simProxy()
  val commitPc = commitWb.commit_pc.map(_.simProxy()).toArray
  val commitRobToPcValid = commitWb.robToPc.valid.simProxy()
  val commitRobToPcRobId = commitWb.robToPc.robId.simProxy()
  val commitRobToPcValue = commitWb.robToPc.pc.map(_.simProxy()).toArray

  val intRfEvents = intRf.logic.writeEvents.toArray
  val intRfEventsValid = intRfEvents.map(_.valid.simProxy())

  val ioBus = lsuPlugin.peripheralBus
  val ioBusCmdValid = ioBus.cmd.valid.simProxy()
  val ioBusCmdReady = ioBus.cmd.ready.simProxy()
  val ioBusRspValid = ioBus.rsp.valid.simProxy()

  val csrAccess = csrAccessPlugin.logic.whitebox.csrAccess
  val csrAccessValid = csrAccess.valid.simProxy()

  val trapWb = privPlugin.logic.whitebox
  val trapFire = trapWb.trap.fire.simProxy()

  val backends = ArrayBuffer[TraceBackend]()
  val commitsCallbacks = ArrayBuffer[(Int, Long) => Unit]()
  nax.scopeProperties.restore()
  val xlen = decodePlugin.xlen

  val robArray = Array.fill(robPlugin.robSize)(new RobCtx)


  def add(tracer : FileBackend) = {
    backends += tracer
    tracer.newCpu(hartId, "RV32IMA", "MSU", 32)
  }

  def checkRob() : Unit = {
    if(commitRobToPcValid.toBoolean){
      val robId = commitRobToPcRobId.toInt
      for(i <- 0 until commitRobToPcValue.size){
        var pc = commitRobToPcValue(i).toLong;
        if(xlen == 32) pc = (pc << 32) >> 32
        val ctx = robArray(robId + i)
        ctx.clear()
        ctx.pc = pc
      }
    }

    for(i <- 0 until intRfEvents.size){
      if(intRfEventsValid(i).toBoolean){
        val port = intRfEvents(i)
        val robId = port.robId.toInt
        val ctx = robArray(robId)
        ctx.integerWriteValid = true
        var data = port.data.toLong
        if(xlen == 32) data = (data << 32) >> 32
        ctx.integerWriteData = data
      }
    }

    if(csrAccessValid.toBoolean){
      val robId = csrAccess.robId.toInt
      val ctx = robArray(robId)
      ctx.csrValid = true
      ctx.csrAddress = csrAccess.address.toInt
      ctx.csrWriteDone = csrAccess.writeDone.toBoolean
      ctx.csrReadDone = csrAccess.readDone.toBoolean
      ctx.csrWriteData = csrAccess.write.toLong
      ctx.csrReadData = csrAccess.read.toLong
    }
  }

  def checkTrap(): Unit ={
    if(trapFire.toBoolean){
      val code = trapWb.trap.code.toInt
      val interrupt = trapWb.trap.interrupt.toBoolean
      backends.foreach(_.trap(hartId, interrupt, code))
    }
  }

  def checkCommits(): Unit ={
    var mask = commitMask.toInt
    for(i <- 0 until commitPlugin.commitCount){
      if((mask & 1) != 0){
        val robId = commitRobId.toInt + i
        val robCtx = robArray(robId)
        if(robCtx.integerWriteValid){
          backends.foreach(_.writeRf(hartId, 0, 32, robCtx.integerWriteData))
        }
        if(robCtx.csrValid){
          if(robCtx.csrReadDone) backends.foreach(_.readRf(hartId, 4, robCtx.csrAddress, robCtx.csrReadData))
          if(robCtx.csrWriteDone) backends.foreach(_.writeRf(hartId, 4, robCtx.csrAddress, robCtx.csrWriteData))
        }
        backends.foreach(_.commit(hartId, robCtx.pc))
        commitsCallbacks.foreach(_(hartId, robCtx.pc))
      }
      mask >>= 1
    }
  }



  var ioAccess : TraceIo = null

  def checkIoBus(): Unit ={
    if(ioBusCmdValid.toBoolean && ioBusCmdReady.toBoolean){
      assert(ioAccess == null)
      ioAccess = new TraceIo(
        write   = ioBus.cmd.write.toBoolean,
        address = ioBus.cmd.address.toLong,
        data    = ioBus.cmd.data.toLong,
        mask    = ioBus.cmd.mask.toInt,
        size    = 1 << ioBus.cmd.size.toInt,
        error   = false
      )
    }
    if(ioBusRspValid.toBoolean){
      assert(ioAccess != null)
      ioAccess.error = ioBus.rsp.error.toBoolean
      if(!ioAccess.write) {
        val offset = (ioAccess.address) & (ioBus.p.dataWidth/8-1)
        val mask = (1l << ioAccess.size*8)-1
        ioAccess.data = (ioBus.rsp.data.toLong >> offset*8) & mask
      }
      backends.foreach(_.ioAccess(hartId, ioAccess))
      ioAccess = null
    }
  }

  nax.clockDomain.onSamplings{
    checkCommits()
    checkTrap()
    checkIoBus()
    checkRob()
  }
}

class TraceIo (var write: Boolean,
               var address: Long,
               var data: Long,
               var mask: Int,
               var size: Int,
               var error: Boolean){
  def serialized() = f"${write.toInt} $address%016x $data%016x $mask%02x $size ${error.toInt}"
}
trait TraceBackend{
  def newCpu(hartId : Int, isa : String, priv : String, physWidth : Int) : Unit
  def loadElf(path : File, offset : Long) : Unit
  def setPc(hartId : Int, pc : Long): Unit
  def writeRf(hardId : Int, rfKind : Int, address : Int, data : Long) //address out of range mean unknown
  def readRf(hardId : Int, rfKind : Int, address : Int, data : Long) //address out of range mean unknown
  def commit(hartId : Int, pc : Long): Unit
  def trap(hartId: Int, interrupt : Boolean, code : Int)
  def ioAccess(hartId: Int, access : TraceIo) : Unit

  def flush() : Unit
  def close() : Unit
}

class FileBackend(f : File) extends TraceBackend{
  val bf = new BufferedWriter(new FileWriter(f))

  override def commit(hartId: Int, pc: Long) = {
    bf.write(f"rv commit $hartId $pc%016x\n")
  }

  override def trap(hartId: Int, interrupt : Boolean, code : Int): Unit ={
    bf.write(f"rv trap $hartId ${interrupt.toInt} $code\n")
  }

  override def writeRf(hartId: Int, rfKind: Int, address: Int, data: Long) = {
    bf.write(f"rv rf w $hartId $rfKind $address $data%016x\n")
  }

  override def readRf(hartId: Int, rfKind: Int, address: Int, data: Long) = {
    bf.write(f"rv rf r $hartId $rfKind $address $data%016x\n")
  }

  def ioAccess(hartId: Int, access : TraceIo) : Unit = {
    bf.write(f"rv io $hartId ${access.serialized()}\n")
  }

  override def loadElf(path: File, offset: Long) = {
    bf.write(f"elf load ${path.getAbsolutePath} $offset%016x\n")
  }

  override def setPc(hartId: Int, pc: Long) = {
    bf.write(f"rv set pc $hartId $pc%016x\n")
  }

  override def newCpu(hartId: Int, isa : String, priv : String, physWidth : Int) = {
    bf.write(f"rv new $hartId $isa $priv $physWidth\n")
  }

  override def flush() = bf.flush()
  override def close() = bf.close()

  def spinalSimFlusher(period : Long): Unit ={
    periodicaly(period)(flush())
    onSimEnd(close())
  }
}

object NaxRiscvTilelinkSim extends App{
  val sc = SimConfig
  sc.allOptimisation
  sc.withFstWave
  sc.withConfig(SpinalConfig().includeSimulation)

  val compiled = sc.compile(new NaxRiscvTilelinkSoCDemo())

  def doIt() {
    compiled.doSimUntilVoid(seed = 42) { dut =>
      fork {
        disableSimWave()
        //      sleep(1939590-100000)
//        enableSimWave()
      }



      val cd = dut.clockDomain
      cd.forkStimulus(10)
//      cd.forkSimSpeedPrinter(4.0)

      val tracer = new FileBackend(new File("trace.txt"))
      tracer.spinalSimFlusher(10*10000)

      val naxProbe = new NaxSimProbe(dut.nax.thread.core, 0)
      naxProbe.add(tracer)



      val memAgent = new MemoryAgent(dut.mem.node.bus, cd)(null)
      val peripheralAgent = new PeripheralEmulator(dut.peripheral.emulated.node.bus, cd)

//      val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"))
//      val elf = new Elf(new File("ext/NaxSoftware/baremetal/coremark/build/rv32ima/coremark.elf"))
      val elf = new Elf(new File("ext/NaxSoftware/baremetal/freertosDemo/build/rv32ima/freertosDemo.elf"))
//      val elf = new Elf(new File("ext/NaxSoftware/baremetal/play/build/rv32ima/play.elf"))
      elf.load(memAgent.mem, -0xffffffff00000000l)
      tracer.loadElf(elf.f, 0)
      tracer.setPc(0, 0x80000000)
      val passSymbol = elf.getSymbolAddress("pass")
      val failSymbol = elf.getSymbolAddress("fail")
      naxProbe.commitsCallbacks += { (hartId, pc) =>
        if(pc == passSymbol) delayed(1)(simSuccess())
        if(pc == failSymbol) delayed(1)(simFailure())
      }

          //          memAgent.mem.loadBin(0x80000000l, "ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin")
          //          memAgent.mem.loadBin(0x80F80000l, "ext/NaxSoftware/buildroot/images/rv32ima/linux.dtb")
          //          memAgent.mem.loadBin(0x80400000l, "ext/NaxSoftware/buildroot/images/rv32ima/Image")
          //          memAgent.mem.loadBin(0x81000000l, "ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio")


//          cd.waitSampling(10000000)
//          simSuccess()
    }
  }

  for(i <- 0 until 1) {
    new Thread {
      override def run {
        doIt()
      }
    }.start()
  }
}
