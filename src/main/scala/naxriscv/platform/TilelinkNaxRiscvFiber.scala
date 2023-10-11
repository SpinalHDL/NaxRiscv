package naxriscv.platform

import naxriscv.Global.XLEN
import naxriscv.utilities.Plugin
import naxriscv.lsu2._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv._
import naxriscv.execute.CsrAccessPlugin
import naxriscv.execute.fpu.FpuSettingPlugin
import naxriscv.frontend.DecoderPlugin
import naxriscv.lsu.DataCachePlugin
import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader}
import spinal.core
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.{Hub, HubFiber}
import spinal.lib.bus.tilelink.sim.{Checker, Endpoint, MemoryAgent, Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionC, TransactionD}
import spinal.lib.bus.tilelink.{M2sSupport, M2sTransfers, Opcode, S2mSupport, SizeRange, fabric}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug.DebugHartBus
import spinal.lib.misc.plic.InterruptCtrlFiber
import spinal.lib.misc.{ClintPort, Elf, InterruptCtrl, InterruptNode, TilelinkClintFiber}
import spinal.lib.sim.SparseMemory
import spinal.sim.{Signal, SimManagerContext}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer



class TilelinkNaxRiscvFiber() extends Area with RiscvHart{
  val iBus = Node.master()
  val dBus = Node.master()
  val pBus = Node.master()
  val buses = List(iBus, dBus, pBus)
  val plugins = Handle[Seq[Plugin]]

  val icfs = ArrayBuffer[InterruptCtrlFiber]()
  def bind(icf : InterruptCtrlFiber): Unit = {
    icf.retain()
    icfs += icf
  }

  val clint = Handle[TilelinkClintFiber]
  def bind(clint : TilelinkClintFiber): Unit = {
    clint.lock.retain()
    this.clint load clint
  }

  def setCoherentConfig(hartId : Int) : this.type = {
    plugins load Config.plugins(
      withCoherency = true,
      withRdTime = false,
      aluCount = 2,
      decodeCount = 2,
      ioRange = a => a(31 downto 28) === 0x1,
      hartId = hartId
    )
    this
  }
  def setPlugins(p : Seq[Plugin]) : this.type = {
    plugins.load(p)
    this
  }


  def privPlugin : PrivilegedPlugin = thread.core.framework.getService[PrivilegedPlugin]
  override def getXlen() = thread.core.framework.getService[DecoderPlugin].xlen
  override def getFlen() = thread.core.framework.getServiceOption[FpuSettingPlugin] match {
    case Some(x) => x.rvd.toInt*64 max x.rvf.toInt*32
    case None => 0
  }
  override def getHartId() = privPlugin.p.hartId
  override def getIntMachineTimer() = privPlugin.io.int.machine.timer
  override def getIntMachineSoftware() = privPlugin.io.int.machine.software
  override def getIntMachineExternal() = privPlugin.io.int.machine.external
  override def getIntSupervisorExternal() = privPlugin.io.int.supervisor.external
  override def getDebugBus(): DebugHartBus = privPlugin.setup.debugBus

  val thread = Fiber build new Area{
    val l = ArrayBuffer[Plugin]()
    l ++= plugins

    val privPlugin = plugins.collectFirst{case p : PrivilegedPlugin => p }.get
    val intIdPerHart = 1+privPlugin.p.withSupervisor.toInt
    val mei = new Area{
      val node = InterruptNode.slave()
      val drivers = icfs.map(_.createInterruptMaster(privPlugin.p.hartId*intIdPerHart))
      drivers.foreach(node << _)
    }

    val sei = privPlugin.p.withSupervisor generate new Area{
      val node = InterruptNode.slave()
      val drivers = icfs.map(_.createInterruptMaster(privPlugin.p.hartId*intIdPerHart+1))
      drivers.foreach(node << _)
    }

    icfs.foreach(_.release())

    val clintPort = clint.createPort(privPlugin.p.hartId)
    val mti, msi = InterruptNode.slave()
    mti << clintPort.mti
    msi << clintPort.msi
    clint.lock.release()

    // Add a plugin to Nax which will handle the negotiation of the tilelink parameters
    l += new Plugin {
      val setup = create early new Area{
        framework.plugins.foreach{
          case p : FetchCachePlugin => iBus.m2s.forceParameters(p.getBusParameter().toTileLinkM2sParameters())
          case p : DataCachePlugin => dBus.m2s.forceParameters(p.setup.dataCacheParameters.memParameter.toTileLinkM2sParameters())
          case p : Lsu2Plugin => pBus.m2s.forceParameters(p.getPeripheralBusParameters().toTileLinkM2sParameters())
          case _ =>
        }

        framework.plugins.foreach{
          case p : FetchCachePlugin => iBus.s2m.supported.load(S2mSupport.none())
          case p : Lsu2Plugin => pBus.s2m.supported.load(S2mSupport.none())
          case p: DataCachePlugin => p.withCoherency match {
            case false => dBus.s2m.supported.load(S2mSupport.none())
            case true => dBus.s2m.supported.load(p.setup.dataCacheParameters.toTilelinkS2mSupported(dBus.s2m.proposed))
          }
          case _ =>
        }

        framework.plugins.foreach{
          case p: DataCachePlugin =>
            if(p.withCoherency)p.setCoherencyInfo(dBus.m2s.parameters.sourceWidth, dBus.s2m.parameters.sinkWidth)
          case _ =>
        }
      }
    }


    val core = new NaxRiscv(l)
    core.plugins.foreach{
      case p : FetchCachePlugin => iBus.bus << p.mem.toTilelink()
      case p : DataCachePlugin =>  dBus.bus << p.mem.toTilelink()
      case p : Lsu2Plugin => pBus.bus << p.peripheralBus.toTilelink()
      case p : PrivilegedPlugin => {
        p.io.int.machine.timer := mti.flag
        p.io.int.machine.software := msi.flag
        p.io.int.machine.external := mei.node.flag
        if(p.p.withSupervisor) p.io.int.supervisor.external := sei.node.flag
        if(p.p.withRdTime) p.io.rdtime := clint.thread.core.io.time
      }
      case _ =>
    }
  }
}





