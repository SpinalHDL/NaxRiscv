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
import spinal.lib.bus.tilelink.coherent.{Hub, HubFabric}
import spinal.lib.bus.tilelink.sim.{Checker, Endpoint, MemoryAgent, Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionC, TransactionD}
import spinal.lib.bus.tilelink.{M2sSupport, M2sTransfers, Opcode, S2mSupport, SizeRange, fabric}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug.DebugHartBus
import spinal.lib.misc.{Elf, TilelinkFabricClint}
import spinal.lib.sim.SparseMemory
import spinal.sim.{Signal, SimManagerContext}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import scala.util.Random



class NaxriscvTilelink() extends Area with RiscvHart{
  val ibus = Node.master()
  val dbus = Node.master()
  val pbus = Node.master()

  val buses = List(ibus, dbus, pbus)
  val plugins = Handle[Seq[Plugin]]

  def setPluginsSimple(hartId : Int) : this.type = {
    plugins load Config.plugins(
      withCoherency = true,
      withRdTime = false,
      aluCount = 1,
      decodeCount = 1,
      ioRange = a => a(31 downto 28) === 0x1,
      hartId = hartId
    )
    this
  }
  def setPlugins(p : Seq[Plugin]) : this.type = {
    plugins.load(p)
    this
  }


  def privPlugin = thread.core.framework.getService[PrivilegedPlugin]
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
      case p : PrivilegedPlugin =>
      case _ =>
    }
  }
}





