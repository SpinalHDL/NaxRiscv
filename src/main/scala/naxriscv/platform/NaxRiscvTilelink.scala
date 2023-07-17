package naxriscv.platform

import naxriscv.utilities.Plugin
import naxriscv.lsu2._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv._
import naxriscv.lsu.DataCachePlugin
import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader}
import spinal.core
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.sim.{MemoryAgent, Monitor, MonitorSubscriber, SlaveDriver, TransactionA, TransactionD}
import spinal.lib.bus.tilelink.{M2sSupport, M2sTransfers, Opcode, S2mSupport, SizeRange, fabric}
import spinal.lib.misc.{Elf, TilelinkFabricClint}
import spinal.lib.sim.SparseMemory

import java.io.File
import java.nio.file.Files

class NaxRiscvTilelink extends Area {
  val ibus = Node.master()
  val dbus = Node.master()
  val pbus = Node.master()

  val buses = List(ibus, dbus, pbus)

  val thread = Fiber build new Area{
    val l = Config.plugins(
//      withCoherency = true,
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
          case p : DataCachePlugin => dbus.s2m.supported.load(S2mSupport.none())
          case p : Lsu2Plugin => pbus.s2m.supported.load(S2mSupport.none())
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
        p.io.int.machine.timer := False
        p.io.int.machine.software := False
        p.io.int.machine.external := False
        p.io.int.supervisor.external := False
      }
      case _ =>
    }
  }
}



class NaxRiscvTilelinkSoCDemo extends Component {
  val filter = new fabric.TransferFilter()

  val nax = new NaxRiscvTilelink()
  filter.up << nax.buses

  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node << filter.down

  val peripheral = new Area {
    val bus = Node()
    bus at 0x10000000l of filter.down

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

object NaxRiscvTilelinkSim extends App{
  import spinal.core.sim._
  val sc = SimConfig
  sc.allOptimisation
//  sc.withFstWave

  val compiled = sc.compile(new NaxRiscvTilelinkSoCDemo())

  compiled.doSimUntilVoid{dut =>
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.forkSimSpeedPrinter(4.0)

    val memAgent = new MemoryAgent(dut.mem.node.bus, cd)(null)
    val peripheralAgent = new PeripheralEmulator(dut.peripheral.emulated.node.bus, cd)

//    val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"))
//    elf.load(memAgent.mem, -0xffffffff00000000l)

    memAgent.mem.loadBin(0x80000000l, "ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin")
    memAgent.mem.loadBin(0x80F80000l, "ext/NaxSoftware/buildroot/images/rv32ima/linux.dtb")
    memAgent.mem.loadBin(0x80400000l, "ext/NaxSoftware/buildroot/images/rv32ima/Image")
    memAgent.mem.loadBin(0x81000000l, "ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio")

    cd.waitSampling(10000000)
    simSuccess()
  }
}
