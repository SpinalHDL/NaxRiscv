package naxriscv.platform

import naxriscv.utilities.Plugin
import naxriscv.lsu2._
import naxriscv.fetch._
import naxriscv.misc._
import naxriscv._
import naxriscv.lsu.DataCachePlugin
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{M2sSupport, S2mSupport}

class NaxRiscvTilelink extends Area {
  val ibus = Node.master()
  val dbus = Node.master()
  val pbus = Node.master()

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


    val nax = new NaxRiscv(l)
    nax.plugins.foreach{
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



object FiberedDemo extends App{
  SpinalVerilog(new Component{
    val n = Node()

    val nax = new NaxRiscvTilelink()
    n << nax.ibus
    n << nax.dbus
    n << nax.pbus

    val mem = new tilelink.fabric.SlaveBusAny()
    mem.node << n
  })
}