package naxriscv.platform.tilelinkdemo

import naxriscv.platform.NaxriscvTilelink
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.HubFabric
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkFabricClint
import spinal.lib.system.tag.PMA


class SocDemo(cpuCount : Int) extends Component {
  val naxes = for(hartId <- 0 until cpuCount) yield new NaxriscvTilelink().setPluginsSimple(hartId)

  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.ibus, nax.dbus)
    ioFilter.up << List(nax.pbus)
  }

  val nonCoherent = Node()

  val hub = new HubFabric()
  hub.up << memFilter.down
  nonCoherent << hub.down

  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node at SizeMapping(0x80000000l, 0x80000000l) of nonCoherent
  mem.node.addTag(PMA.MAIN)

  val peripheral = new Area {
    val bus = Node()
    bus at (0x10000000l, 0x10000000l)  of (hub.down, ioFilter.down)

    val clint = new TilelinkFabricClint()
    clint.node at 0x10000 of bus

    for(nax <- naxes) clint.bindHart(nax)


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

    val custom = Fiber build new Area{
      val mei,sei = in Bool()
      clint.harts.foreach{hart =>
        hart.getIntMachineExternal() := mei
        hart.getIntSupervisorExternal() := sei
      }
    }
  }
}

object SocDemo extends App{
  SpinalVerilog(new SocDemo(2))
}
