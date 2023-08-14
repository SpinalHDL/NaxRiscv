package naxriscv.platform.tilelinkdemo

import naxriscv.platform.NaxriscvTilelink
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkFabricClint


class SocDemo(cpuCount : Int) extends Component {

  val naxes = for(hartId <- 0 until cpuCount) yield new NaxriscvTilelink(hartId)

  val filter = new fabric.TransferFilter()
  for(nax <- naxes) filter.up << nax.buses


  val nonCoherent = Node()

  //  val hub = new HubFabric()
  //  hub.up << filter.down
  //  nonCoherent << hub.down

  nonCoherent << filter.down

  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node at SizeMapping(0x80000000l, 0x80000000l) of nonCoherent

  val peripheral = new Area {
    val bus = Node()
    bus at 0x10000000l of nonCoherent

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
