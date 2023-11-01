package naxriscv.platform.tilelinkdemo

import naxriscv.lsu.DataCachePlugin
import naxriscv.platform.{RvlsBackend, TilelinkNaxRiscvFiber}
import riscv.model.Model
import spinal.core._
import spinal.core.fiber._
import spinal.lib.StreamPipe
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkClintFiber
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA

// SocDemo is a little SoC made only for simulation purposes.
class SocDemo(cpuCount : Int, withL2 : Boolean = true, asic : Boolean = false) extends Component {
  // Create a few NaxRiscv cpu
  val naxes = for(hartId <- 0 until cpuCount) yield new TilelinkNaxRiscvFiber().setCoherentConfig(hartId, asic = asic)

  // As NaxRiscv may emit memory request to some unmapped memory space, we need to catch those with TransactionFilter
  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.iBus, nax.dBus)
    ioFilter.up << List(nax.pBus)
  }

  // Handle memory coherency
  var nonCoherent: Node = null

  val noL2 = !withL2 generate new Area {
    val hub = new HubFiber()
    hub.up << memFilter.down
    nonCoherent = hub.down
  }

  val l2 = withL2 generate new Area {
    val cache = new CacheFiber()
    cache.parameter.throttleList = naxes.map(_.plugins.collectFirst {case p : DataCachePlugin => p}.get)
    cache.parameter.cacheWays = 4
    cache.parameter.cacheBytes = 128 * 1024
    cache.up << memFilter.down
    nonCoherent = cache.down
  }

  // Create a tilelink memory bus which will get out of the SoC to connect the main memory
  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node at SizeMapping(0x80000000l, 0x80000000l) of nonCoherent
  mem.node.addTag(PMA.MAIN)

  // Handle all the IO / Peripheral things
  val peripheral = new Area {
    val bus = Node()
    bus at (0x10000000l, 0x10000000l)  of (ioFilter.down)

    val clint = new TilelinkClintFiber()
    clint.node at 0x10000 of bus

    val plic = new TilelinkPlicFiber()
    plic.node at 0xC00000l of bus

    if(withL2) l2.cache.ctrl at 0x20000 of bus

    for(nax <- naxes) {
      nax.bind(clint)
      nax.bind(plic)
    }

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
      naxes.foreach{ hart =>
        hart.getIntMachineExternal() setWhen mei
        hart.getIntSupervisorExternal() setWhen sei
      }
    }
  }
}

object SocDemo extends App{
  SpinalVerilog(new SocDemo(2))
}
