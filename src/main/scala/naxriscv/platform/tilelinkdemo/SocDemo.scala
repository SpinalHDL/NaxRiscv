package naxriscv.platform.tilelinkdemo

import naxriscv.platform.{JniBackend, NaxriscvTilelink}
import riscv.model.Model
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.HubFabric
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkFabricClint
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA


class SocDemo(cpuCount : Int) extends Component {
  val naxes = for(hartId <- 0 until cpuCount) yield new NaxriscvTilelink().setPluginsSimple(hartId)

  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.iBus, nax.dBus)
    ioFilter.up << List(nax.pBus)
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

    val plic = new TilelinkPlicFiber()
    plic.node at 0xC00000l of bus

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

object Jni extends App{
  val m = new Model
  println(m.newModel())
}

object RvlsTest extends App{
  val f = new JniBackend
  f.newCpuMemoryView(0, 1, 2)
  f.newCpu(0,"RV32IMA", "MSU", 32, 0);
  try {
    f.commit(0, 0x44)
  }catch {
    case _ => println("MIAOUUUU")
  }
}