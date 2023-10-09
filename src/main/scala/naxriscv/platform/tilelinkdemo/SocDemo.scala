package naxriscv.platform.tilelinkdemo

import naxriscv.platform.{JniBackend, TilelinkNaxRiscvFiber}
import riscv.model.Model
import spinal.core._
import spinal.core.fiber._
import spinal.lib.StreamPipe
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{DirectoryFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkClintFiber
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA


class SocDemo(cpuCount : Int) extends Component {
  val naxes = for(hartId <- 0 until cpuCount) yield new TilelinkNaxRiscvFiber().setCoherentConfig(hartId)

  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.iBus, nax.dBus)
    ioFilter.up << List(nax.pBus)
  }

  val nonCoherent = Node()

  //  val hub = new HubFiber()
  val hub = new DirectoryFiber()
  hub.parameter.cacheWays = 4
  hub.parameter.cacheBytes = 128*1024
  hub.up << memFilter.down
  nonCoherent << hub.down


//  nonCoherent << memFilter.down

  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node at SizeMapping(0x80000000l, 0x80000000l) of nonCoherent
  mem.node.addTag(PMA.MAIN)

  val peripheral = new Area {
    val bus = Node()
    bus at (0x10000000l, 0x10000000l)  of (ioFilter.down)

    val clint = new TilelinkClintFiber()
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
  f.trap(0, false, 42)
}




class SocDemoSmall(cpuCount : Int) extends Component {
  val naxes = List.fill(cpuCount)(new TilelinkNaxRiscvFiber())
  for((nax, hartId) <- naxes.zipWithIndex) nax.setCoherentConfig(hartId)

  val hub = new HubFiber()
  for(nax <- naxes)  hub.up << (nax.iBus, nax.dBus)

  val mem = new tilelink.fabric.SlaveBusAny()
  mem.node at SizeMapping(0x80000000l, 64 kB) of (hub.down)

  val peripheral = new Area {
    val bus = Node()

    val clint = new TilelinkClintFiber()
    clint.node at 0x10000 of bus

    val plic = new TilelinkPlicFiber()
    plic.node at 0xC00000l of bus

    for(nax <- naxes) {
      nax.bind(clint)
      nax.bind(plic)
      bus at(0x10000000l, 16 MB) of(nax.pBus)
    }
  }

  peripheral.bus.setUpConnection(a = StreamPipe.FULL)
}

object SocDemoSmallGen extends App{
  SpinalVerilog(new SocDemoSmall(2))
}