package naxriscv.platform.litex

import naxriscv.platform.NaxriscvTilelink
import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.HubFabric
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkFabricClint
import spinal.lib.system.tag.PMA

class NaxSoc(cpuCount : Int) extends Component{
  val naxes = for(hartId <- 0 until cpuCount) yield new NaxriscvTilelink(hartId)

  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.ibus, nax.dbus)
    ioFilter.up << List(nax.pbus)
  }

  val hub = new HubFabric()
  hub.up << memFilter.down

  val toAxi4 = new fabric.Axi4Bridge
  toAxi4.up at SizeMapping(0x80000000l, 0x80000000l) of hub.down
  toAxi4.down.addTag(PMA.MAIN)


  val peripheral = new Area {
    val bus = Node()
    bus at (0x10000000l, 0x10000000l) of (hub.down, ioFilter.down)

    val clint = new TilelinkFabricClint()
    clint.node at 0x10000 of bus

    for(nax <- naxes) clint.bindHart(nax)

    val toAxiLite4 = new fabric.AxiLite4Bridge
    toAxiLite4.up << bus
  }

  val mem = Fiber build master(toAxi4.down.pipelined())
  val csr = Fiber build master(peripheral.toAxiLite4.down.pipelined())

  val patcher = Fiber build new Area{
    Axi4SpecRenamer(mem.get)
    AxiLite4SpecRenamer(csr.get)

    naxes.foreach{hart =>
      hart.getIntMachineExternal() := False
      hart.getIntSupervisorExternal() := False
    }
  }
}


object NaxSoc extends App{
  SpinalVerilog(new NaxSoc(1))
}
