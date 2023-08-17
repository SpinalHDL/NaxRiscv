package naxriscv.platform.litex

import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.platform.NaxriscvTilelink
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}
import spinal.lib.bus.misc.{OffsetTransformer, OrMapping, SizeMapping}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.HubFabric
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkFabricClint
import spinal.lib.system.tag.{MappedNode, MemoryConnection, MemoryTransferTag, MemoryTransfers, PMA}

import scala.collection.mutable.ArrayBuffer

class NaxSoc(naxPlugins : Seq[Seq[Plugin]], regions : ArrayBuffer[LitexMemoryRegion]) extends Component{
  val naxes = for(p <- naxPlugins) yield new NaxriscvTilelink().setPlugins(p)

  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.ibus, nax.dbus)
    ioFilter.up << List(nax.pbus)
  }

  val hub = new HubFabric()
  hub.up << memFilter.down

  val toAxi4 = new fabric.Axi4Bridge
  regions.filter(_.onMemory).foreach(r =>
    toAxi4.up at r.mapping of hub.down
  )
  toAxi4.down.addTag(PMA.MAIN)

  val peripheral = new Area {
    val bus = Node()
    bus << (hub.down, ioFilter.down)

    val clint = new TilelinkFabricClint()
    clint.node at 0xF0010000l of bus

    for(nax <- naxes) clint.bindHart(nax)

    val toAxiLite4 = new fabric.AxiLite4Bridge
    toAxiLite4.up at(OrMapping(regions.filter(_.onPeripheral).map(_.mapping))) of bus
    toAxiLite4.up.forceDataWidth(32)
    toAxiLite4.up.setDecoderConnection(_.connectFrom(_)(d=StreamPipe.M2S))
//    regions.filter(_.onPeripheral).foreach(r =>
//      toAxiLite4.up at r.mapping of bus
//    )

    val virtualRegions = for(region <- regions if region.onPeripheral) yield new Area with SpinalTagReady{
      val self = this

      new MemoryConnection {
        override def m = toAxiLite4.down
        override def s = self
        override def transformers = List(OffsetTransformer(region.mapping.lowerBound))
        override def mapping = region.mapping
        override def sToM(downs: MemoryTransfers, args: MappedNode) = downs
        populate()
      }

      addTag(new MemoryTransferTag {
        override def get = toAxiLite4.up.m2s.parameters.emits
      })
      if(region.isCachable) addTag(PMA.MAIN)
    }
  }

  val mBus = Fiber build master(toAxi4.down.pipelined())
  val pBus = Fiber build master(peripheral.toAxiLite4.down.pipelined())

  val patcher = Fiber build new Area{
    Axi4SpecRenamer(mBus.get)
    AxiLite4SpecRenamer(pBus.get)

    naxes.foreach{hart =>
      hart.getIntMachineExternal() := False
      hart.getIntSupervisorExternal() := False
    }

    val i = MemoryConnection.getMemoryTransfers(naxes(0).ibus)
    val d = MemoryConnection.getMemoryTransfers(naxes(0).dbus)
    val p = MemoryConnection.getMemoryTransfers(naxes(0).pbus)

    naxes(0).thread.core.plugins.foreach {
      case p: EmbeddedJtagPlugin => {
        if (p.withTap) p.logic.jtag.toIo().setName("jtag")
        else p.logic.jtagInstruction.toIo().setName("jtag_instruction")
        p.logic.ndmreset.toIo().setName("debug_ndmreset")
      }
      case _ =>
    }

    println(p)
  }
}


object NaxSoc extends App{
//  SpinalVerilog(new NaxSoc(1))
}
