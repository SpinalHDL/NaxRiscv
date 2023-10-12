package naxriscv.platform.litex

import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.execute.CsrTracer
import naxriscv.lsu.DataCachePlugin
import naxriscv.platform.TilelinkNaxRiscvFiber
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer, Axi4ToTilelinkFiber}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}
import spinal.lib.bus.misc.{OffsetTransformer, OrMapping, SizeMapping}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.misc.{InterruptNode, TilelinkClintFiber}
import spinal.lib.system.tag.{MappedNode, MemoryConnection, MemoryTransferTag, MemoryTransfers, PMA}

import scala.collection.mutable.ArrayBuffer

class NaxSocConfig(){
  var naxPlugins : Seq[Seq[Plugin]] = null
  val regions = ArrayBuffer[LitexMemoryRegion]()
  var withJtagTap = false
  var withJtagInstruction = false
  var withDebug = false
  var withDma = false
  var mBusWidth = 64
  var l2Bytes = 128*1024
  var l2Ways = 8
  def withL2 = l2Bytes > 0
}

class NaxSoc(c : NaxSocConfig) extends Component{
  import c._
  val mainDataWidth = c.naxPlugins.head.collectFirst { case p: DataCachePlugin => p.memDataWidth }.get

  val naxes = for(p <- naxPlugins) yield new TilelinkNaxRiscvFiber().setPlugins(p)
  for(nax <- naxes){
    nax.dBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S, b = StreamPipe.HALF, c = StreamPipe.FULL, e = StreamPipe.HALF)
    nax.iBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S)
    nax.pBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)
  }

  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.iBus, nax.dBus)
    ioFilter.up << List(nax.pBus)
  }

  val dma = c.withDma generate new Area {
    val bus = slave(
      Axi4(
        Axi4Config(
          addressWidth = 32,
          dataWidth = mainDataWidth,
          idWidth = 4
        )
      )
    )

    val bridge = new Axi4ToTilelinkFiber(64, 4)
    bridge.up load bus
    bridge.down.setDownConnection(a = StreamPipe.FULL)
    memFilter.up << bridge.down
  }


  var nonCoherent : Node = null
  val noL2 = !withL2 generate new Area {
    val hub = new HubFiber()
    hub.up << memFilter.down
    hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
    hub.down.forceDataWidth(mainDataWidth)
    nonCoherent = hub.down
  }

  val l2 = withL2 generate new Area {
    val cache = new CacheFiber()
    cache.parameter.cacheWays = 4
    cache.parameter.cacheBytes = 128 * 1024
    cache.up << memFilter.down
    cache.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
    cache.down.forceDataWidth(mainDataWidth)
    nonCoherent = cache.down
  }

  val withMem = regions.exists(_.onMemory)
  val toAxi4 = withMem generate new fabric.Axi4Bridge
  if(withMem) {
    toAxi4.up.forceDataWidth(mBusWidth)
    toAxi4.down.addTag(PMA.MAIN)
    regions.filter(_.onMemory).foreach(r =>
      toAxi4.up at r.mapping of nonCoherent
    )
  }



  val peripheral = new Area {
    val bus = Node()
    bus << (nonCoherent, ioFilter.down)
    bus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)
    bus.forceDataWidth(32)

    val clint = new TilelinkClintFiber()
    clint.node at 0xF0010000l of bus

    val plic = new TilelinkPlicFiber()
    plic.node at 0xF0C00000l of bus

    val externalInterrupts = new Area {
      val port = in Bits (32 bits)
      val toPlic = for (i <- 0 to 31) yield (i != 0) generate new Area {
        val node = plic.createInterruptSlave(i)
        node.withUps = false
        node.flag := port(i)
      }
    }


    for(nax <- naxes) {
      nax.bind(clint)
      nax.bind(plic)
    }

    val axiLiteRegions = regions.filter(e => e.onPeripheral && !e.isIo)
    val toAxiLite4 = new fabric.AxiLite4Bridge
    toAxiLite4.up at(OrMapping(axiLiteRegions.map(_.mapping))) of bus
//    toAxiLite4.up.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

    val virtualRegions = for(region <- axiLiteRegions) yield new Area with SpinalTagReady{
      def self = this

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

  val mBus = withMem generate (Fiber build master(toAxi4.down.pipelined()))
  val pBus = Fiber build master(peripheral.toAxiLite4.down.pipelined(ar=StreamPipe.HALF, aw=StreamPipe.HALF, w=StreamPipe.HALF, b=StreamPipe.HALF, r=StreamPipe.HALF))

  val debug = c.withDebug generate new Area{
    val cd = ClockDomain.current.copy(reset = in Bool())
    val cdi = c.withJtagInstruction generate ClockDomain.external("jtag_instruction", withReset = false)

    val dm = cd(new DebugModuleFiber())
    naxes.foreach(dm.bindHart)
    val tap = c.withJtagTap generate cd(dm.withJtagTap())
    val instruction = c.withJtagInstruction generate cdi(dm.withJtagInstruction())
  }

  val patcher = Fiber build new Area{
    if(c.withDma) {
      Axi4SpecRenamer(dma.bus)
      dma.bridge.down.bus
    }
    if(withMem) Axi4SpecRenamer(mBus.get)
    AxiLite4SpecRenamer(pBus.get)

    naxes(0).dBus.bus


    val i = MemoryConnection.getMemoryTransfers(naxes(0).iBus)
    val d = MemoryConnection.getMemoryTransfers(naxes(0).dBus)
    val p = MemoryConnection.getMemoryTransfers(naxes(0).pBus)


    if (withJtagTap) debug.tap.jtag.setName("jtag")
    if (withJtagInstruction) debug.instruction.setName("jtag_instruction")
    if (c.withDebug) {
      debug.dm.ndmreset.toIo().setName("debug_ndmreset")
      debug.cd.reset.setName("debug_reset")
    }

    val tracer = master(Reg(Flow(Bits(8 bits))))
    val trigger = False
    tracer.valid init(False)
    tracer.valid := Delay(trigger, 2)
    tracer.payload init(0)
    for(nax <- naxes) {
      nax.plugins.collectFirst { case p: CsrTracer => p } match {
        case Some(p) => when(p.logic.flowOut.valid){
          trigger := True
          tracer.payload := p.logic.flowOut.payload
        }
        case None =>
      }
    }
  }
}


object NaxSoc extends App{
//  SpinalVerilog(new NaxSoc(1))
}
