package naxriscv.platform.litex

import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.execute.CsrTracer
import naxriscv.fetch.FetchCachePlugin
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

  val socClk = in Bool()
  val asyncReset = in Bool()

  val socClkCd = ClockDomain(socClk)
  val socResetCtrl = socClkCd on new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH)

  val system = socResetCtrl.cd on new AreaRoot {
    val mainDataWidth = c.naxPlugins.head.collectFirst { case p: DataCachePlugin => p.memDataWidth }.get

    val naxes = for (p <- naxPlugins) yield new TilelinkNaxRiscvFiber().setPlugins(p)
    for (nax <- naxes) {
      nax.dBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S, b = StreamPipe.HALF, c = StreamPipe.FULL, e = StreamPipe.HALF)
      nax.iBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S)
      nax.pBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)
    }

    val memFilter, ioFilter = new fabric.TransferFilter()
    for (nax <- naxes) {
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
      bridge.up load bus.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.FULL, b = StreamPipe.HALF, r = StreamPipe.FULL)
      bridge.down.setDownConnection(a = StreamPipe.FULL)
      memFilter.up << bridge.down

      //As litex reset will release before our one, we need to ensure that we don't eat a transaction
      Fiber build {
        bridge.read.get
        bridge.write.get
        when(ClockDomain.current.isResetActive){
          bus.ar.ready := False
          bus.aw.ready := False
          bus.w.ready := False
        }
      }
    }


    var nonCoherent: Node = null
    val noL2 = !withL2 generate new Area {
      val hub = new HubFiber()
      hub.up << memFilter.down
      hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
      hub.down.forceDataWidth(mainDataWidth)
      nonCoherent = hub.down
    }

    val l2 = withL2 generate new Area {
      val cache = new CacheFiber()
      cache.parameter.throttleList = naxes.map(_.plugins.collectFirst {case p : DataCachePlugin => p}.get)
      cache.parameter.cacheWays = l2Ways
      cache.parameter.cacheBytes = l2Bytes
      cache.up << memFilter.down
      cache.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL, d = StreamPipe.FULL)
      cache.down.forceDataWidth(mainDataWidth)
      nonCoherent = cache.down
    }

    val withMem = regions.exists(_.onMemory)
    val toAxi4 = withMem generate new fabric.Axi4Bridge
    if (withMem) {
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


      if (withL2) l2.cache.ctrl at 0xF0080000l of bus

      val externalInterrupts = new Area {
        val port = in Bits (32 bits)
        val toPlic = for (i <- 0 to 31) yield (i != 0) generate new Area {
          val node = plic.createInterruptSlave(i)
          node.withUps = false
          node.flag := port(i)
        }
      }


      for (nax <- naxes) {
        nax.bind(clint)
        nax.bind(plic)
      }

      val axiLiteRegions = regions.filter(e => e.onPeripheral && !e.isIo)
      val toAxiLite4 = new fabric.AxiLite4Bridge
      toAxiLite4.up << bus

      val virtualRegions = for (region <- axiLiteRegions) yield new Area with SpinalTagReady {
        def self = this

        new MemoryConnection {
          override def up = toAxiLite4.down
          override def down = self
          override def transformers = Nil
          override def mapping = region.mapping
          populate()
        }

        addTag(new MemoryTransferTag {
          override def get = toAxiLite4.up.m2s.parameters.emits
        })
        if (region.isCachable) addTag(PMA.MAIN)
      }
    }

    val scope = new ScopeFiber() {
      up at 0xF1000000l of peripheral.bus
      lock.retain()

      val filler = Fiber build new Area {
        if (withL2) {
          val l2c = l2.cache.logic.cache
          add(l2c.events.acquire.hit, 0xF00) //acquire is used by data cache
          add(l2c.events.acquire.miss, 0xF04)
          add(l2c.events.getPut.hit, 0xF20) //getPut is used by instruction cache refill and DMA
          add(l2c.events.getPut.miss, 0xF24)
        }
        for ((nax, i) <- naxes.zipWithIndex) nax.plugins.foreach {
          case p: FetchCachePlugin => add(p.logic.refill.fire, i * 0x80 + 0x000)
          case p: DataCachePlugin => {
            add(p.logic.cache.refill.push.fire, i * 0x80 + 0x010)
            add(p.logic.cache.writeback.push.fire && p.logic.cache.writeback.push.dirty, i * 0x80 + 0x014)
            if (withL2) {
              val l2c = l2.cache.logic.cache
              l2c.rework {
                //For each core, generate a L2 d$ miss probe
                val masterSpec = l2c.p.unp.m.masters.find(_.name == p).get
                val masterHit = masterSpec.sourceHit(l2c.ctrl.processStage(l2c.CTRL_CMD).source)
                add((masterHit && l2c.events.acquire.miss).setCompositeName(l2c.events.acquire.miss, s"nax_$i"), i * 0x80 + 0x40)
              }
            }
          }
          case _ =>
        }
        lock.release()
      }
    }

    val mBus = withMem generate (Fiber build master(toAxi4.down.pipelined()))
    val pBus = Fiber build master(peripheral.toAxiLite4.down.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.HALF, b = StreamPipe.HALF, r = StreamPipe.HALF))

    val debug = c.withDebug generate new Area {
      val cd = ClockDomain.current.copy(reset = in Bool())
      val cdi = c.withJtagInstruction generate ClockDomain.external("jtag_instruction", withReset = false)

      val dm = cd(new DebugModuleFiber())
      naxes.foreach(dm.bindHart)
      val tap = c.withJtagTap generate cd(dm.withJtagTap())
      val instruction = c.withJtagInstruction generate cdi(dm.withJtagInstruction())
    }

    val patcher = Fiber build new Area {
      if (c.withDma) {
        Axi4SpecRenamer(dma.bus)
        dma.bridge.down.bus
      }
      if (withMem) Axi4SpecRenamer(mBus.get)
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
      tracer.valid init (False)
      tracer.valid := Delay(trigger, 2)
      tracer.payload init (0)
      for (nax <- naxes) {
        nax.plugins.collectFirst { case p: CsrTracer => p } match {
          case Some(p) => when(p.logic.flowOut.valid) {
            trigger := True
            tracer.payload := p.logic.flowOut.payload
          }
          case None =>
        }
      }
    }
  }
}


object NaxSoc extends App{
//  SpinalVerilog(new NaxSoc(1))
}
