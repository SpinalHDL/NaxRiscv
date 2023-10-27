package naxriscv.platform.asic

import naxriscv.{Config, NaxRiscv}
import naxriscv.compatibility.{EnforceSyncRamPhase, MemReadDuringWriteHazardPhase, MemReadDuringWritePatcherPhase, MultiPortWritesSymplifier}
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.fetch.FetchCachePlugin
import naxriscv.lsu.DataCachePlugin
import naxriscv.prediction.{BtbPlugin, GSharePlugin}
import naxriscv.utilities.DocPlugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.Rtl

object test_c extends App{
  LutInputs.set(4)
  def plugins = {
    val l = Config.plugins(
      asic = true,
      withRdTime = false,
      aluCount    = 1,
      decodeCount = 1,
      debugTriggers = 4,
      withDedicatedLoadAgu = false,
      withRvc = false,
      withLoadStore = false,
      withMmu = false,
      withDebug = false,
      withEmbeddedJtagTap = false,
      jtagTunneled = false,
      withFloat = false,
      withDouble = false,
      withLsu2 = true,
      lqSize = 8,
      sqSize = 8,
      dispatchSlots = 8,
      robSize = 16,
      branchCount = 4,
      //      withCoherency = true,
      ioRange = a => a(31 downto 28) === 0x1// || !a(12)//(a(5, 6 bits) ^ a(12, 6 bits)) === 51
    )
    l.foreach{
      case p : EmbeddedJtagPlugin => p.debugCd.load(ClockDomain.current.copy(reset = Bool().setName("debug_reset")))
      case p: FetchCachePlugin => p.wayCount = 2; p.cacheSize = 4096; p.memDataWidth = 64
      case p: DataCachePlugin => p.wayCount = 2; p.cacheSize = 4096; p.memDataWidth = 64
      case p: BtbPlugin => p.entries = 64
      case p: GSharePlugin => p.memBytes = 512

//      case p: FetchCachePlugin => p.wayCount = 1; p.cacheSize = 256; p.memDataWidth = 64
//      case p: DataCachePlugin => p.wayCount = 1; p.cacheSize = 256; p.memDataWidth = 64
//      case p: BtbPlugin => p.entries = 8
//      case p: GSharePlugin => p.memBytes = 32
      case _ =>
    }
    l
  }

//  val spinalConfig = SpinalConfig()
  val spinalConfig = SpinalSky130()
  //spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
//  spinalConfig.addStandardMemBlackboxing(blackboxAllWhatsYouCan)

  spinalConfig.generateVerilog(new NaxRiscv(plugins).setDefinitionName("nax"))

//  spinalConfig.generateVerilog(new StreamFifo(UInt(4 bits), 256).setDefinitionName("nax"))
}
