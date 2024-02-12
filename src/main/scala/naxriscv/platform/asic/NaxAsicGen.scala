package naxriscv.platform.asic

import naxriscv.{Config, NaxRiscv}
import naxriscv.compatibility.{CombRamBlackboxer, EnforceSyncRamPhase, MemReadDuringWriteHazardPhase, MemReadDuringWritePatcherPhase, MultiPortWritesSymplifier}
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.fetch.FetchCachePlugin
import naxriscv.lsu.DataCachePlugin
import naxriscv.lsu2.Lsu2Plugin
import naxriscv.prediction.{BtbPlugin, GSharePlugin}
import naxriscv.utilities.DocPlugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.Rtl

object NaxAsicGen extends App{
  var ramBlocks = "inferred"
  var regFileFakeRatio = 1
  var withLsu = true
  var withIoFf = false
  var withRfLatchRam = true
  var blackBoxCombRam = false

  assert(new scopt.OptionParser[Unit]("NaxAsicGen") {
    help("help").text("prints this usage text")
    opt[Unit]("sky130-ram") action { (v, c) => ramBlocks = "sky130" }
    opt[Int]("regfile-fake-ratio") action { (v, c) => regFileFakeRatio = v }
    opt[Unit]("no-lsu") action { (v, c) => withLsu = false }
    opt[Unit]("io-ff") action { (v, c) => withIoFf = true }
    opt[Unit]("no-rf-latch-ram") action { (v, c) => withRfLatchRam = false }
    opt[Unit]("bb-comb-ram") action { (v, c) => blackBoxCombRam = true }
  }.parse(args, Unit).nonEmpty)


  LutInputs.set(4)
  def plugins = {
    val l = Config.plugins(
      asic = true,
      withRfLatchRam = withRfLatchRam,
      withRdTime = false,
      aluCount    = 1,
      decodeCount = 1,
      debugTriggers = 4,
      withDedicatedLoadAgu = false,
      withRvc = false,
      withLoadStore = withLsu,
      withMmu = withLsu,
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
      mmuSets = 4,
      regFileFakeRatio = regFileFakeRatio,
      //      withCoherency = true,
      ioRange = a => a(31 downto 28) === 0x1// || !a(12)//(a(5, 6 bits) ^ a(12, 6 bits)) === 51
    )

    l.foreach{
      case p : EmbeddedJtagPlugin => p.debugCd.load(ClockDomain.current.copy(reset = Bool().setName("debug_reset")))
      case _ =>
    }

    ramBlocks match {
      case "inferred" => l.foreach {
        case p: FetchCachePlugin => p.wayCount = 1; p.cacheSize = 256; p.memDataWidth = 64
        case p: DataCachePlugin => p.wayCount = 1; p.cacheSize = 256; p.memDataWidth = 64
        case p: BtbPlugin => p.entries = 8
        case p: GSharePlugin => p.memBytes = 32
        case p: Lsu2Plugin => p.hitPedictionEntries = 64
        case _ =>
      }
      case "sky130" => l.foreach {
        case p: FetchCachePlugin => p.wayCount = 2; p.cacheSize = 4096; p.memDataWidth = 64
        case p: DataCachePlugin => p.wayCount = 2; p.cacheSize = 4096; p.memDataWidth = 64
        case p: BtbPlugin => p.entries = 64
        case p: GSharePlugin => p.memBytes = 512
        case p: Lsu2Plugin => p.hitPedictionEntries = 64
        case _ =>
      }
    }
    l
  }

  var spinalConfig = ramBlocks match {
    case "inferred" => SpinalConfig()
    case "sky130" => SpinalSky130()
  }

  if(blackBoxCombRam) spinalConfig.memBlackBoxers += new CombRamBlackboxer()

  def gen = new NaxRiscv(plugins).setDefinitionName("nax")
  spinalConfig.generateVerilog(if(withIoFf) Rtl.ffIo(gen) else gen)

//  spinalConfig.generateVerilog(new StreamFifo(UInt(4 bits), 256).setDefinitionName("nax"))
}
