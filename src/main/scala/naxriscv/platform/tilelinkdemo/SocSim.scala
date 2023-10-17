package naxriscv.platform.tilelinkdemo

import naxriscv.fetch.FetchCachePlugin
import naxriscv.lsu.DataCachePlugin
import naxriscv.platform.{FileBackend, RvlsBackend, NaxriscvProbe, NaxriscvTilelinkProbe, PeripheralEmulator}
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent}
import spinal.lib.misc.Elf
import spinal.lib.misc.test.{DualSimTracer, MultithreadedFunSuite, MultithreadedTester}

import java.io.File
import scala.collection.mutable.ArrayBuffer


object SocSim extends App {
  val runLinux = true
  val dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  val traceIt = false

  val sc = SimConfig
  sc.normalOptimisation
  sc.withFstWave
  sc.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC)).includeSimulation)
  sc.addSimulatorFlag("--threads 1")
//  sc.addSimulatorFlag("--prof-exec")

  // Tweek the toplevel a bit
  class SocDemoSim(cpuCount : Int) extends SocDemo(cpuCount){
    setDefinitionName("SocDemo")
    val dcache = naxes(0).plugins.collectFirst { case p: DataCachePlugin => p }.get
    val icache = naxes(0).plugins.collectFirst { case p: FetchCachePlugin => p }.get

    // You can for instance override cache parameters of cpu 0 like that :
    // dcache.cacheSize = 2048
    // icache.cacheSize = 2048

    // hub.parameter.cacheBytes = 4096
  }
  val compiled = sc.compile(new SocDemoSim(cpuCount = 1))

  // How we want to run the test
  dualSim match {
    case true => DualSimTracer.withCb(compiled, window = 50000*10, seed = 2)(testIt)
    case false => compiled.doSimUntilVoid(name = s"test", seed = 2){dut => disableSimWave(); testIt(dut, f => if(traceIt) f)}
  }

  // Testbench code
  def testIt(dut : SocDemoSim, onTrace : (=> Unit) => Unit = cb => {}): Unit = {
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    // cd.forkSimSpeedPrinter(1.0)

    // Connect the few peripherals
    val ma = new MemoryAgent(dut.mem.node.bus, cd, seed = 0, randomProberFactor = 0.2f)(null) {
      mem.randOffset = 0x80000000l
      driver.driver.setFactor(0.8f)
      val checker = if(monitor.bus.p.withBCE) Checker(monitor)
    }
    val pa = new PeripheralEmulator(dut.peripheral.emulated.node.bus, dut.peripheral.custom.mei, dut.peripheral.custom.sei, cd)

    // Rvls will check that the CPUs are doing things right
    val rvls = new RvlsBackend(new File(compiled.compiledPath, currentTestName))
    rvls.spinalSimFlusher(10 * 10000)
    rvls.spinalSimTime(10000)

    // Collect traces from the CPUs behaviour
    val naxes = dut.naxes.map(nax => new NaxriscvTilelinkProbe(nax, nax.getHartId()))
    naxes.foreach(_.add(rvls))

    // Things to enable when we want to collect traces
    onTrace{
      enableSimWave()
      rvls.debug()

      val tracerFile = new FileBackend(new File(new File(compiled.compiledPath, currentTestName), "tracer.log"))
      tracerFile.spinalSimFlusher(10 * 10000)
      tracerFile.spinalSimTime(10000)
      naxes.foreach { hart =>
        hart.add(tracerFile)
        val r = hart.backends.reverse
        hart.backends.clear()
        hart.backends ++= r
      }
    }

    // Load the binaries
    if(runLinux){
      def load(offset : Long, file : String) = {
        ma.mem.loadBin(offset-0x80000000l, file)
        rvls.loadBin(offset, new File(file))
      }

      load(0x80000000l, "ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin")
      load(0x80F80000l, s"ext/NaxSoftware/buildroot/images/rv32ima/linux_${dut.naxes.size}c.dtb")
      load(0x80400000l, "ext/NaxSoftware/buildroot/images/rv32ima/Image")
      load(0x81000000l, "ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio")
    } else {
      val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"))
      elf.load(ma.mem, -0xffffffff80000000l)
      rvls.loadElf(0, elf.f)

      val passSymbol = elf.getSymbolAddress("pass")
      val failSymbol = elf.getSymbolAddress("fail")
      naxes.foreach { nax =>
        nax.commitsCallbacks += { (hartId, pc) =>
          if (pc == passSymbol) delayed(1) {
            println("nax(0) d$ refill = " + dut.dcache.logic.cache.refill.pushCounter.toLong)
            println("nax(0) i$ refill = " + dut.icache.logic.refill.pushCounter.toLong)
            simSuccess()
          }
          if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
        }
      }
    }

    println("Sim starting <3")
  }
}
