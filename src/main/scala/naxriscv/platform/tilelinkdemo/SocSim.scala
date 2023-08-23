package naxriscv.platform.tilelinkdemo

import naxriscv.platform.{FileBackend, JniBackend, NaxriscvProbe, NaxriscvTilelinkProbe, PeripheralEmulator}
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent}
import spinal.lib.misc.Elf
import spinal.lib.misc.test.{DualSimTracer, MultithreadedFunSuite, MultithreadedTester}

import java.io.File
import scala.collection.mutable.ArrayBuffer


object SocSim extends App {
  val sc = SimConfig
  sc.normalOptimisation
  sc.withFstWave
  sc.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC)).includeSimulation)
  sc.addSimulatorFlag("--threads 1")
//  sc.addSimulatorFlag("--prof-exec")

  val tester = new MultithreadedTester(8)
  import tester._

  val compiled = sc.compile(new SocDemo(2))

//  for (i <- 0 until 64) test("test_" + i) {
//    compiled.doSimUntilVoid(name = s"test_$i", seed = i)(testIt(_))
//  }

  DualSimTracer.withCb(compiled, window = 50000*10, 2)(testIt)


  await()

//  compiled.doSimUntilVoid(name = s"test", seed = 2)(testIt(_))

  def testIt(dut : SocDemo, onTrace : (=> Unit) => Unit = cb => {}): Unit = {
    disableSimWave()
    fork {

//      sleep(100000*5)
//      simFailure("<3")
      //        sleep(100 * 10)
      //        sleep(100000 * 10)
      //        println("****")
      //        enableSimWave()
      //        sleep(10 * 10)
      //
      //        println(s"\n\nRAND=${simRandom.nextInt()}\n\n")
      //        simSuccess()

//      while (true) {
//        disableSimWave()
//        sleep(100000 * 10)
//        enableSimWave()
//        sleep(100 * 10)
//      }
      //      waitUntil(simTime() > 307150-10000)
      //      enableSimWave()
      //      sleep(10000*2)
      //      disableSimWave()
      //      simSuccess()
      //
      //        waitUntil(simTime() > 3275640-300000)
      //        enableSimWave()
      //        sleep(400000)
      //        disableSimWave()
      //
      //        simFailure("done")
    }



    val cd = dut.clockDomain
    cd.forkStimulus(10)
    //    cd.forkSimSpeedPrinter(1.0)


    val memAgent = new MemoryAgent(dut.mem.node.bus, cd, seed = 0, randomProberFactor = 0.2f)(null) {
      mem.randOffset = 0x80000000l

      import driver.driver._

      a.factor = 0.8f
      if (c != null) c.factor = 0.8f
      if (c != null) e.factor = 0.8f
      if (c != null) b.ctrl.transactionDelay = () => {
        val x = simRandom(sm).nextInt(100)
        (x * x * 3 / 100 / 100)
      }
      d.ctrl.transactionDelay = () => {
        val x = simRandom(sm).nextInt(100)
        (x * x * 3 / 100 / 100)
      }
    }

    val checker = Checker(memAgent.monitor)

    val peripheralAgent = new PeripheralEmulator(dut.peripheral.emulated.node.bus, dut.peripheral.custom.mei, dut.peripheral.custom.sei, cd)


    val tracer = new JniBackend(new File(compiled.compiledPath, currentTestName))
    tracer.spinalSimFlusher(10 * 10000)
    tracer.spinalSimTime(10000)
    // tracer.debug()

    val naxes = dut.naxes.map(nax =>
      new NaxriscvTilelinkProbe(nax, nax.getHartId()).add (tracer)
    )


    onTrace{
      tracer.debug()

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

    //        val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"))
        //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/coremark/build/rv32ima/coremark.elf"))
        //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/freertosDemo/build/rv32ima/freertosDemo.elf"))
        //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/play/build/rv32ima/play.elf"))
    //    val elf = new Elf(new File("ext/NaxSoftware/baremetal/simple/build/rv32ima/simple.elf"))
//              val elf = new Elf(new File("ext/NaxSoftware/baremetal/coherency/build/rv32ima/coherency.elf"))
        //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/machine/build/rv32ima/machine.elf"))
        //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/supervisor/build/rv32ima/supervisor.elf"))
        //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/mmu_sv32/build/rv32ima/mmu_sv32.elf"))

//        elf.load(memAgent.mem, -0xffffffff80000000l)
//        tracer.loadElf(0, elf.f)
//
//
//        val passSymbol = elf.getSymbolAddress("pass")
//        val failSymbol = elf.getSymbolAddress("fail")
//        naxes.foreach { nax =>
//          nax.commitsCallbacks += { (hartId, pc) =>
//            if (pc == passSymbol) delayed(1)(simSuccess())
//            if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
//          }
//        }

    memAgent.mem.loadBin(0x00000000l, "ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin")
    memAgent.mem.loadBin(0x00400000l, "ext/NaxSoftware/buildroot/images/rv32ima/Image")
    memAgent.mem.loadBin(0x01000000l, "ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio")
    memAgent.mem.loadBin(0x00F80000l, s"ext/NaxSoftware/buildroot/images/rv32ima/linux_${dut.naxes.size}c.dtb")


    tracer.loadBin(0x80000000l, new File("ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin"))
    tracer.loadBin(0x80F80000l, new File(s"ext/NaxSoftware/buildroot/images/rv32ima/linux_${dut.naxes.size}c.dtb"))
    tracer.loadBin(0x80400000l, new File("ext/NaxSoftware/buildroot/images/rv32ima/Image"))
    tracer.loadBin(0x81000000l, new File("ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio"))

    //      cd.waitSampling(4000000)
    //      simSuccess()
  }


}

/*
Starting klogd: [Error] Simulation failed at time=2265553930
Exception in thread "main" spinal.sim.SimFailure: Nax didn't commited anything since too long
  at spinal.core.sim.package$.simFailure(package.scala:164)
 */