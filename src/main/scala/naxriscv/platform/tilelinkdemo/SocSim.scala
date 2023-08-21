package naxriscv.platform.tilelinkdemo

import naxriscv.platform.{FileBackend, JniBackend, NaxriscvProbe, NaxriscvTilelinkProbe, PeripheralEmulator}
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent}
import spinal.lib.misc.Elf

import java.io.File
import scala.util.Random

object SocSim extends App{
  val sc = SimConfig
  sc.normalOptimisation
  sc.withFstWave
  sc.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)).includeSimulation)
  sc.addSimulatorFlag("--threads 4")
//  sc.addSimulatorFlag("--prof-exec")


  val compiled = sc.compile(new SocDemo(1))

  compiled.doSimUntilVoid(seed = 42) { dut =>
    fork {
      disableSimWave()
      while(true) {
        disableSimWave()
        sleep(100000 * 10)
        enableSimWave()
        sleep(  100 * 10)
      }
      //        waitUntil(simTime() > 15323180-100000)
      //        enableSimWave()
      //        sleep(500000)
      //        disableSimWave()
      //        simFailure("done")
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
    //      cd.forkSimSpeedPrinter(1.0)



    val memAgent = new MemoryAgent(dut.mem.node.bus, cd, seed = 0, randomProberFactor = 0.2f)(null){
      mem.randOffset = 0x80000000l

      import driver.driver._
      a.factor = 0.8f
      if(c != null) c.factor = 0.8f
      if(c != null) e.factor = 0.8f
      if(c != null) b.ctrl.transactionDelay = () => {
        val x = Random.nextInt(100)
        (x*x*3/100/100)
      }
      d.ctrl.transactionDelay = () => {
        val x = Random.nextInt(100)
        (x*x*3/100/100)
      }
      //        override def onA(a: TransactionA) = {
      //          if(a.address == 0x0FE5C7C0l){
      //            println(f"\nGOT IT AT ${a.opcode} ${a.param} $simTime\n")
      //          }
      //          super.onA(a)
      //        }
      //        override def onC(c: TransactionC) = {
      //          if(c.address == 0x0FE5C7C0l){
      //            println(f"\nGOT IT AT ${c.opcode} ${c.param} $simTime\n")
      //          }
      //          super.onC(c)
      //        }
      //        override def onD(d: TransactionD) = {
      //          if(d.address == 0x0FE5C7C0l){
      //            println(f"\nGOT IT AT ${d.opcode} ${d.param} $simTime\n")
      //          }
      //          super.onD(d)
      //        }
    }

    val checker = Checker(memAgent.monitor)

    val peripheralAgent = new PeripheralEmulator(dut.peripheral.emulated.node.bus, dut.peripheral.custom.mei, dut.peripheral.custom.sei, cd)

//    val tracer = new FileBackend(new File("trace.log"))
    val tracer = new JniBackend()
    tracer.spinalSimFlusher(10*10000)
    tracer.spinalSimTime(10000)
    tracer.debug()

    val naxes = dut.naxes.map(nax =>
      new NaxriscvTilelinkProbe(nax, nax.getHartId()).add(tracer)
    )

//    val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"))
//    //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/coremark/build/rv32ima/coremark.elf"))
//    //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/freertosDemo/build/rv32ima/freertosDemo.elf"))
//    //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/play/build/rv32ima/play.elf"))
////    val elf = new Elf(new File("ext/NaxSoftware/baremetal/simple/build/rv32ima/simple.elf"))
////          val elf = new Elf(new File("ext/NaxSoftware/baremetal/coherency/build/rv32ima/coherency.elf"))
//    //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/machine/build/rv32ima/machine.elf"))
//    //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/supervisor/build/rv32ima/supervisor.elf"))
//    //      val elf = new Elf(new File("ext/NaxSoftware/baremetal/mmu_sv32/build/rv32ima/mmu_sv32.elf"))
//
//    elf.load(memAgent.mem, -0xffffffff80000000l)
//    tracer.loadElf(0, elf.f)
//
//
//    val passSymbol = elf.getSymbolAddress("pass")
//    val failSymbol = elf.getSymbolAddress("fail")
//    naxes.foreach { nax =>
//      nax.commitsCallbacks += { (hartId, pc) =>
//        if (pc == passSymbol) delayed(1)(simSuccess())
//        if (pc == failSymbol) delayed(1)(simFailure("Software reach the fail symbole :("))
//      }
//    }

          memAgent.mem.loadBin(0x00000000l, "ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin")
          memAgent.mem.loadBin(0x00400000l, "ext/NaxSoftware/buildroot/images/rv32ima/Image")
          memAgent.mem.loadBin(0x01000000l, "ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio")
          memAgent.mem.loadBin(0x00F80000l, s"ext/NaxSoftware/buildroot/images/rv32ima/linux_${dut.naxes.size}c.dtb")

    //      tracer.loadBin(0x80000000l, new File("ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin"))
    //      tracer.loadBin(0x00F80000l, new File(s"ext/NaxSoftware/buildroot/images/rv32ima/linux_${dut.naxes.size}c.dtb"))
    //      tracer.loadBin(0x80400000l, new File("ext/NaxSoftware/buildroot/images/rv32ima/Image"))
    //      tracer.loadBin(0x81000000l, new File("ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio"))



    //      cd.waitSampling(4000000)
    //      simSuccess()
  }
}
