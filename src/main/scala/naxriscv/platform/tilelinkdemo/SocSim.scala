package naxriscv.platform.tilelinkdemo

import naxriscv.fetch.{FetchCachePlugin, PcPlugin}
import naxriscv.lsu.DataCachePlugin
import naxriscv.platform.{FileBackend, NaxriscvProbe, NaxriscvTilelinkProbe, PeripheralEmulator, RvlsBackend}
import spinal.core._
import spinal.core.fiber.Fiber
import spinal.core.sim._
import spinal.lib.bus.tilelink.ScopeFiber
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent}
import spinal.lib.misc.Elf
import spinal.lib.misc.test.{DualSimTracer, MultithreadedFunSuite, MultithreadedTester}

import java.nio.file.Paths
import java.io.File
import java.lang
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.io.StdIn.{readLine}
import scala.util.matching.Regex
import java.io.{OutputStream, PrintStream, ByteArrayOutputStream}
import sys.process._

/*
Multi core SoC simulation.

Setup :
- You need Verilator installed
- Unless you add --no-rvls, you need to compile rvls. See ext/rvls/README.md

Parameters :
--trace : Enable wave capture
--dual-sim : Enable dual lock step simulation to only trace the 50000 cycles before failure
--naxCount INT : Number of NaxRiscv cores
--no-l2 : Disable the l2 cache
--no-rvls : Disable rvls, so you don't need to compile it, but the NaxRiscv behaviour will not be checked.
--load-bin HEX,STRING : Load at address the given file. ex : 80000000,fw_jump.bin
--load-elf STRING : Load the given elf file. If both pass/fail symbole are defined, they will end the simulation once reached

The following commands are for the sbt terminal (that you can enter using "sbt" in the terminal)

Run baremetal example :
naxriscv.platform.tilelinkdemo.SocSim                                      \
--load-elf ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf

Boot dual core linux example :
naxriscv.platform.tilelinkdemo.SocSim                                      \
--load-bin 80000000,ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin   \
--load-bin 80F80000,ext/NaxSoftware/buildroot/images/rv32ima/linux.dtb  \
--load-bin 80400000,ext/NaxSoftware/buildroot/images/rv32ima/Image         \
--load-bin 81000000,ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio   \
--nax-count 2
 */

object SocSim extends App {
  var dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  var traceIt = false
  var withRvls = true
  var withL2 = true
  var asic = false
  var iverilog = false
  var naxCount = 1
  var xlen = 32
  var withRvc = false
  var withFloat = false
  var withDouble = false
  val bins = ArrayBuffer[(Long, String)]()
  val elfs = ArrayBuffer[String]()
  val u32 = ArrayBuffer[(Long, Int)]()
  val passTests = ArrayBuffer[String]()
  val failTests = ArrayBuffer[String]()
  var startSymbol : String = "_start"
  var passSymbol : String = "pass"
  var failSymbol : String = "fail"
  var startAdd = 0
  var workspacePath : String = "simWorkspace"
  var workspaceName: String = "SocDemo"
  var workspaceOutputDir: String = "logs"
  var workspaceOutputSubDir: String = "."
  // Linux commands
  var getc = ArrayBuffer[String]()
  var putc = ArrayBuffer[String]()
  var doSuccess = false

  assert(new scopt.OptionParser[Unit]("NaxRiscv") {
    help("help").text("prints this usage text")
    opt[Int]("xlen") action { (v, c) => xlen = v }
    opt[Unit]("withRvc") action { (v, c) => withRvc = true }
    opt[Unit]("withFloat") action { (v, c) => withFloat = true }
    opt[Unit]("withDouble") action { (v, c) => withDouble = true }
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("trace") action { (v, c) => traceIt = true }
    opt[Unit]("no-rvls") action { (v, c) => withRvls = false }
    opt[Unit]("no-l2") action { (v, c) => withL2 = false }
    opt[Unit]("asic") action { (v, c) => asic = true }
    opt[Unit]("iverilog") action { (v, c) => iverilog = true }
    opt[Int]("nax-count") action { (v, c) => naxCount = v }
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += (lang.Long.parseLong(v(1), 16) -> v(0)) }
    opt[String]("load-elf") unbounded() action { (v, c) => elfs += v }
    opt[Seq[String]]("load-u32") unbounded() action { (v, c) => u32 += ( lang.Long.parseLong(v(1), 16) -> Integer.parseInt(v(0), 10))}
    opt[String]("start-symbol") action { (v, c) => startSymbol = v }
    opt[String]("pass-symbol") action { (v, c) => passSymbol = v }
    opt[String]("fail-symbol") action { (v, c) => failSymbol = v }
    opt[Int]("start-add") action { (v, c) => startAdd = v }
    opt[String]("getc") unbounded() action { (v, c) => getc += v }
    opt[String]("putc") unbounded() action { (v, c) => putc += v }
    opt[Unit]("success") action { (v, c) => doSuccess = true }
    opt[String]("workspace-path") unbounded() action { (v, c) => workspacePath = v }
    opt[String]("workspace-name") unbounded() action { (v, c) => workspaceName = v }
    opt[String]("workspace-output-dir") unbounded() action { (v, c) => workspaceOutputDir = v }
    opt[String]("workspace-output-sub-dir") unbounded() action { (v, c) => workspaceOutputSubDir = v }
  }.parse(args, Unit).nonEmpty)
  // Replace commas (",") with spaces (" ")
  if(getc.nonEmpty){
    getc = getc.map(_.replaceAll(",", " "))
  }
  if(putc.nonEmpty){
    putc = putc.map(_.replaceAll(",", " "))
  }
  if (workspaceOutputSubDir == null){
    workspaceOutputSubDir = Paths.get(elfs.apply(0)).getParent().getFileName().toString()
  }//else{
  //  workspaceOutputSubDir =  Paths.get(elfs.apply(0)).getParent().getFileName().toString() + "/" + workspaceOutputSubDir
  //}
  //println("workspaceName: "+ workspaceName)
  val sc = SimConfig
  sc.allOptimisation
  //set workspace path and name
  sc.workspacePath(workspacePath)

  sc.workspaceName(workspaceName)

  sc.wavePath(s"waves/${workspaceOutputSubDir}")

//  sc.normalOptimisation
  if(iverilog) {
    sc.withIVerilog
    withRvls = false //unsuported because of probe
  }
//  sc.withWave
  if(!iverilog) sc.withFstWave
  if(iverilog && traceIt) sc.withWave
  sc.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC)).includeSimulation)
//  sc.addSimulatorFlag("--threads 1")
//  sc.addSimulatorFlag("--prof-exec")

  // Tweek the toplevel a bit
  class SocDemoSim(cpuCount : Int) extends SocDemo(cpuCount, withL2 = withL2, asic = asic, xlen = xlen, withRvc = withRvc, withFloat = withFloat, withDouble = withDouble) {
    setDefinitionName("SocDemo")
    // You can for instance override cache parameters of the CPU caches like that :
    naxes.flatMap(_.plugins).foreach{
      case p : FetchCachePlugin => //p.cacheSize = 2048
      case p : DataCachePlugin =>  //p.cacheSize = 2048
      case _ =>
    }

    // l2.cache.parameter.cacheBytes = 4096

    // Here we add a scope peripheral, which can count the number of cycle that a given signal is high
    // See ext/NaxSoftware/baremetal/socdemo for software usages
    val scope = new ScopeFiber(){
      up at 0x04000000 of peripheral.bus
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
            add(p.logic.cache.writeback.push.fire, i * 0x80 + 0x014)
            if (withL2) {
              val l2c = l2.cache.logic.cache
              l2c.rework{
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

  }
  val compiled = sc.compile(new SocDemoSim(cpuCount = naxCount))

  for (file <- elfs) {
    //println("ELF PATH: "+file)
    // How we want to run the test
    dualSim match {
      case true => DualSimTracer.withCb(compiled, window = 50000 * 10, seed = 2, file, runBuildroot = false)(testIt)
      case false => compiled.doSimUntilVoid(name = file.substring(file.lastIndexOf("/") + 1), seed = 2) { dut => disableSimWave(); testIt(dut, f => if (traceIt) f, file, runBuildroot = false) }
    }
  }

  if(getc.nonEmpty || putc.nonEmpty){
    val testName = workspaceOutputSubDir.replace("/", "_")
    val file = "/"+testName
    // How we want to run the test
    dualSim match {
      case true => DualSimTracer.withCb(compiled, window = 50000 * 10, seed = 2, file, runBuildroot = true)(testIt)
      case false => compiled.doSimUntilVoid(name = testName, seed = 2) { dut => disableSimWave(); testIt(dut, f => if (traceIt) f, file, runBuildroot = true) }
    }
  }

  println(passTests.size + " TESTS PASSED" + " & " + failTests.size + " TESTS FAILED")
  if (failTests.size > 0){
    println("LIST OF TESTS FAILED")
    for(test <- failTests) println(test)
  }

  // Testbench code
  def testIt(dut : SocDemoSim, onTrace : (=> Unit) => Unit = cb => {}, elfFile : String, runBuildroot: Boolean = false): Unit = {
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    //cd.forkSimSpeedPrinter(1.0)

    // Connect the few peripherals
    val ma = new MemoryAgent(dut.mem.node.bus, cd, seed = 0, randomProberFactor = 0.2f)(null) {
      mem.randOffset = 0x80000000l
      driver.driver.setFactor(0.8f)
      val checker = if(monitor.bus.p.withBCE) Checker(monitor)
    }
    val pa = new PeripheralEmulator(dut.peripheral.emulated.node.bus, dut.peripheral.custom.mei, dut.peripheral.custom.sei, cd)

    println("\nPeripheralEmulator IS created\n")

    //set workspace path and name
    //val parentDir = Paths.get(elfFile).getParent().getFileName().toString()
    /*var wavePath = s"${workspaceOutputDir}/${parentDir}"
    println("WAVE PATH: " + wavePath)
    sc.wavePath(wavePath)*/

    //set output directory
    //val parentDir = Paths.get(elfFile).getParent().getFileName().toString()
    val lastDotIndex : Int = currentTestName.lastIndexOf('.')
    val _currentTestName: String = if (lastDotIndex != -1) {
      currentTestName.substring(0, lastDotIndex)
    } else {
      currentTestName
    }

    val logsOutputPathFile = new File(compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir, _currentTestName)
    if(!logsOutputPathFile.exists()) {
      logsOutputPathFile.mkdirs
    }
    println("logs_output: "+logsOutputPathFile);

    // Rvls will check that the CPUs are doing things right
    val rvls = withRvls generate new RvlsBackend(logsOutputPathFile)
    if(withRvls) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }
    // isa extensions for spike
    var isa = ""
    // I : integer
    if (xlen == 32) isa += "RV32I"
    else isa += "RV64I"
    // M : integer multiplication and division
    // A : atomic
    isa += "MA"
    // F : adds floating-point registers, single-precision
    if(withFloat) isa += "F"
    // D : expends the floating-point registers, and adds double-precision
    if(withDouble) isa += "D"
    // C : compressed instruction extension
    if(withRvc) isa += "C"

    // Collect traces from the CPUs behaviour
    val naxes = (!iverilog) generate dut.naxes.map(nax => new NaxriscvTilelinkProbe(nax, nax.getHartId()))
    println()
    if(withRvls) {
      naxes.foreach(_.add(rvls, isa))
      rvls.debug()
    }
    // Things to enable when we want to collect traces
    onTrace{
      enableSimWave()      
      //val parentDir = Paths.get(elfFile).getParent().getFileName().toString()
      val tracerFile = new FileBackend(new File(logsOutputPathFile, "tracer.log"))
      println("tracerFile Path: "+compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir+"/"+ _currentTestName);
      //println("Name: " + parentDir);
      //println("currentTestName: " + currentTestName);
      tracerFile.spinalSimFlusher(10 * 10000)
      tracerFile.spinalSimTime(10000)
      if(naxes != null) naxes.foreach { hart =>
        hart.add(tracerFile, isa)
        val r = hart.backends.reverse
        hart.backends.clear()
        hart.backends ++= r
      }
    }

    // Load the binaries
    for((offset, file) <- bins){
      ma.mem.loadBin(offset - 0x80000000l, file)
      if(withRvls){
        rvls.loadBin(offset, new File(file))
      }
    }

    // Load U32
    //if(u32.nonEmpty){
    for ((address, data) <- u32) {
      ma.mem.write(address - 0x80000000l, data)
      println("TEST: " + ma.mem.readInt(address).toHexString)
      if (withRvls) {
        rvls.loadU32(address, data)
      }
    }
    //}
    // Setting the function to run the Buildroot process
    if (runBuildroot) {
      // Iterate the buffers of the getc and putc arrays alternately.
      while (getc.nonEmpty || putc.nonEmpty) {
        if(getc.nonEmpty){
          // Add items from getc to the queue
          getc.headOption.foreach { element =>
            pa.testScheduleQueue.enqueue(new pa.WaitPutc(element))
            println("GETC: " + element)
          }
          getc = getc.tail
        }
        // Add items from putc to the queue
        if(putc.nonEmpty){
          putc.headOption.foreach { element =>
            pa.testScheduleQueue.enqueue(new pa.DoGetc(element))
            println("PUTC: " + element)
          }
          putc = putc.tail
        }
      }
      // Add a success to the end of the queue
      if(doSuccess == true){
        pa.testScheduleQueue.enqueue(new pa.DoSuccess(compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir, passTests))
        println("DoSuccess: success")
      }
      // Press the first command in Linux
      pa.testScheduleQueueNext()
    }
    else{
      // load elfs
      val elf = new Elf(new File(elfFile), xlen)
      elf.load(ma.mem, 0x80000000l)
      if (withRvls) rvls.loadElf(0, elf.f)

      if (startSymbol != null) {
        dut.naxes.foreach {
          nax => cd.onNextSampling {
            nax.thread.core.framework.getService[PcPlugin].logic.fetchPc.pcReg #= BigInt(elf.getSymbolAddress(startSymbol).toInt.toHexString, 16) + startAdd
          }
        }
        if (withRvls) dut.naxes.map(nax => rvls.setPc(nax.getHartId(), elf.getSymbolAddress(startSymbol) + startAdd))
      }

      //if (elf.getELFSymbol(passSymbol) != null && elf.getELFSymbol("fail") != null) {
      if (elf.getELFSymbol(passSymbol) != null) {
        val getPassSymbol = elf.getSymbolAddress(passSymbol)
        //val getFailSymbol = elf.getSymbolAddress("fail")
        if (naxes != null) naxes.foreach { nax =>
          nax.commitsCallbacks += { (hartId, pc) =>
            if (pc == getPassSymbol) delayed(1) {
              dut.naxes.foreach { nax =>
                println(s"Hart $hartId")
                nax.plugins.foreach {
                  case p: FetchCachePlugin => println("- i$ refill = " + p.logic.refill.pushCounter.toLong)
                  case p: DataCachePlugin => println("- d$ refill = " + p.logic.cache.refill.pushCounter.toLong)
                  case _ =>
                }
              }
              println("TEST PASS")
              passTests += elfFile.substring(elfFile.lastIndexOf("/") + 1)
              // Delete “FAIL” file or directory
              val failFile = new File(compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir+"/"+_currentTestName, "FAIL")
              if (failFile.exists()) {
                failFile.delete()
              }
              // Create the “PASS” file
              val passFile = new File(compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir+"/"+_currentTestName, "PASS")
              println("PASS FILE "+passFile)
              passFile.createNewFile()
              // The test is carried out successfully
              simSuccess()
            }
          }
        }
      }
      if(elf.getELFSymbol(failSymbol) != null){
        val getFailSymbol = elf.getSymbolAddress(failSymbol)
        if (naxes != null) naxes.foreach { nax =>
          nax.commitsCallbacks += { (hartId, pc) =>
            if (pc == getFailSymbol) delayed(1) {
              println("TEST FAIL")
              failTests += elfFile.substring(elfFile.lastIndexOf("/") + 1)
              // Delete “PASS” file or directory
              val passFile = new File(compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir+"/"+_currentTestName, "PASS")
              if(passFile.exists()){
                passFile.delete()
              }
              // Create the “FAIL” file
              val failFile = new File(compiled.compiledPath+"/"+ workspaceOutputDir+"/"+workspaceOutputSubDir+"/"+_currentTestName, "FAIL")
              failFile.createNewFile()
              //The test failed
              simSuccess()
              //simFailure("Software reach the fail symbole :(")
            }
          }
        }
      }
    }

    println("Sim starting <3")

  }
}
