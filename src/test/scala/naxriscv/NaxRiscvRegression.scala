// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv

import naxriscv.Gen.plugins
import naxriscv.compatibility.{MemReadDuringWriteHazardPhase, MultiPortWritesSymplifier}
import naxriscv.utilities.{DocPlugin, Plugin}
import org.apache.commons.io.FileUtils
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.SpinalConfig
import spinal.lib.misc.test.MultithreadedFunSuite

import java.io.{File, OutputStream}
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Random

object HeavyLock
class NaxRiscvRegression extends MultithreadedFunSuite(sys.env.getOrElse("NAXRISCV_REGRESSION_THREAD_COUNT", "4").toInt){

  var seed = sys.env.getOrElse("NAXRISCV_SEED", Random.nextInt(100000000).toString).toInt
  println("SEED="+seed)



  def doTest(name : String,
             plugins : => Seq[Plugin],
             linuxCount : Int = sys.env.getOrElse("LINUX_COUNT", "0").toInt,
             freertosCount : Int = sys.env.getOrElse("FREERTOS_COUNT", "0").toInt,
             seed : Int = Random.nextInt()): Unit ={
    testMp(name){
      val workspacePath = s"simWorkspace/regression/$name"
      val testbenchPath = s"src/test/cpp/naxriscv"

      println("Use seed : " + seed)

      FileUtils.deleteQuietly(new File(workspacePath))

      val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = workspacePath)
      spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
      spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

      spinalConfig.includeSimulation

      val report = HeavyLock.synchronized {spinalConfig.generateVerilog(new NaxRiscv(plugins))}
      val doc = report.toplevel.framework.getService[DocPlugin]
      doc.genC(workspacePath + "/nax.h")

      def cpyTb(file : String) = FileUtils.copyFile(new File(testbenchPath + "/" + file), new File(workspacePath + "/" + file))
      cpyTb("makefile")
      cpyTb("testsGen.py")
      FileUtils.copyDirectory(new File(testbenchPath + "/src"), new File(workspacePath + "/src"))

      def doCmd(cmd: String, extraEnv: (String, String)*): String = {
        val stdOut = new StringBuilder()
        class Logger extends ProcessLogger {
          override def err(s: => String): Unit = {
            if (!s.startsWith("ar: creating ")) println(s)
          }
          override def out(s: => String): Unit = {
            val v : String = s.replace("\r", "")
            println(v)
            stdOut ++= v + "\n"
          }
          override def buffer[T](f: => T) = f
        }
        val ret = Process(cmd, new File(workspacePath), extraEnv :_*).!(new Logger)
        assert(ret == 0)
        stdOut.toString()
      }

      val env = List(
        "NAXRISCV_GEN_FOLDER" -> ".",
        "NAXRISCV_SOFTWARE" -> "../../../ext/NaxSoftware",
        "SPIKE" -> "../../../ext/riscv-isa-sim",
        "FREERTOS_COUNT" -> freertosCount.toString,
        "LINUX_COUNT" -> linuxCount.toString,
        "NAXRISCV_SEED" -> seed.toString,
        "NAXRISCV_TEST_FPU_FACTOR" -> 0.10.toString
      )

      val makeThreadCount = sys.env.getOrElse("NAXRISCV_REGRESSION_MAKE_THREAD_COUNT", "3").toInt
      println("Env :\n" + env.map(e => e._1 + "=" + e._2).mkString(" "))
      doCmd("python3 ./testsGen.py", env :_*)
      HeavyLock.synchronized {
        doCmd("make compile", env: _*)
      }
      doCmd(s"make test-all -j${makeThreadCount}", env :_*)
      val passed = doCmd(s"find output -name PASS", env :_*).lines.filter(line => line.contains("PASS")).toArray.size
      val failed = doCmd(s"find output -name FAIL", env :_*).lines.filter(line => line.contains("FAIL")).toArray.size
      println(s"PASS = $passed")
      println(s"FAIL = $failed")
      if(failed != 0 || passed == 0) throw new Exception("Failure")
    }
  }


  doTest("config_rv32imasu",  Config.plugins(withRdTime = false, xlen = 32, withRvc = false), linuxCount = 0, freertosCount = 0)
  doTest("config_rv64imasu",  Config.plugins(withRdTime = false, xlen = 64, withRvc = false), linuxCount = 0, freertosCount = 0)
  doTest("config_rv32imacsu", Config.plugins(withRdTime = false, xlen = 32, withRvc = true), linuxCount = 0, freertosCount  = 0)
  doTest("config_rv32imafcsu", Config.plugins(withRdTime = false, xlen = 32, withRvc = true, withFloat = true, withDouble = false), linuxCount = 0, freertosCount = 0)
  doTest("config_rv32imafdcsu", Config.plugins(withRdTime = false, xlen = 32, withRvc = true, withFloat = true, withDouble = true), linuxCount = 0, freertosCount = 1)
  doTest("config_rv64imacsu", Config.plugins(withRdTime = false, xlen = 64, withRvc = true), linuxCount = 0, freertosCount = 0)
  doTest("config_rv64imafcsu", Config.plugins(withRdTime = false, xlen = 64, withRvc = true, withFloat = true, withDouble = false), linuxCount = 0, freertosCount = 0)
  doTest("config_rv64imafdcsu", Config.plugins(withRdTime = false, xlen = 64, withRvc = true, withFloat = true, withDouble = true), linuxCount = 1, freertosCount = 1)
}

