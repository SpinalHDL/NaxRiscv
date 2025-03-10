// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv

import naxriscv.Gen.plugins
import naxriscv.compatibility.{MemReadDuringWriteHazardPhase, MultiPortWritesSymplifier}
//import naxriscv.utilities.{DocPlugin, Plugin}
import org.apache.commons.io.FileUtils
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.SpinalConfig
import spinal.lib.misc.test.MultithreadedFunSuite

import java.nio.file.{Files, Paths, Path, StandardOpenOption}
import java.io.{File, OutputStream}
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Random

//object SyncBlock
//object NaxRiscvRvlsSim extends App {
  class NaxRiscvRvls extends MultithreadedFunSuite(sys.env.getOrElse("NAXRISCV_REGRESSION_THREAD_COUNT", "1").toInt) {

    var seed = sys.env.getOrElse("NAXRISCV_SEED", Random.nextInt(100000000).toString).toInt
    println("SEED="+seed)



    def doTest(name : String,
              naxCount : Int,
              params: => Seq[(String, String)],
              linuxCount : Int = sys.env.getOrElse("LINUX_COUNT", "0").toInt,
              freertosCount : Int = sys.env.getOrElse("FREERTOS_COUNT", "0").toInt,
              seed : Int = Random.nextInt()): Unit ={
      testMp(name){
        var workspacePath = s"simWorkspace/rvls/$name"
        val testbenchPath = s"src/test/python/naxriscv"

        println("Use seed : " + seed)

        FileUtils.deleteQuietly(new File(workspacePath))

        def cpyTb(file : String) = FileUtils.copyFile(new File(testbenchPath + "/" + file), new File(workspacePath + "/" + file))

        cpyTb("testsGenRvls.py")

        def doCmd(cmd: String, extraEnv: (String, String)*)(args: String*): String = {
          val stdOut = new StringBuilder()
          class Logger extends ProcessLogger {
            override def err(s: => String): Unit = {
              if (!s.startsWith("ar: creating ")) println(s)
            }
            override def out(s: => String): Unit = {
              val v : String = s.replace("\r", "")
              if (!v.contains("PASS") && !v.contains("make")) {
                println(v)
              }
              stdOut ++= v + "\n"
            }
            override def buffer[T](f: => T) = f
          }
          // Concatenate command and arguments
          val fullCommand = if (args.isEmpty) cmd else cmd + " " + args.mkString(" ")

          // Get the current working directory
          val currentDirectory = new File(workspacePath).getAbsolutePath

          //Run the command with the specified working directory
          try{
            val ret = Process(fullCommand, new File(workspacePath), extraEnv :_*).!(new Logger)
            assert(ret == 0)
          } catch {
            case ex: Exception => println(s"Error executing command: ${ex.getMessage}")
          }
          stdOut.toString()
        }

        // =====================================================================
        // Environment Configuration
        // =====================================================================
        
        // Verify and configure Verilator installation
        val verilatorRoot = {
          val base = new File("toolchain/verilator-v4.228")
          val canonical = base.getCanonicalFile
          if (!canonical.exists()) throw new Exception(s"Verilator missing at ${canonical.getAbsolutePath}")
          canonical.getAbsolutePath
        }

        // Ensure Verilator binary exists
        val verilatorBin = new File(s"$verilatorRoot/bin/verilator")
        if (!verilatorBin.exists()) throw new Exception(s"Verilator binary missing: ${verilatorBin.getAbsolutePath}")

        // Verify and configure Spike RISC-V simulator
        val spikeRoot = {
          val base = new File("ext/riscv-isa-sim")
          val canonical = base.getCanonicalFile
          if (!canonical.exists()) throw new Exception(s"Spike installation missing at ${canonical.getAbsolutePath}")
          canonical.getAbsolutePath
        }

        // Build environment variables for subprocesses
        val env = List(
          "WORKSPACE_PATH"       -> s"./simWorkspace/rvls",
          "WORKSPACE_NAME"       -> s"$name",
          "WORKSPACE_OUTPUT_DIR" -> s"logs",
          "NAXRISCV_SOFTWARE"    -> "ext/NaxSoftware",
          "SPIKE"                -> spikeRoot,                 // Absolute path to Spike
          "VERILATOR_ROOT"       -> verilatorRoot,             // Verilator installation root
          "PATH"                 -> List(                      // Prepend Verilator to PATH
            s"$verilatorRoot/bin", 
            System.getenv("PATH")
          ).mkString(File.pathSeparator),
          "FREERTOS_COUNT"       -> freertosCount.toString,
          "LINUX_COUNT"         -> linuxCount.toString,
          "NAXRISCV_SEED"        -> seed.toString,
          "NAXRISCV_COUNT"       -> naxCount.toString,
          "NAXRISCV_TEST_FPU_FACTOR" -> 0.10.toString
        )
        val envMap = env.toMap

        def parseConfig(isa: String): Option[List[String]] = {
          val configMap: Map[String, List[String]] = Map(
            "config_rv32imasu"    -> List("--xlen 32"),
            "config_rv64imasu"    -> List("--xlen 64"),
            "config_rv32imacsu"   -> List("--xlen 32", "--withRvc"),
            "config_rv64imacsu"   -> List("--xlen 64", "--withRvc"),
            "config_rv32imafcsu"  -> List("--xlen 32", "--withFloat", "--withRvc"),
            "config_rv64imafcsu"  -> List("--xlen 64", "--withFloat", "--withRvc"),
            "config_rv32imafdcsu" -> List("--xlen 32", "--withFloat", "--withDouble", "--withRvc"),
            "config_rv64imafdcsu" -> List("--xlen 64", "--withFloat", "--withDouble", "--withRvc")
          )

          configMap.get(isa)
        }

        // Function to extract configuration name from given string
        def extractConfigName(nameString: String): String = {
          if (nameString.startsWith("config_")) {
            val pattern = "config_(\\w+)".r
            val matches = pattern.findAllMatchIn(nameString)
            if (matches.hasNext) {
              matches.next().group(1)
            } else {
              throw new IllegalArgumentException("Configuration name could not be retrieved.")
            }
          } else {
            nameString
          }
        }

        // Filter params to get arguments with values equal to 1
        val filteredParams = params.collect {
          case (param, "1") => s"--$param"
        }.toList

        val makeThreadCount = sys.env.getOrElse("NAXRISCV_REGRESSION_MAKE_THREAD_COUNT", "3").toInt

        // Get the configuration arguments that are used in test generation
        val configurationArgs = parseConfig(name).getOrElse(Nil) ++ filteredParams

        configurationArgs match {
          case args: List[String] =>
            // Call doCmd with command, environment variables, and configuration arguments
            doCmd("python3 ./testsGenRvls.py", env: _*)(args: _*)
        } 
        
        // Extract config name
        val configName = extractConfigName(name)

        var command = s"make test-rvls-all".replace("rvls", configName)
        workspacePath = "."
        doCmd(command, env :_*)()
        command = s"make test-rvls-report".replace("rvls", configName)
        doCmd(command, env :_*)()

        workspacePath = s"simWorkspace/rvls/$name"
        val workspaceOutputDir = envMap.getOrElse("WORKSPACE_OUTPUT_DIR", "logs")
        val passed = doCmd(s"find $workspaceOutputDir -name PASS", env :_*)().lines.filter(line => line.contains("PASS")).toArray.size
        val failed = doCmd(s"find $workspaceOutputDir -name FAIL", env :_*)().lines.filter(line => line.contains("FAIL")).toArray.size
        println(s"PASS = $passed")
        println(s"FAIL = $failed")
        if(failed != 0 || passed == 0) throw new Exception("Failure")
      }
    }
 
    //doTest("config_rv32imasu", naxCount = 1, params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 1, freertosCount = 1)
    //doTest("config_rv64imasu", naxCount = 1,  params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 1, freertosCount = 1)
    //doTest("config_rv32imacsu",naxCount = 1,  params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 1, freertosCount = 1)
    //doTest("config_rv32imafcsu", naxCount = 1,  params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 0, freertosCount = 1)
    //doTest("config_rv32imafdcsu", naxCount = 1,  params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 0, freertosCount = 1)
    //doTest("config_rv64imacsu", naxCount = 1,  params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 1, freertosCount = 1)
    //doTest("config_rv64imafcsu", naxCount = 1,  params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 0, freertosCount = 1) 
    doTest("config_rv64imafdcsu", naxCount = 1, params = Seq("noL2" -> "0", "noRvls" -> "0", "dualSim" -> "0", "trace" -> "0"), linuxCount = 1, freertosCount = 1)
  }



