package riscv.model

import java.io.File
import scala.sys.process.{Process, ProcessLogger}

object Playground extends App{
  val java_home = System.getProperty("java.home")
  val location = new File("src/main/cpp/riscv/model")
  val spikeDir = new File(s"ext/riscv-isa-sim").getAbsolutePath
  val spikeSo = new File(s"$spikeDir/build/package.so").getAbsolutePath

  import spinal.lib.DoCmd._
  doCmd(Seq(
    "g++","-c","-fPIC",
    s"-I${java_home}/include", s"-I${java_home}/include/linux",
    s"-I${spikeDir}/build",s"-I${spikeDir}/riscv", s"-I${spikeDir}/fesvr",s"-I${spikeDir}/softfloat",
    "model.cpp","-o","model.o"
  ), location)

  doCmd(Seq(
    "g++","-shared","-fPIC",
    "-o","model.so","model.o", spikeSo, "-lc"
  ), location)


  val spike = new Model

//  for(i <- 0 until 10) {
//    var x = 0l
//    var i = 0
//    val t1 = System.nanoTime()
//    while(i < 1000000) {
//      val value = spike.newHandle("abc", 42)
//      x += value
//      i += 1
//    }
//    val t2 = System.nanoTime()
//    println(s"${(t2 - t1)/1000000} $x")
//  }

  val model = spike.newModel()
  println(model)
  spike.deleteModel(model)
}
