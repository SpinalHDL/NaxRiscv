package naxriscv.sandbox

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import spinal.core.sim._

import java.io.{BufferedWriter, File, FileWriter}

object FpuRandomGen extends App{
  val rvd = true
  val rvf = true
  val xlen = 64
  val rounds = 10000

  val ptrFree = "x31"
  val ptrCmp = "x30"
  val tmp = "x29"
  val loop = "x28"
  val ptrRead = "x1"
  val ptrWrite = "x2"
  val ints = 3 to 10
  val floats = 0 to 10
  val memSize = 500

  def randomFloat = "f" + floats.randomPick()
  def randomInt = "x" + ints.randomPick()

  val opsFast = List(
    () => s"fmv.x.w   $randomInt  , $randomFloat\n",
    () => s"fmv.s.x   $randomFloat, $randomInt\n",
    () => s"fadd.s    $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fsub.s    $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fmul.s    $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fmadd.s   $randomFloat, $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fmsub.s   $randomFloat, $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fnmadd.s  $randomFloat, $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fnmsub.s  $randomFloat, $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fsgnj.s   $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fsgnjn.s  $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fsgnjx.s  $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fmin.s    $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fmax.s    $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fle.s     $randomInt  , $randomFloat, $randomFloat\n",
    () => s"feq.s     $randomInt  , $randomFloat, $randomFloat\n",
    () => s"flt.s     $randomInt  , $randomFloat, $randomFloat\n",
    () => s"fclass.s  $randomInt  , $randomFloat\n",
    () => s"fcvt.s.wu $randomFloat, $randomInt\n",
    () => s"fcvt.s.w  $randomFloat, $randomInt\n",
    () => s"fcvt.wu.s $randomInt  , $randomFloat\n",
    () => s"fcvt.w.s  $randomInt  , $randomFloat\n"
  )

  val opsSlow = List(
    () => s"fdiv.s    $randomFloat, $randomFloat, $randomFloat\n",
    () => s"fsqrt.s   $randomFloat, $randomFloat\n"
  )


  val out = new StringBuilder()
  val streams = ArrayBuffer[(Int, () => Unit)]()
  def addStream(weight : Int)(body : => Unit) = streams += (weight -> (() => body))

  addStream(100){
    for(i <- 0 until Random.nextInt(4)+1){
      out ++= s"flw $randomFloat, ${Random.nextInt(memSize) & ~3}($ptrRead)\n"
    }
  }
  addStream(100){
    for(i <- 0 until Random.nextInt(4)+1){
      out ++= s"fsw $randomFloat, ${Random.nextInt(memSize) & ~3}($ptrWrite)\n"
    }
  }
  addStream(2000){
    out ++= opsFast.randomPick()()
  }
  addStream(25){
    out ++= opsSlow.randomPick()()
  }
  addStream(50){
    for(i <- 0 until Random.nextInt(10)+1) {
      out ++= s"xor       $randomInt  , $randomInt, $randomInt\n"
    }
  }
  addStream(50){
    out ++= s"ble      $randomInt, $randomInt, 1f\n"
    out ++= s"xor       $randomInt  , $randomInt, $randomInt\n"
    out ++= s"1:\n"
  }

  //Check writes and increment memory pointers
  addStream(10){
    out ++= s"mv   $ptrFree, $ptrWrite\n"
    out ++= s"addi $ptrCmp, $ptrFree, $memSize\n"
    out ++= s"1:\n"
    out ++= s"lw  $tmp, 0($ptrFree)\n"
    out ++= s"addi  $ptrFree, $ptrFree, 4\n"
    out ++= s"bne $ptrCmp, $ptrFree, 1b\n"
    out ++= s"addi $ptrCmp, $ptrFree, $memSize\n"
    out ++= s"addi $ptrRead, $ptrRead, $memSize\n"
    out ++= s"addi $ptrWrite, $ptrWrite, $memSize\n"
  }

  addStream(25){
    out ++= s"csrr      $tmp, fcsr\n"
    out ++= s"csrw      fcsr, x0\n"
  }


  out ++= s"li $loop, 0\n"
  out ++= s"2:\n"
  addStream(50){
    out ++= s"li $tmp, ${Random.nextInt(5)+1}\n"
    out ++= s"addi $loop, $loop, 1\n"
    out ++= s"bne $loop, $tmp, 2b\n"
    out ++= s"li $loop, 0\n"
    out ++= s"2:\n"
  }


  val totalWeight = streams.map(_._1).sum
  for(i <- 0 until rounds){
    val weight = Random.nextInt(totalWeight)
    var sum = 0
    var id = 0
    while(sum + streams(id)._1 < weight){
      sum += streams(id)._1
      id += 1
    }
    streams(id)._2()
  }

  val file = new File("ext/NaxSoftware/baremetal/fpu_test3/src/randomRvfd.h")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(out.toString())
  bw.close()

}
