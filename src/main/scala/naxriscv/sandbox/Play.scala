package naxriscv.sandbox
import spinal.core._

import java.nio.ByteBuffer
object PlaySandbox extends App{
  SpinalVerilog(new Component{
    val zero,revert = in Bool()
    val rs1 = in UInt(32 bits)
    val rs2 = in UInt(32 bits)
    val src1 = out UInt(32 bits)
    val rs2Patched =  CombInit(rs2)
    when(revert){ rs2Patched :=  ~rs2  }
    when(zero){ rs2Patched := 0 }
    src1 := rs1 + rs2Patched
  })
}


object CheckFloat extends App{
  import java.nio.file.{Files, Paths}

  val byteArray = Files.readAllBytes(Paths.get("ext/NaxSoftware/baremetal/fpu_test/vector/f32.bin"))
  for(i <- 0 until byteArray.size by 4){
    val v = ByteBuffer.wrap((0 to 3).map(e => byteArray(i+3-e)).toArray).getInt
    val man = v & 0x7FFFF
    val exp = (v >> 23) & 0xFF
    val sign = (v >> 31) & 1
    if(exp == 0xFF && man != 0){
      println(f"${v}%08x")
      if(v == 0x7fc00000){
        println("HIT")
      }
    }
  }
}