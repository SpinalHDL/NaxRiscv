package naxriscv.sandbox
import spinal.core._
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
