package naxriscv

object Rvls extends App{
  import rvls.jni.Frontend
  // The method `newDisassemble` requires two parameters: a Long and an Int (x$1: Long, x$2: Int)
  // The current call to `Frontend.newDisassemble(32)` only provides a single Int argument (32) 
  // and does not supply the required Long argument (x$1). 
  // To resolve this error, both parameters must be provided in the method call.

  //val handle = Frontend.newDisassemble(32)
  //println(Frontend.disassemble(handle, 0x03410793))
  //println(Frontend.disassemble(handle, 0x13))
  //Frontend.deleteDisassemble(handle)
}
