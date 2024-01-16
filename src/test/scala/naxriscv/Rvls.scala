package naxriscv

object Rvls extends App{
  import rvls.jni.Frontend

  val handle = Frontend.newDisassemble(32)
  println(Frontend.disassemble(handle, 0x03410793))
  println(Frontend.disassemble(handle, 0x13))
  Frontend.deleteDisassemble(handle)
}
