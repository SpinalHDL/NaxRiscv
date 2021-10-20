package naxriscv.utilities

object ReadAfterWrite extends App{
  val logsPath =  "/media/data/open/riscv/test/m5out/trace.out"
  val source = scala.io.Source.fromFile("logsPath")
  val lines = try source.mkString finally source.close()

  
}
