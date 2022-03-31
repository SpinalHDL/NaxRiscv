package naxriscv.debug

import naxriscv.utilities._
import spinal.core._

class EmbeddedJtagPlugin(p : DebugTransportModuleParameter) extends Plugin{
  val setup = create early new Area{
    getService[DocPlugin].property("EMBEDDED_JTAG","1")
  }

  val logic = create late new Area{
   val dmi = DebugTransportModuleJtagTap(
     p.copy(addressWidth = 7),
     debugCd = ClockDomain.current
   )
    val dm = DebugModule(
      DebugModuleParameter(
        version = p.version + 1,
        harts = 1,
        progBufSize = 4,
        datacount   = 8
      )
    )
    dm.io.ctrl <> dmi.io.bus
    dm.io.harts.head.resume.served := False
    dm.io.harts.head.halted := False
    dm.io.harts.head.running := False
    dm.io.harts.head.unavailable := False
    val jtag = dmi.io.jtag.toIo()
  }
}
