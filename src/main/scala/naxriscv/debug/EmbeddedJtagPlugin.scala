package naxriscv.debug

import naxriscv.misc.PrivilegedPlugin
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
//    dm.io.harts.head.resume.served := False
//    dm.io.harts.head.halted := False
//    dm.io.harts.head.running := False
//    dm.io.harts.head.unavailable := False
    val jtag = dmi.io.jtag.toIo()

    getService[PrivilegedPlugin].setup.debugBus.setAsDirectionLess <> dm.io.harts(0)
  }
}


/*
//TODO
- Ebreak stoping programm buffer
- All debug mode privelege special cases
- debug csr only accessible in debug mode
- RV64 abstract register access  need to access data1 aswell

make compile && ./obj_dir/VNaxRiscv --timeout-disable --spike-disable

src/openocd -f ../VexRiscvOoo/src/main/tcl/openocd/naxriscv_sim.tcl


riscv32-unknown-elf-gdb myExecutable.elf
target remote localhost:3333
set remotetimeout 60
set arch riscv:rv32
load
break main
continue
 */