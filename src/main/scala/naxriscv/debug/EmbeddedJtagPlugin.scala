package naxriscv.debug

import naxriscv.Global
import naxriscv.misc.PrivilegedPlugin
import naxriscv.utilities._
import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.slave

class EmbeddedJtagPlugin(var p : DebugTransportModuleParameter,
                         var withTap : Boolean = true,
                         var withTunneling : Boolean = false,
                         val noTapCd : Handle[ClockDomain] = Handle[ClockDomain].setName("jtagCd")) extends Plugin{
  val setup = create early new Area{
    getService[DocPlugin].property("EMBEDDED_JTAG","1")
  }

  val logic = create late new Area{
    val jtag = withTap generate slave(Jtag())
    val jtagInstruction = !withTap generate slave(JtagTapInstructionCtrl())
    val dm = DebugModule(
      DebugModuleParameter(
        version = p.version + 1,
        harts = 1,
        progBufSize = 4,
        datacount   = Global.XLEN/32
      )
    )
    val dmiDirect = if(withTap && !withTunneling) new Area {
      val logic = DebugTransportModuleJtagTap(
        p.copy(addressWidth = 7),
        debugCd = ClockDomain.current
      )
      dm.io.ctrl <> logic.io.bus
      logic.io.jtag <> jtag
    }
    val dmiTunneled = if(withTap && withTunneling) new Area {
      val logic = DebugTransportModuleJtagTapWithTunnel(
        p.copy(addressWidth = 7),
        debugCd = ClockDomain.current
      )
      dm.io.ctrl <> logic.io.bus
      logic.io.jtag <> jtag
    }
    val dmiNoTap = if(!withTap) new Area{
      val logic = DebugTransportModuleTunneled(
        p       = p,
        jtagCd  = noTapCd,
        debugCd = ClockDomain.current
      )
      jtagInstruction <> logic.io.instruction
      dm.io.ctrl <> logic.io.bus
    }

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
src/openocd -f tcl/interface/ftdi/ft2232h_breakout.cfg -f ../VexRiscvOoo/src/main/tcl/openocd/naxriscv_jtag.tcl
src/openocd -s tcl -f ../VexRiscvOoo/src/main/tcl/openocd/digilent_nexys_video.tcl -f ../VexRiscvOoo/src/main/tcl/openocd/naxriscv_jtag_tunneled.tcl

load_image /media/data/open/riscv/litex/standalone_nax/images/Image 0x41000000
dump_image /media/data/open/riscv/litex/standalone_nax/images/ImageDump 0x41000000 7548012


load_image /media/data/open/riscv/litex/standalone_nax/images/opensbi.bin 0x40f00000
dump_image /media/data/open/riscv/litex/standalone_nax/images/opensbiDump 0x40f00000 53640

load_image /media/data/open/riscv/litex/standalone_nax/images/Image 0x40000000
load_image /media/data/open/riscv/litex/standalone_nax/images/rv32.dtb 0x40ef0000
load_image /media/data/open/riscv/litex/standalone_nax/images/rootfs.cpio 0x41000000
load_image /media/data/open/riscv/litex/standalone_nax/images/opensbi.bin 0x40f00000
reg a0 0
reg a1 0
reg a2 0
reg pc 0x40f00000
resume

#echo [load_image /media/data/open/riscv/litex/standalone_nax/images/arty_a7.dts 0x40f00000]
#echo [load_image /media/data/open/riscv/litex/standalone_nax/images/raw 0x40f00000]
#echo [dump_image /media/data/open/riscv/litex/standalone_nax/images/dump 0x40f00000 1152044]

#adapter speed 100000
#ftdi tdo_sample_edge falling


riscv32-unknown-elf-gdb myExecutable.elf
target remote localhost:3333
set remotetimeout 60
set arch riscv:rv32
load
break main
continue


target remote localhost:3333
set remotetimeout 60
set arch riscv:rv64
load
break main
continue
 */

