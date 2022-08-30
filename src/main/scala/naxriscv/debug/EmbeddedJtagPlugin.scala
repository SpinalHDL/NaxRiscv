package naxriscv.debug

import naxriscv.Global
import naxriscv.misc.PrivilegedPlugin
import naxriscv.utilities._
import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib.com.jtag.{Jtag, JtagTapFactory, JtagTapInstructionCtrl}
import spinal.lib.slave

class EmbeddedJtagPlugin(var p : DebugTransportModuleParameter,
                         var withTap : Boolean = true,
                         var withTunneling : Boolean = false) extends Plugin{

  val debugCd = Handle[ClockDomain].setName("debugCd")
  val noTapCd = Handle[ClockDomain].setName("jtagCd")

  val setup = create early new Area{
    getService[DocPlugin].property("EMBEDDED_JTAG","1")
  }

  val logic = create late debugCd(new Area{
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
    val ndmreset = dm.io.ndmreset.toIo()

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

    val privBus = getService[PrivilegedPlugin].setup.debugBus.setAsDirectionLess()
    privBus <> dm.io.harts(0)
    privBus.dmToHart.removeAssignments() <-< dm.io.harts(0).dmToHart
  })
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

mdw 0x40000000 16
mww 0x40000000 0x12345678
mdw 0x40000000 16
load_image /home/rawrr/Downloads/top_soc.v 0x40000000

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

load_image /media/data/open/riscv/litex/buildroot_9ef54b7d/output/images/Image 0x40000000
load_image /media/data/open/riscv/litex/standalone_nax/images/rv32.dtb 0x40ef0000
load_image /media/data/open/riscv/litex/buildroot_9ef54b7d/output/images/rootfs.cpio 0x41000000
load_image /media/data/open/riscv/litex/standalone_nax/images/opensbi.bin 0x40f00000
reg a0 0
reg a1 0
reg a2 0
reg pc 0x40f00000
resume


load_image /media/data/open/riscv/VexRiscvOoo/ext/NaxSoftware/baremetal/dhrystone/build/rv32im/dhrystone.bin 0x80000000


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

/*

#jtag newtap $_CHIPNAME dummy2 -irlen 5 -expected-id 0x10005FFF
jtag newtap $_CHIPNAME cpu -irlen 6 -expected-id 0x10003FFF
#jtag newtap $_CHIPNAME dummy1 -irlen 4 -expected-id 0x10004FFF

      val jtag1 = Jtag()
      val tap1 = logic.jtagCd on JtagTapFactory(jtag1, instructionWidth = 4)
      val idcodeArea1 = logic.jtagCd on tap1.idcode(B"x10004FFF")(1)

      val jtag2 = Jtag()
      val tap2 = logic.jtagCd on JtagTapFactory(jtag2, instructionWidth = 5)
      val idcodeArea2 = logic.jtagCd on tap2.idcode(B"x10005FFF")(1)

      jtag1.tck := jtag.tck
      logic.io.jtag.tck := jtag.tck
      jtag2.tck := jtag.tck

      jtag1.tms := jtag.tms
      logic.io.jtag.tms := jtag.tms
      jtag2.tms := jtag.tms

      jtag1.tdi := jtag.tdi
      logic.io.jtag.tdi := jtag1.tdo
      jtag2.tdi := logic.io.jtag.tdo
      jtag.tdo := jtag2.tdo
 */