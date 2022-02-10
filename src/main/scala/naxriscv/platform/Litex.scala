package naxriscv.platform

import naxriscv.Config
import naxriscv.compatibility.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import naxriscv.fetch.FetchAxi4
import naxriscv.lsu.{DataCacheAxi4, LsuPeripheralAxiLite4}
import naxriscv.misc.PrivilegedPlugin
import naxriscv.utilities.Framework
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.WishboneClint
import spinal.lib.misc.plic.WishbonePlic


class NaxRiscvLitex extends Component{
  val cpu = new Component {
    Config.properties()
    val ramDataWidth = 128
    val ioDataWidth  =  32
    val plugins = Config.plugins(
      resetVector = 0,
      ioRange    = _(30, 2 bits) =/= U"01",
      fetchRange = a => SizeMapping(0x40000000, 0x10000000).hit(a) || SizeMapping(0, 0x00020000).hit(a)
    )
    plugins += new FetchAxi4(
      ramDataWidth = ramDataWidth,
      ioDataWidth  =  ioDataWidth
    )
    plugins += new DataCacheAxi4(
      dataWidth = ramDataWidth
    )
    plugins += new LsuPeripheralAxiLite4()
    val framework = new Framework(plugins)
  }

  val ram = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam.toIo()
    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi.toIo()
    Axi4SpecRenamer(ibus)
    Axi4SpecRenamer(dbus)
  }

  val peripheral = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiIo.toIo()
    val dbus = cpu.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo()
    AxiLite4SpecRenamer(ibus)
    AxiLite4SpecRenamer(dbus)

    val clintCtrl = new WishboneClint(1)
    val plicCtrl = new WishbonePlic(
      sourceCount = 32,
      targetCount = 2
    )

    val clint = clintCtrl.io.bus.toIo()
    val plic = plicCtrl.io.bus.toIo()
    val interrupt = plicCtrl.io.sources.toIo()

    val priv = cpu.framework.getService[PrivilegedPlugin].io
    priv.int.machine.timer       := clintCtrl.io.timerInterrupt(0)
    priv.int.machine.software    := clintCtrl.io.softwareInterrupt(0)
    priv.int.machine.external    := plicCtrl.io.targets(0)
    priv.int.supervisor.external := plicCtrl.io.targets(1)
    priv.rdtime                  := clintCtrl.io.time
  }
}

object LitexGen extends App{
  val spinalConfig = SpinalConfig(inlineRom = true)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  spinalConfig.generateVerilog(new NaxRiscvLitex)
}

/*
python3 -m litex_boards.targets.digilent_arty --cpu-type=naxriscv --with-ethernet --eth-ip 192.168.178.43 --eth-dynamic-ip  --load
litex_sim --cpu-type=naxriscv --with-sdram --sdram-module=MT41K128M16 --sdram-data-width=16  --sdram-init images/sim.json --trace --trace-fst --trace-start 2000000000000
eth_local_ip 192.168.178.43
eth_remote_ip 192.168.178.32
eth_local_ip  10.42.0.2
eth_remote_ip 10.42.0.1

netboot
boot 0x40f00000

dtc -O dtb -o rv32.dtb arty_a7.dts
py3tftp -p 69

make CROSS_COMPILE=riscv-none-embed- PLATFORM=litex/vexriscv
cp build/platform/litex/vexriscv/firmware/fw_jump.bin ../../litex_naxriscv_test/images/opensbi.bin
cp build/platform/litex/vexriscv/firmware/fw_jump.elf ../../litex_naxriscv_test/images/opensbi.elf

bus_timeout 1000000


nax after rdtime impl =>
[    1.765916] Run /init as init process
Saving random seed: [    2.659508] random: dd: uninitialized urandom read (512 bytes read)

Nax =>
[    0.395393] Unpacking initramfs...
[    1.005972] Initramfs unpacking failed: invalid magic at start of compressed archive
[    1.129054] Freeing initrd memory: 8192K
[    1.137357] workingset: timestamp_bits=30 max_order=16 bucket_order=0
[    1.205282] Block layer SCSI generic (bsg) driver version 0.4 loaded (major 253)
[    1.212020] io scheduler mq-deadline registered
[    1.216594] io scheduler kyber registered
[    1.762725] libphy: Fixed MDIO Bus: probed
[    1.766553] ohci_hcd: USB 1.1 'Open' Host Controller (OHCI) Driver
[    1.772462] ohci-platform: OHCI generic platform driver
[    1.778273] usbcore: registered new interface driver usb-storage
[    1.785094] usbcore: registered new interface driver ftdi_sio
[    1.790374] usbserial: USB Serial support registered for FTDI USB Serial Device
[    1.798221] udc-core: couldn't find an available UDC - added [g_audio] to list of pending drivers
[    1.809753] mousedev: PS/2 mouse device common for all mice
[    1.815633] i2c /dev entries driver
[    1.826120] usbcore: registered new interface driver usbhid
[    1.831009] usbhid: USB HID core driver
[    1.837768] usbcore: registered new interface driver snd-usb-audio
[    1.850875] NET: Registered protocol family 10
[    1.862263] Segment Routing with IPv6
[    1.865695] sit: IPv6, IPv4 and MPLS over IPv4 tunneling driver
[    1.876683] NET: Registered protocol family 17
[    1.882126] ALSA device list:
[    1.883944]   No soundcards found.
[    1.894063] Freeing unused kernel memory: 216K
[    1.897829] Kernel memory protection not selected by kernel config.
[    1.904376] Run /init as init process
[    1.907574]   with arguments:
[    1.907696]     /init
[    1.907800]   with environment:
[    1.907903]     HOME=/
[    1.908006]     TERM=linux
[    2.109186] tmpfs: Unknown parameter 'mode'
[    2.119644] tmpfs: Unknown parameter 'mode'
[    2.130038] tmpfs: Unknown parameter 'mode'
[    2.858237] random: dd: uninitialized urandom read (512 bytes read)

Vexriscv_smp =>
[    0.663808] Unpacking initramfs...
[    1.880436] Initramfs unpacking failed: invalid magic at start of compressed archive
[    1.934910] Freeing initrd memory: 8192K
[    1.947388] workingset: timestamp_bits=30 max_order=16 bucket_order=0
[    2.202019] Block layer SCSI generic (bsg) driver version 0.4 loaded (major 253)
[    2.209072] io scheduler mq-deadline registered
[    2.213686] io scheduler kyber registered
[    3.278364] libphy: Fixed MDIO Bus: probed
[    3.282631] ohci_hcd: USB 1.1 'Open' Host Controller (OHCI) Driver
[    3.288521] ohci-platform: OHCI generic platform driver
[    3.295413] usbcore: registered new interface driver usb-storage
[    3.303111] usbcore: registered new interface driver ftdi_sio
[    3.308960] usbserial: USB Serial support registered for FTDI USB Serial Device
[    3.317790] udc-core: couldn't find an available UDC - added [g_audio] to list of pending drivers
[    3.331541] mousedev: PS/2 mouse device common for all mice
[    3.339110] i2c /dev entries driver
[    3.360753] usbcore: registered new interface driver usbhid
[    3.366119] usbhid: USB HID core driver
[    3.375401] usbcore: registered new interface driver snd-usb-audio
[    3.403968] NET: Registered protocol family 10
[    3.420687] Segment Routing with IPv6
[    3.424860] sit: IPv6, IPv4 and MPLS over IPv4 tunneling driver
[    3.443239] NET: Registered protocol family 17
[    3.450599] ALSA device list:
[    3.452588]   No soundcards found.
[    3.461175] Freeing unused kernel memory: 216K
[    3.465608] Kernel memory protection not selected by kernel config.
[    3.472027] Run /init as init process
[    3.475663]   with arguments:
[    3.475868]     /init
[    3.476062]   with environment:
[    3.476264]     HOME=/
[    3.476458]     TERM=linux
[    3.872963] tmpfs: Unknown parameter 'mode'
[    3.887043] tmpfs: Unknown parameter 'mode'
[    3.901197] tmpfs: Unknown parameter 'mode'
[    5.386571] random: dd: uninitialized urandom read (512 bytes read)

 */