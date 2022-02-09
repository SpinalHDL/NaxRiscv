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
python3 -m litex_boards.targets.digilent_arty --cpu-type=naxriscv --build --with-ethernet --eth-ip 192.168.178.43
litex_sim --cpu-type=naxriscv --with-sdram --sdram-module=MT41K128M16 --sdram-data-width=16  --sdram-init images/sim.json --trace --trace-fst --trace-start 2000000000000
eth_local_ip 192.168.178.43
eth_remote_ip 192.168.178.32
netboot
boot 0x40f00000

dtc -O dtb -o rv32.dtb arty_a7.dts


make CROSS_COMPILE=riscv-none-embed- PLATFORM=litex/vexriscv
cp build/platform/litex/vexriscv/firmware/fw_jump.bin ../../litex_naxriscv_test/images/opensbi.bin
cp build/platform/litex/vexriscv/firmware/fw_jump.elf ../../litex_naxriscv_test/images/opensbi.elf

bus_timeout 1000000


Vexriscv_smp =>

root@buildroot:~# dmesg
[    0.000000] Linux version 5.11.0-rc2 (rawrr@rawrr) (riscv32-buildroot-linux-gnu-gcc.br_real (Buildroot 2020.11-rc2-49-g1672e25010) 10.2.0, GNU ld (GNU Binutils) 2.34) #2 SMP Tue May 25 18:25:52 CEST 2021
[    0.000000] earlycon: sbi0 at I/O port 0x0 (options '')
[    0.000000] printk: bootconsole [sbi0] enabled
[    0.000000] Initial ramdisk at: 0x(ptrval) (8388608 bytes)
[    0.000000] Zone ranges:
[    0.000000]   Normal   [mem 0x0000000040000000-0x000000004fffffff]
[    0.000000] Movable zone start for each node
[    0.000000] Early memory node ranges
[    0.000000]   node   0: [mem 0x0000000040000000-0x000000004fffffff]
[    0.000000] Initmem setup node 0 [mem 0x0000000040000000-0x000000004fffffff]
[    0.000000] On node 0 totalpages: 65536
[    0.000000]   Normal zone: 512 pages used for memmap
[    0.000000]   Normal zone: 0 pages reserved
[    0.000000]   Normal zone: 65536 pages, LIFO batch:15
[    0.000000] SBI specification v0.2 detected
[    0.000000] SBI implementation ID=0x1 Version=0x8
[    0.000000] SBI v0.2 TIME extension detected
[    0.000000] SBI v0.2 IPI extension detected
[    0.000000] SBI v0.2 RFENCE extension detected
[    0.000000] SBI v0.2 HSM extension detected
[    0.000000] riscv: ISA extensions aim
[    0.000000] riscv: ELF capabilities aim
[    0.000000] percpu: Embedded 10 pages/cpu s18316 r0 d22644 u40960
[    0.000000] pcpu-alloc: s18316 r0 d22644 u40960 alloc=10*4096
[    0.000000] pcpu-alloc: [0] 0
[    0.000000] Built 1 zonelists, mobility grouping on.  Total pages: 65024
[    0.000000] Kernel command line: console=hvc0 earlycon=sbi rootwait root=/dev/ram0
[    0.000000] Dentry cache hash table entries: 32768 (order: 5, 131072 bytes, linear)
[    0.000000] Inode-cache hash table entries: 16384 (order: 4, 65536 bytes, linear)
[    0.000000] Sorting __ex_table...
[    0.000000] mem auto-init: stack:off, heap alloc:off, heap free:off
[    0.000000] Memory: 242900K/262144K available (6192K kernel code, 583K rwdata, 1014K rodata, 221K init, 228K bss, 19244K reserved, 0K cma-reserved)
[    0.000000] SLUB: HWalign=32, Order=0-3, MinObjects=0, CPUs=1, Nodes=1
[    0.000000] rcu: Hierarchical RCU implementation.
[    0.000000] rcu: 	RCU restricting CPUs from NR_CPUS=8 to nr_cpu_ids=1.
[    0.000000] rcu: RCU calculated value of scheduler-enlistment delay is 25 jiffies.
[    0.000000] rcu: Adjusting geometry for rcu_fanout_leaf=16, nr_cpu_ids=1
[    0.000000] NR_IRQS: 64, nr_irqs: 64, preallocated irqs: 0
[    0.000000] riscv-intc: 32 local interrupts mapped
[    0.000000] random: get_random_bytes called from start_kernel+0x360/0x4d0 with crng_init=0
[    0.000000] riscv_timer_init_dt: Registering clocksource cpuid [0] hartid [0]
[    0.000000] clocksource: riscv_clocksource: mask: 0xffffffffffffffff max_cycles: 0x171024e7e0, max_idle_ns: 440795205315 ns
[    0.000020] sched_clock: 64 bits at 100MHz, resolution 10ns, wraps every 4398046511100ns
[    0.009391] Console: colour dummy device 80x25
[    0.013465] printk: console [hvc0] enabled
[    0.021702] printk: bootconsole [sbi0] disabled
[    0.031216] Calibrating delay loop (skipped), value calculated using timer frequency.. 200.00 BogoMIPS (lpj=400000)
[    0.042084] pid_max: default: 32768 minimum: 301
[    0.050330] Mount-cache hash table entries: 1024 (order: 0, 4096 bytes, linear)
[    0.057800] Mountpoint-cache hash table entries: 1024 (order: 0, 4096 bytes, linear)
[    0.090333] rcu: Hierarchical SRCU implementation.
[    0.100103] smp: Bringing up secondary CPUs ...
[    0.104141] smp: Brought up 1 node, 1 CPU
[    0.113616] devtmpfs: initialized
[    0.138636] clocksource: jiffies: mask: 0xffffffff max_cycles: 0xffffffff, max_idle_ns: 7645041785100000 ns
[    0.148685] futex hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.160550] NET: Registered protocol family 16
[    0.375454] SCSI subsystem initialized
[    0.383043] usbcore: registered new interface driver usbfs
[    0.389000] usbcore: registered new interface driver hub
[    0.394575] usbcore: registered new device driver usb
[    0.404215] FPGA manager framework
[    0.408183] Advanced Linux Sound Architecture Driver Initialized.
[    0.425750] clocksource: Switched to clocksource riscv_clocksource
[    0.594461] NET: Registered protocol family 2
[    0.607060] tcp_listen_portaddr_hash hash table entries: 512 (order: 0, 6144 bytes, linear)
[    0.615980] TCP established hash table entries: 2048 (order: 1, 8192 bytes, linear)
[    0.624083] TCP bind hash table entries: 2048 (order: 2, 16384 bytes, linear)
[    0.631497] TCP: Hash tables configured (established 2048 bind 2048)
[    0.638476] UDP hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.645079] UDP-Lite hash table entries: 256 (order: 1, 8192 bytes, linear)
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