package naxriscv.platform

import naxriscv._
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.misc._
import naxriscv.utilities._
import naxriscv.compatibility.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import naxriscv.platform.ScalaInterpreter.evaluate
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnlyArbiter, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.{AxiLite4Clint, WishboneClint}
import spinal.lib.misc.plic.{AxiLite4Plic, WishbonePlic}

import scala.collection.mutable.ArrayBuffer


class NaxRiscvLitex(plugins : ArrayBuffer[Plugin], xlen : Int) extends Component{

  val ramDataWidth = 64
  val ioDataWidth  =  32
  plugins += new FetchAxi4(
    ramDataWidth = ramDataWidth,
    ioDataWidth  = ioDataWidth
  )
  plugins += new DataCacheAxi4(
    dataWidth = ramDataWidth
  )
  plugins += new LsuPeripheralAxiLite4()

  val cpu = new NaxRiscv(
    xlen = xlen,
    plugins
  )

  val ram = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam.toIo()
    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi.toIo()
    Axi4SpecRenamer(ibus)
    Axi4SpecRenamer(dbus)

//    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam
//    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi
//
//    val arbiter = Axi4ReadOnlyArbiter(dbus.config.copy(idWidth = (dbus.config.idWidth max ibus.config.idWidth) + 1), inputsCount = 2)
//    arbiter.io.inputs(0) << ibus
//    arbiter.io.inputs(1) << dbus.toReadOnly()
//
//    val bus = master(Axi4(arbiter.outputConfig))
//    bus << arbiter.io.output
//    bus << dbus.toWriteOnly()
//
//    Axi4SpecRenamer(bus)
  }

  val peripheral = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiIo.toIo()
    val dbus = cpu.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo()
    AxiLite4SpecRenamer(ibus)
    AxiLite4SpecRenamer(dbus)

    val clintCtrl = new AxiLite4Clint(1)
    val plicCtrl = new AxiLite4Plic(
      sourceCount = 32,
      targetCount = 2
    )

    val clint = clintCtrl.io.bus.toIo()
    val plic = plicCtrl.io.bus.toIo()
    val interrupt = plicCtrl.io.sources.toIo()

    AxiLite4SpecRenamer(clint)
    AxiLite4SpecRenamer(plic)

    val priv = cpu.framework.getService[PrivilegedPlugin].io
    priv.int.machine.timer       := clintCtrl.io.timerInterrupt(0)
    priv.int.machine.software    := clintCtrl.io.softwareInterrupt(0)
    priv.int.machine.external    := plicCtrl.io.targets(0)
    priv.int.supervisor.external := plicCtrl.io.targets(1)
    priv.rdtime                  := clintCtrl.io.time
  }
}

object LitexGen extends App{

  var netlistDirectory = "."
  var netlistName = "NaxRiscvLitex"
  var resetVector = 0l
  var xlen = 32
  val files = ArrayBuffer[String]()
  assert(new scopt.OptionParser[Unit]("NaxRiscv") {
    help("help").text("prints this usage text")
    opt[String]("netlist-directory") action { (v, c) => netlistDirectory = v }
    opt[String]("netlist-name") action { (v, c) => netlistName = v }
    opt[String]("scala-file") unbounded() action  { (v, c) => files += v }
    opt[Long]("reset-vector") action  { (v, c) => resetVector = v }
    opt[Int]("xlen") action  { (v, c) => xlen = v }
  }.parse(args))

  val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = netlistDirectory)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  spinalConfig.generateVerilog {


    val codes = ArrayBuffer[String]()
    codes +=
      s"""
         |import scala.collection.mutable.ArrayBuffer
         |import naxriscv.utilities.Plugin
         |import spinal.lib.bus.misc.SizeMapping
         |val plugins = ArrayBuffer[Plugin]()
         |val resetVector = ${resetVector}l
         |val xlen = ${xlen}
         |""".stripMargin
    codes ++= files.map(scala.io.Source.fromFile(_).mkString)
    codes += "plugins\n"
    val code = codes.mkString("\n")
    val plugins = ScalaInterpreter.evaluate[ArrayBuffer[Plugin]](code)
    new NaxRiscvLitex(plugins, xlen).setDefinitionName(netlistName)
  }
}

object ScalaInterpreter extends App{

  def evaluate[T](clazz : String) = {
    import scala.tools.nsc.Settings
    import scala.tools.nsc.interpreter.IMain

    val settings = new Settings
    settings.usejavacp.value = true
    settings.deprecation.value = true

    val aaa = 32
    val eval = new IMain(settings)
    val evaluated = eval.interpret(clazz)
    val res = eval.valueOfTerm("res0").get.asInstanceOf[T]
    res
  }
}


/*
python3 -m litex_boards.targets.digilent_arty --cpu-type=naxriscv --with-ethernet --eth-ip 192.168.178.43 --eth-dynamic-ip  --load
litex_sim --cpu-type=naxriscv --with-sdram --sdram-module=MT41K128M16 --sdram-data-width=16  --sdram-init images/sim.json --trace --trace-fst --trace-start 2000000000000
eth_local_ip 192.168.178.43
eth_remote_ip 192.168.178.32
eth_local_ip  10.42.0.2
eth_remote_ip 10.42.0.1

litex_sim --cpu-type=naxriscv --with-sdram --sdram-module=MT41K128M16 --sdram-data-width=16  --trace --trace-fst --trace-start 2000000000000

netboot
boot 0x40f00000

dtc -O dtb -o rv32.dtb arty_a7.dts
py3tftp -p 69

picocom -b 115200 /dev/ttyUSB1 --imap lfcrlf

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --with-video-framebuffer --with-spi-sdcard --with-ethernet  --build --load

export DISPLAY=:0
twm &
xinit Xorg


chocolate-doom -2 &
sleep 3
WID=$(xdotool getwindowfocus)
xdotool windowmove $WID 100 0

chocolate-doom -1 &
sleep 3
WID=$(xdotool getwindowfocus)
xdotool windowmove $WID 100 100

chocolate-doom -1 &
sleep 3
WID=$(xdotool getwindowfocus)
xdotool windowmove $WID 100 300

chocolate-doom -2 -timedemo demo1.lmp
Nax : timed 5026 gametics in 4958 realtics (35.480034 fps)
      timed 5026 gametics in 4866 realtics (36.150841 fps)
      timed 5026 gametics in 4219 realtics (41.694714 fps)
      timed 5026 gametics in 3692 realtics (47.646263 fps)
      timed 5026 gametics in 3444 realtics (51.077236 fps) (no more memcpy for doom flush)
      timed 5026 gametics in 2369 realtics (74.254959 fps) (-1)
Vex : timed 5026 gametics in 5606 realtics (31.378880 fps)
      timed 5026 gametics in 5238 realtics (33.583427 fps)
      timed 5026 gametics in 4866 realtics (36.150841 fps) (-1)

chocolate-doom -2 -timedemo demo1.lmp  -noblit
Nax : timed 5026 gametics in 2040 realtics (86.230392 fps)
      timed 5026 gametics in 1965 realtics (89.521629 fps)
      timed 5026 gametics in 1926 realtics (91.334373 fps)
      timed 5026 gametics in 1789 realtics (98.328674 fps)
      timed 5026 gametics in 1781 realtics (98.770355 fps)
Vex : timed 5026 gametics in 3851 realtics (45.679043 fps)

no draw no blit :
Nax : timed 5026 gametics in 277 realtics (635.054138 fps


root@buildroot:~# ramspeed -b 1 -g  1
RAMspeed (GENERIC) v2.6.0 by Rhett M. Hollander and Paul V. Bolotoff, 2002-09

1Gb per pass mode

INTEGER & WRITING         1 Kb block: 378.26 MB/s
INTEGER & WRITING         2 Kb block: 382.95 MB/s
INTEGER & WRITING         4 Kb block: 379.85 MB/s
INTEGER & WRITING         8 Kb block: 382.67 MB/s
INTEGER & WRITING        16 Kb block: 367.09 MB/s
INTEGER & WRITING        32 Kb block: 140.12 MB/s
INTEGER & WRITING        64 Kb block: 124.59 MB/s
INTEGER & WRITING       128 Kb block: 124.20 MB/s
INTEGER & WRITING       256 Kb block: 121.18 MB/s
INTEGER & WRITING       512 Kb block: 122.01 MB/s



make CROSS_COMPILE=riscv-none-embed- PLATFORM=litex/vexriscv
cp build/platform/litex/vexriscv/firmware/fw_jump.bin ../../litex_naxriscv_test/images/opensbi.bin
cp build/platform/litex/vexriscv/firmware/fw_jump.elf ../../litex_naxriscv_test/images/opensbi.elf

SDL_VIDEODRIVER=fbcon chocolate-doom
SDL_VIDEODRIVER=directfb chocolate-doom


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