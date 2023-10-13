// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.platform

import naxriscv._
import naxriscv.fetch._
import naxriscv.lsu._
import naxriscv.misc._
import naxriscv.utilities._
import naxriscv.compatibility.{EnforceSyncRamPhase, MemReadDuringWritePatcherPhase, MultiPortWritesSymplifier}
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.platform.ScalaInterpreter.evaluate
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnlyArbiter, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.misc.{AxiLite4Clint, WishboneClint}
import spinal.lib.misc.plic.{AxiLite4Plic, WishbonePlic}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class NaxRiscvLitex(plugins : ArrayBuffer[Plugin], xlen : Int, toPeripheral : UInt => Bool) extends Component{

  val ramDataWidth = 64
  val ioDataWidth  = 32
  plugins += new FetchAxi4(
    ramDataWidth = ramDataWidth,
    ioDataWidth  = ioDataWidth,
    toPeripheral = cmd => toPeripheral(cmd.address)
  )
  plugins += new DataCacheAxi4(
    dataWidth = ramDataWidth
  )
  plugins += new LsuPeripheralAxiLite4(
    ioDataWidth  = ioDataWidth
  )

  val cpu = new NaxRiscv(
    plugins
  )

  val ram = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam.toIo()
    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi.toIo()
    Axi4SpecRenamer(ibus)
    Axi4SpecRenamer(dbus)

    plugins.foreach{
      case p : EmbeddedJtagPlugin => {
        if(p.withTap) p.logic.jtag.toIo().setName("jtag")
        else p.logic.jtagInstruction.toIo().setName("jtag_instruction")
        p.logic.ndmreset.toIo().setName("debug_ndmreset")
      }
      case _ =>
    }

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
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiPeripheral.toIo()
    val dbus = cpu.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo()
    AxiLite4SpecRenamer(ibus)
    AxiLite4SpecRenamer(dbus)

    val clintCtrl = new AxiLite4Clint(1, bufferTime = xlen == 64)
    val plicCtrl = new AxiLite4Plic(
      sourceCount = 31,
      targetCount = 2
    )

    val clint = clintCtrl.io.bus.toIo()
    val plic = plicCtrl.io.bus.toIo()
    val interrupt = in Bits(32 bits)
    plicCtrl.io.sources := interrupt >> 1

    AxiLite4SpecRenamer(clint)
    AxiLite4SpecRenamer(plic)

    val priv = cpu.framework.getService[PrivilegedPlugin].io
    priv.int.machine.timer       := clintCtrl.io.timerInterrupt(0)
    priv.int.machine.software    := clintCtrl.io.softwareInterrupt(0)
    priv.int.machine.external    := plicCtrl.io.targets(0)
    if(priv.int.supervisor != null) priv.int.supervisor.external := plicCtrl.io.targets(1)
    priv.rdtime                  := clintCtrl.io.time
  }
}

case class LitexMemoryRegion(mapping : SizeMapping, mode : String, bus : String){
  def isIo = mode.contains("i") || mode.contains("o")
  def isExecutable = mode.contains("x")
  def isCachable = mode.contains("c")
  def onPeripheral = bus match {
    case "m" => false
    case "p" => true
  }
  def onMemory = !onPeripheral
}

object LitexGen extends App{
  var netlistDirectory = "."
  var netlistName = "NaxRiscvLitex"
  var resetVector = 0l
  var xlen = 32
  var jtagTap = false
  var jtagInstruction = false
  var debug = false
  val files = ArrayBuffer[String]()
  val scalaArgs = ArrayBuffer[String]()
  val memoryRegions = ArrayBuffer[LitexMemoryRegion]()

  assert(new scopt.OptionParser[Unit]("NaxRiscv") {
    help("help").text("prints this usage text")
    opt[String]("netlist-directory") action { (v, c) => netlistDirectory = v }
    opt[String]("netlist-name") action { (v, c) => netlistName = v }
    opt[String]("scala-file") unbounded() action  { (v, c) => files += v }
    opt[String]("scala-args") unbounded() action  { (v, c) =>
      val elements = v.split(",").map(_.split("="))
      for(e <- elements) scalaArgs += s"""args("${e(0)}") = ${e(1)}"""
    }
    opt[Long]("reset-vector") action  { (v, c) => resetVector = v }
    opt[Int]("xlen") action  { (v, c) => xlen = v }
    opt[Unit]("with-jtag-tap") action  { (v, c) => jtagTap = true }
    opt[Unit]("with-jtag-instruction") action  { (v, c) => jtagInstruction = true }
    opt[Unit]("with-debug") action  { (v, c) => debug = true }
    opt[Seq[String]]("memory-region") unbounded() action  { (v, c) =>
      assert(v.length == 4, "--memory-region need 4 parameters")
      val r = new LitexMemoryRegion(SizeMapping(BigInt(v(0)), BigInt(v(1))), v(2), v(3))
      memoryRegions += r
      assert(!(r.onMemory && !r.isCachable), s"Region $r isn't supported by NaxRiscv, data cache will always cache memory")
      assert(!(r.onMemory &&  r.isIo ), s"Region $r isn't supported by NaxRiscv, IO have to be on peripheral bus")
    }
  }.parse(args, Unit).nonEmpty)

  val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = netlistDirectory)
  spinalConfig.addTransformationPhase(new MemReadDuringWritePatcherPhase)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  spinalConfig.generateVerilog {

    val codes = ArrayBuffer[String]()
    codes +=
      s"""
         |import scala.collection.mutable.ArrayBuffer
         |import naxriscv.utilities.Plugin
         |import naxriscv.platform.LitexMemoryRegion
         |import spinal.lib.bus.misc.SizeMapping
         |val plugins = ArrayBuffer[Plugin]()
         |val resetVector = ${resetVector}l
         |val args = scala.collection.mutable.LinkedHashMap[String, Any]()
         |val xlen = ${xlen}
         |val jtagTap = ${jtagTap}
         |val jtagInstruction = ${jtagInstruction}
         |val debug = ${debug}
         |def arg[T](key : String, default : T) = args.getOrElse(key, default).asInstanceOf[T]
         |
         |
         |${scalaArgs.mkString("\n")}
         |""".stripMargin
    codes ++= files.map(scala.io.Source.fromFile(_).mkString)
    codes += "plugins\n"
    val code = codes.mkString("\n")
    val plugins = ScalaInterpreter.evaluate[ArrayBuffer[Plugin]](code, List(
      ("memoryRegions", "Seq[naxriscv.platform.LitexMemoryRegion]", memoryRegions)
    ))
    new NaxRiscvLitex(plugins, xlen, address => memoryRegions.filter(_.onPeripheral).map(_.mapping.hit(address)).orR).setDefinitionName(netlistName)
  }
}

object ScalaInterpreter extends App{

  def evaluate[T](clazz : String, binds : Seq[(String, String, Any)]) = {
    import scala.tools.nsc.Settings
    import scala.tools.nsc.interpreter.IMain

    val settings = new Settings
    settings.usejavacp.value = true
    settings.deprecation.value = true

    val aaa = 32
    val eval = new IMain(settings)
    for(bind <- binds) eval.bind(bind._1, bind._2, bind._3)
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

tar -cvf chroot.tar debian-riscv64-tarball-20180418
curl --upload-file chroot.tar https://transfer.sh/chroot.tar

mount -o remount,rw /

picocom -b 115200 /dev/ttyUSB1 --imap lfcrlf
qemu-img convert -f qcow2 -O raw sid.qcow2 /dev/sdb3
qemu image => https://wiki.debian.org/RISC-V#OS_.2F_filesystem_imageshttps://gist.github.com/shamil/62935d9b456a6f9877b5
sudo gedit /media/rawrr/rootfs/lib/systemd/system/getty@.service
https://gist.github.com/shamil/62935d9b456a6f9877b5

-drive file=/dev/sdb,format=raw,if=virtio

nano /etc/ssh/sshd_config
PermitRootLogin yes

sudo modprobe nbd max_part=8
sudo qemu-nbd --connect=/dev/nbd0 sid.qcow2
sudo fdisk /dev/nbd0 -l
sudo mount /dev/nbd0p1 part

sudo umount part
sudo qemu-nbd --disconnect /dev/nbd0
sudo rmmod nbd

sudo dd if=/dev/nbd0p1 of=/dev/sdb3

ttyLXU0
systemctl enable serial-getty@ttyLXU0.service
/etc/systemd/system/getty.target.wants/serial-getty@ttyLXU0.service

rm -rf /etc/systemd/system/serial-getty@hvc0.service
systemctl enable serial-getty@hvc0.service
sudo nano /etc/resolv.conf => 8.8.8.8

ip addr del 10.0.2.15/24 dev eth0
ip addr change 192.168.1.50/24 dev eth0
ip addr show
ip route add default via 192.168.1.100

date -s "19 AUG 2022 14:47"

export SDL_VIDEODRIVER=directfb
/usr/games/openttd  -r 640x480 -b 8bpp-optimized -g -s null -m null

wget https://file-examples.com/storage/fe5467a6a163010b197fb20/2017/11/file_example_MP3_1MG.mp3
mpg123 -w wave.wav file_example_MP3_1MG.mp3

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --with-video-framebuffer --with-spi-sdcard --with-ethernet  --build --load

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --with-video-framebuffer --with-spi-sdcard --with-ethernet --xlen=32 --scala-args='rvc=true,alu-count=1,decode-count=1' --with-jtag-tap --build --load

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --with-video-framebuffer --with-spi-sdcard --with-ethernet --xlen=32 --scala-args='rvc=true,alu-count=1,decode-count=1' --with-jtag-instruction --build --load

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --with-video-framebuffer --with-spi-sdcard --with-ethernet --xlen=32 --scala-args='rvc=false,rvf=false,rvd=false' --with-jtag-instruction --csr-csv build/digilent_nexys_video/csr.csv --csr-json build/digilent_nexys_video/csr.json --load

curl https://www.ports.debian.org/archive_2022.key | apt-key add -

extract cpio =>
sudo cpio -iv < /tmp/archive.cpio

Error opening terminal: vt100.

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
      timed 5026 gametics in 3395 realtics (51.814434 fps)
      timed 5026 gametics in 3187 realtics (55.196110 fps) (LSU2)
      timed 5026 gametics in 2369 realtics (74.254959 fps) (-1)
      timed 5026 gametics in 2301 realtics (76.449371 fps) (-1)
      timed 5026 gametics in 2375 realtics (74.067368 fps) (LSU2)
Vex : timed 5026 gametics in 5606 realtics (31.378880 fps)
      timed 5026 gametics in 5238 realtics (33.583427 fps)
      timed 5026 gametics in 4866 realtics (36.150841 fps) (-1)

chocolate-doom -2 -timedemo demo1.lmp  -noblit
Nax : timed 5026 gametics in 2040 realtics (86.230392 fps)
      timed 5026 gametics in 1965 realtics (89.521629 fps)
      timed 5026 gametics in 1926 realtics (91.334373 fps)
      timed 5026 gametics in 1789 realtics (98.328674 fps)
      timed 5026 gametics in 1781 realtics (98.770355 fps)
      timed 5026 gametics in 1786 realtics (98.493843 fps)
      timed 5026 gametics in 1751 realtics (100.462593 fps)
      timed 5026 gametics in 1737 realtics (101.272308 fps) (LSU2)
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

obj_dir/VNaxRiscv --name play --load-elf ../../../../ext/NaxSoftware/baremetal/framebuffer/build/rv32ima/framebuffer.elf --framebuffer 0x80200000,800,600


export LINUX_IMAGES=$NAXRISCV/../imageDoom
./obj_dir/VNaxRiscv \
    --framebuffer 0x80200000,800,600  \
    --load-bin $LINUX_IMAGES/fw_jump.bin,0x80000000 \
    --load-bin $LINUX_IMAGES/linux.dtb,0x80F80000 \
    --load-bin $LINUX_IMAGES/Image,0x80400000 \
    --load-bin $LINUX_IMAGES/rootfs.cpio,0x81000000 \
     --no-stdin                  \
     --no-putc-flush          \
     --output-dir output/seed$1_lat$2 \
     --seed=$1 \
     --memory-latency $2 \
     --getc "buildroot login" \
     --success


root
export DISPLAY=:0
chocolate-doom -2 -timedemo demo1.lmp

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --bus-standard axi-lite --with-video-framebuffer --with-spi-sdcard --with-ethernet --xlen=64 --scala-args='rvc=true,rvf=true,rvd=true' --with-jtag-tap --build --load
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=naxriscv  --bus-standard axi-lite --with-video-framebuffer --with-spi-sdcard --with-ethernet --xlen=64 --scala-args='rvc=true,rvf=true,rvd=true,alu-count=1,decode-count=1' --with-jtag-instruction --build --load
python3 -m litex_boards.targets.digilent_arty --variant=a7-100  --cpu-type=naxriscv  --with-spi-sdcard --with-ethernet --xlen=64 --scala-args='rvc=true,rvf=true,rvd=true,alu-count=1,decode-count=1'  --build --load
python3 -m litex_boards.targets.efinix_titanium_ti60_f225_dev_kit --cpu-type=naxriscv --xlen=32 --scala-args='rvc=false,rvf=false,rvd=false,alu-count=1,decode-count=1,mmu=false,supervisor=false,distributed-ram=false,dispatch-slots=16,rob-size=32' --build



./make.py --board=arty --variant=a7-100 --cpu-count=1 --load

Starting Xorg: OK

Welcome to Buildroot
buildroot login: root
                   __   _
                  / /  (_)__  __ ____ __
                 / /__/ / _ \/ // /\ \ /
                /____/_/_//_/\_,_//_\_\
                      / _ \/ _ \
   __   _ __      _  _\___/_//_/         ___  _
  / /  (_) /____ | |/_/__| | / /____ __ / _ \(_)__ _____  __
 / /__/ / __/ -_)>  </___/ |/ / -_) \ // , _/ (_-</ __/ |/ /
/____/_/\__/\__/_/|_|____|___/\__/_\_\/_/|_/_/___/\__/|___/
                  / __/  |/  / _ \
                 _\ \/ /|_/ / ___/
                /___/_/  /_/_/
  32-bit RISC-V Linux running on LiteX / VexRiscv-SMP.

root@buildroot:~# export DISPLAY=:0
root@buildroot:~# chocolate-doom -2 -timedemo demo1.lmp
                         Chocolate Doom 3.0.1
Z_Init: Init zone memory allocation daemon.
zone memory: 0x947e0010, 1000000 allocated for zone
Using /root/.local/share/chocolate-doom/ for configuration and saves
V_Init: allocate screens.
M_LoadDefaults: Load system defaults.
saving config in /root/.local/share/chocolate-doom/default.cfg
W_Init: Init WADfiles.
 adding /usr/share/games/doom/doom1.wad
 adding demo1.lmp
Playing demo demo1.lmp.
===========================================================================
                            DOOM Shareware
===========================================================================
 Chocolate Doom is free software, covered by the GNU General Public
 License.  There is NO warranty; not even for MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. You are welcome to change and distribute
 copies under certain conditions. See the source for more information.
===========================================================================
I_Init: Setting up machine state.
Error initialising SDL_mixer: No such audio device
Error initialising SDL_mixer: No such audio device
OPL_Init: Failed to find a working driver.
Dude.  The Adlib isn't responding.
NET_Init: Init network subsystem.
M_Init: Init miscellaneous info.
R_Init: Init DOOM refresh daemon - [...................]
P_Init: Init Playloop state.
S_Init: Setting up sound.
D_CheckNetGame: Checking network game status.
startskill 2  deathmatch: 0  startmap: 1  startepisode: 1
player 1 of 1 (1 nodes)
Emulating the behavior of the 'Doom 1.9' executable.
HU_Init: Setting up heads up display.
ST_Init: Init status bar.
AdjustWindowSize window size =640,480
AdjustWindowSize window size =640,480
CreateUpscaledTexture: Limited texture size to 0x0 (max 16000000 pixels, max texture size 0x0)
The framebuffer device was opened successfully.
I_InitFb 800x600, 3200 32bpp


[    0.465672] TCP bind hash table entries: 4096 (order: 3, 32768 bytes, linear)
[    0.472807] TCP: Hash tables configured (established 4096 bind 4096)
[    0.479726] UDP hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.485566] UDP-Lite hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.493606] NET: Registered PF_UNIX/PF_LOCAL protocol family
[    0.505119] Unpacking initramfs...
[    0.588394] workingset: timestamp_bits=30 max_order=17 bucket_order=0
[    0.644997] io scheduler mq-deadline registered
[    0.648573] io scheduler kyber registered
[    0.731987] LiteX SoC Controller driver initialized
[    1.548772] f0001000.serial: ttyLXU0 at MMIO 0x0 (irq = 0, base_baud = 0) is a liteuart
[    1.560970] printk: console [liteuart0] enabled
[    1.560970] printk: console [liteuart0] enabled
[    1.569870] printk: bootconsole [liteuart0] disabled
[    1.569870] printk: bootconsole [liteuart0] disabled
[    1.639391] liteeth f0002000.mac eth0: irq 2 slots: tx 2 rx 2 size 2048
[    1.665203] mousedev: PS/2 mouse device common for all mice
[    1.671086] i2c_dev: i2c /dev entries driver
[    1.704585] mmc_spi spi0.0: SD/MMC host mmc0, no WP, no poweroff, cd polling
[    1.723909] NET: Registered PF_INET6 protocol family
[    1.755242] mmc0: host does not support reading read-only switch, assuming write-enable
[    1.762792] mmc0: new SDHC card on SPI
[    1.791912] Segment Routing with IPv6
[    1.794631] In-situ OAM (IOAM) with IPv6
[    1.799091] sit: IPv6, IPv4 and MPLS over IPv4 tunneling driver
[    1.809380] NET: Registered PF_PACKET protocol family
[    1.819656] mmcblk0: mmc0:0000       14.7 GiB
[    1.856272]  mmcblk0: p1
[    3.332516] Initramfs unpacking failed: invalid magic at start of compressed archive
[    3.743193] Freeing initrd memory: 40960K
[    3.751280] Freeing unused kernel image (initmem) memory: 212K
[    3.756451] Kernel memory protection not selected by kernel config.
[    3.762731] Run /init as init process



dri => -extension GLX ?
lsof /usr/lib/riscv64-linux-gnu/dri/swrast_dri.so

/etc/X11/xorg.conf
Section "Extensions"
	Option "GLX" "Disable"
EndSection

cat /var/log/Xorg.0.log

xauth -f /tmp/lightdmauth
list
exit


cp /var/run/lightdm/root/:0 /tmp/lightdmauth
chmod a+r /tmp/lightdmauth
export XAUTHORITY=/tmp/lightdmauth
export DISPLAY=unix:0

xdotool type root
xdotool key Tab
xdotool type root
xdotool key Return


nohup /usr/games/openttd -v sdl -b 8bpp-optimized -s null -m null &
nohup /usr/games/chocolate-doom -iwad Doom1.WAD  -1 -nosound &
SDL_NOMOUSE=1 nohup VisualBoyAdvance -1  emu/Tetris.gb &
nohup xterm -geometry 120x15 -e  watch whetstone/raystones &

Debian setup :
set dns, configure eth0, enable time over internet
enable root ssh
disable x11 GLX extention
enable HVC0

 */
