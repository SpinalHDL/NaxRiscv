# NaxRiscv

An RISC-V core currently characterised by : 

- Out of order execution
- Superscalar (ex : 2 decode, 3 execution units, 2 retire)
- RV32IMASU (Linux and freertos works in simulation)
- Portable HDL, but target FPGA with distributed ram (Xilinx series 7 is the reference used so far)
- Target a (relatively) low area usage and high fmax (not the best IPC)
- Decentralized hardware elaboration (Empty toplevel parametrized with plugins)
- Frontend implemented around a pipelining framework to ease customisation
- Non-blocking Data cache with multiple refill and writeback slots
- BTB + GSHARE + RAS branch predictors
- Hardware refilled MMU
- Load to use latency of 3 cycles via the speculative cache hit predictor 
- Pipeline visualisation via verilator simulation and Konata (gem5 file format)

# Running Verilator simulation

See src/test/cpp/naxriscv/README.md

Linux/Buildroot running in simulation : 

```
./obj_dir/VNaxRiscv \
    --load-bin $LINUX_IMAGES/fw_jump.bin,0x80000000 \
    --load-bin $LINUX_IMAGES/linux.dtb,0x80F80000 \
    --load-bin $LINUX_IMAGES/Image,0x80400000 \
    --load-bin $LINUX_IMAGES/rootfs.cpio,0x81000000 
OpenSBI v0.8
   ____                    _____ ____ _____
  / __ \                  / ____|  _ \_   _|
 | |  | |_ __   ___ _ __ | (___ | |_) || |
 | |  | | '_ \ / _ \ '_ \ \___ \|  _ < | |
 | |__| | |_) |  __/ | | |____) | |_) || |_
  \____/| .__/ \___|_| |_|_____/|____/_____|
        | |
        |_|

Platform Name       : NaxRiscv
Platform Features   : timer,mfdeleg
Platform HART Count : 1
Boot HART ID        : 0
Boot HART ISA       : rv32imasu
BOOT HART Features  : scounteren,mcounteren
BOOT HART PMP Count : 0
Firmware Base       : 0x80000000
Firmware Size       : 64 KB
Runtime SBI Version : 0.2

MIDELEG : 0x00000222
MEDELEG : 0x0000b109
[    0.000000] Linux version 5.10.1 (rawrr@rawrr) (riscv32-buildroot-linux-gnu-gcc.br_real (Buildroot 2020.11-rc3-8-g9ef54b7d0b) 10.2.0, GNU ld (GNU Binutils) 2.34) #2 SMP Wed Jan 26 14:18:17 CET 2022
[    0.000000] earlycon: sbi0 at I/O port 0x0 (options '')
[    0.000000] printk: bootconsole [sbi0] enabled
[    0.000000] Initial ramdisk at: 0x(ptrval) (8388608 bytes)
[    0.000000] Zone ranges:
[    0.000000]   Normal   [mem 0x0000000080400000-0x000000008fffffff]
[    0.000000] Movable zone start for each node
[    0.000000] Early memory node ranges
[    0.000000]   node   0: [mem 0x0000000080400000-0x000000008fffffff]
[    0.000000] Initmem setup node 0 [mem 0x0000000080400000-0x000000008fffffff]
[    0.000000] SBI specification v0.2 detected
[    0.000000] SBI implementation ID=0x1 Version=0x8
[    0.000000] SBI v0.2 TIME extension detected
[    0.000000] SBI v0.2 IPI extension detected
[    0.000000] SBI v0.2 RFENCE extension detected
[    0.000000] SBI v0.2 HSM extension detected
[    0.000000] riscv: ISA extensions aim
[    0.000000] riscv: ELF capabilities aim
[    0.000000] percpu: Embedded 10 pages/cpu s18700 r0 d22260 u40960
[    0.000000] Built 1 zonelists, mobility grouping on.  Total pages: 64008
[    0.000000] Kernel command line: rootwait console=hvc0 earlycon=sbi root=/dev/ram0 init=/sbin/init
[    0.000000] Dentry cache hash table entries: 32768 (order: 5, 131072 bytes, linear)
[    0.000000] Inode-cache hash table entries: 16384 (order: 4, 65536 bytes, linear)
[    0.000000] Sorting __ex_table...
[    0.000000] mem auto-init: stack:off, heap alloc:off, heap free:off
[    0.000000] Memory: 241280K/258048K available (4717K kernel code, 553K rwdata, 632K rodata, 166K init, 213K bss, 16768K reserved, 0K cma-reserved)
[    0.000000] SLUB: HWalign=64, Order=0-3, MinObjects=0, CPUs=1, Nodes=1
[    0.000000] rcu: Hierarchical RCU implementation.
[    0.000000] rcu: 	RCU restricting CPUs from NR_CPUS=8 to nr_cpu_ids=1.
[    0.000000] rcu: RCU calculated value of scheduler-enlistment delay is 25 jiffies.
[    0.000000] rcu: Adjusting geometry for rcu_fanout_leaf=16, nr_cpu_ids=1
[    0.000000] NR_IRQS: 64, nr_irqs: 64, preallocated irqs: 0
[    0.000000] riscv-intc: 32 local interrupts mapped
[    0.000000] random: get_random_bytes called from start_kernel+0x35c/0x4dc with crng_init=0
[    0.000000] riscv_timer_init_dt: Registering clocksource cpuid [0] hartid [0]
[    0.000000] clocksource: riscv_clocksource: mask: 0xffffffffffffffff max_cycles: 0x171024e7e0, max_idle_ns: 440795205315 ns
[    0.000096] sched_clock: 64 bits at 100MHz, resolution 10ns, wraps every 4398046511100ns
[    0.001452] Console: colour dummy device 80x25
[    0.001923] printk: console [hvc0] enabled
[    0.001923] printk: console [hvc0] enabled
[    0.002597] printk: bootconsole [sbi0] disabled
[    0.002597] printk: bootconsole [sbi0] disabled
[    0.003469] Calibrating delay loop (skipped), value calculated using timer frequency.. 200.00 BogoMIPS (lpj=400000)
[    0.004346] pid_max: default: 32768 minimum: 301
[    0.006040] Mount-cache hash table entries: 1024 (order: 0, 4096 bytes, linear)
[    0.006715] Mountpoint-cache hash table entries: 1024 (order: 0, 4096 bytes, linear)
[    0.018275] rcu: Hierarchical SRCU implementation.
[    0.021169] smp: Bringing up secondary CPUs ...
[    0.021752] smp: Brought up 1 node, 1 CPU
[    0.024190] devtmpfs: initialized
[    0.029478] clocksource: jiffies: mask: 0xffffffff max_cycles: 0xffffffff, max_idle_ns: 7645041785100000 ns
[    0.030479] futex hash table entries: 256 (order: 2, 16384 bytes, linear)
[    0.032419] NET: Registered protocol family 16
[    0.072397] clocksource: Switched to clocksource riscv_clocksource
[    0.130361] NET: Registered protocol family 2
[    0.135484] tcp_listen_portaddr_hash hash table entries: 512 (order: 0, 6144 bytes, linear)
[    0.136588] TCP established hash table entries: 2048 (order: 1, 8192 bytes, linear)
[    0.137670] TCP bind hash table entries: 2048 (order: 2, 16384 bytes, linear)
[    0.138674] TCP: Hash tables configured (established 2048 bind 2048)
[    0.139694] UDP hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.140653] UDP-Lite hash table entries: 256 (order: 1, 8192 bytes, linear)
[    0.145111] Unpacking initramfs...
[    0.382796] Initramfs unpacking failed: invalid magic at start of compressed archive
[    0.438458] Freeing initrd memory: 8192K
[    0.442311] workingset: timestamp_bits=30 max_order=16 bucket_order=0
[    0.497197] Block layer SCSI generic (bsg) driver version 0.4 loaded (major 254)
[    0.497827] io scheduler mq-deadline registered
[    0.498260] io scheduler kyber registered
[    0.863943] NET: Registered protocol family 10
[    0.869801] Segment Routing with IPv6
[    0.870713] sit: IPv6, IPv4 and MPLS over IPv4 tunneling driver
[    0.875543] NET: Registered protocol family 17
[    0.880356] Freeing unused kernel memory: 164K
[    0.880786] Kernel memory protection not selected by kernel config.
[    0.881380] Run /init as init process
Starting syslogd: OK
Starting klogd: OK
Running sysctl: OK
Saving random seed: [    1.502669] random: dd: uninitialized urandom read (512 bytes read)
OK
Starting network: OK

Welcome to Buildroot
buildroot login: root
           _  _                     ___      _
    o O O | \| |   __ _    __ __   | _ \    (_)     ___     __     __ __
   o      | .` |  / _` |   \ \ /   |   /    | |    (_-<    / _|    \ V /
  TS__[O] |_|\_|  \__,_|   /_\_\   |_|_\   _|_|_   /__/_   \__|_   _\_/_
 {======|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|
./o--000'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'
login[65]: root login on 'console'
root@buildroot:~# cat /proc/cpuinfo
processor	: 0
hart		: 0
isa		: rv32ima
mmu		: sv32

root@buildroot:~# echo 1+2+3*4 | bc
15
root@buildroot:~# micropython
MicroPython v1.13 on 2022-01-26; linux version
Use Ctrl-D to exit, Ctrl-E for paste mode
>>> 1+2+3
6
>>> import math
>>> math.sin(math.pi/4)
0.7071067811865475
>>> from sys import exit
>>> exit()
root@buildroot:~# ls /
bin      init     linuxrc  opt      run      tmp
dev      lib      media    proc     sbin     usr
etc      lib32    mnt      root     sys      var
root@buildroot:~#
```

# Framework

The toplevel of NaxRiscv only contains a framework which can schedule a list plugins. The framework itself does not create any hardware.

```scala
val framework = new Framework(plugins())
```

To give an overview of how much the design is splited between plugins, here is the list of them for one functional CPU :

```
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
    plugins += new MmuPlugin(
      spec    = MmuSpec.sv32,
      ioRange = _(31 downto 28) === 0x1
    )

    //FETCH
    plugins += new FetchPlugin()
    plugins += new PcPlugin()
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      memDataWidth = Fetch.FETCH_DATA_WIDTH,
      reducedBankWidth = false,
      hitsWithTranslationWays = true,
      translationStorageParameter = MmuStorageParameter(
        levels   = List(
          MmuStorageLevel(
            id    = 0,
            ways  = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id    = 1,
            ways  = 2,
            depth = 32
          )
        ),
        priority = 0
      ),
      translationPortParameter  = MmuPortParameter(
        readAt = 0,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
    )
    plugins += new AlignerPlugin(inputAt = 2)

    //FRONTEND
    plugins += new FrontendPlugin()
    plugins += new DecompressorPlugin()
    plugins += new DecoderPlugin()
    plugins += new RfTranslationPlugin()
    plugins += new RfDependencyPlugin()
    plugins += new RfAllocationPlugin(riscv.IntRegFile)
    plugins += new DispatchPlugin(
      slotCount = 32
    )

    //BRANCH PREDICTION
    plugins += new BranchContextPlugin(
      branchCount = 16
    )
    plugins += new HistoryPlugin(
      historyFetchBypass = true
    )
    plugins += new DecoderPredictionPlugin()
    plugins += new BtbPlugin(
      entries = 512,
      readAt = 0,
      hitAt = 1,
      jumpAt = 1
    )
    plugins += new GSharePlugin(
      memBytes = 4 KiB,
      historyWidth = 24,
      readAt = 0
    )

    //LOAD / STORE
    plugins += new LsuPlugin(
      lqSize = 16,
      sqSize = 16,
      loadToCacheBypass = true,
      lqToCachePipelined = true,
      hazardPedictionEntries = 512,
      hazardPredictionTagWidth = 16,
      hitPedictionEntries = 1024,
      translationStorageParameter = MmuStorageParameter(
        levels   = List(
          MmuStorageLevel(
            id    = 0,
            ways  = 4,
            depth = 32
          ),
          MmuStorageLevel(
            id    = 1,
            ways  = 2,
            depth = 32
          )
        ),
        priority = 1
      ),
      loadTranslationParameter  = MmuPortParameter(
        readAt = 0,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      ),
      storeTranslationParameter = MmuPortParameter(
        readAt = 1,
        hitsAt = 1,
        ctrlAt = 1,
        rspAt  = 1
      )
    )
    plugins += new DataCachePlugin(
      memDataWidth = 64,
      cacheSize    = 4096*4,
      wayCount     = 4,
      refillCount = 2,
      writebackCount = 2,
      tagsReadAsync = true,
      reducedBankWidth = false,
      loadRefillCheckEarly = false
    )

    //MISC
    plugins += new RobPlugin(
      completionWithReg = false
    )
    plugins += new CommitPlugin(
      ptrCommitRetimed = true
    )
    plugins += new RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 64,
      bankCount = 1
    )
    plugins += new CommitDebugFilterPlugin(List(4, 8, 12))
    plugins += new CsrRamPlugin()
    plugins += new PrivilegedPlugin(PrivilegedConfig.full)
    plugins += new PerformanceCounterPlugin(
      additionalCounterCount = 4,
      bufferWidth            = 6
    )

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("EU0")
    plugins += new SrcPlugin("EU0", earlySrc = true)
    plugins += new IntAluPlugin("EU0", aluStage = 0)
    plugins += new ShiftPlugin("EU0" , aluStage = 0)

    plugins += new ExecutionUnitBase("EU1", writebackCountMax = 1)
    plugins += new SrcPlugin("EU1", earlySrc = true)
    plugins += new MulPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU1", writebackAt = 2)
    plugins += new BranchPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new StorePlugin("EU1")
    plugins += new CsrAccessPlugin("EU1")(
      decodeAt = 0,
      readAt = 1,
      writeAt = 2,
      writebackAt = 2,
      staticLatency = false
    )
    plugins += new EnvCallPlugin("EU1")(rescheduleAt = 2)

    plugins += new ExecutionUnitBase("EU2", writebackCountMax = 0)
    plugins += new SrcPlugin("EU2")
    plugins += new LoadPlugin("EU2")
```

Each of those plugins may :

- Implement services used by other plugins (ex : provide jump interfaces, provide rescheduling interface, provide a pipeline skeleton)
- Use other plugins functionalities
- Create hardware
- Create early tasks (used to setup things between plugins)
- Create late tasks (used in general to create the required hardware)

### Plugin tasks

Here is can instance of dummy plugin creating two tasks (setup / logic): 

```scala
class DummyPlugin() extends Plugin {
  val setup = create early new Area {
    //Here you can setup things with other plugins
    //This code will always run before any late tasks
  }

  val logic = create late new Area {
    //Here you can (for instance) generate hardware
    //This code will always start after any early task
  }
}
```

Note that create early and create late will execute their code into a new threads, which are scheduled by the Framework class.

### Service definition

For instance, the JumpService, allowing other plugins to order jumps, is defined as following : 

```
trait JumpService extends Service{
  def createJumpInterface(priority : Int) : Flow[JumpCmd] 
}
case class JumpCmd(pcWidth : Int) extends Bundle{
  val pc = UInt(pcWidth bits)
}
```

### Service implementation

The PcPlugin can implement the JumpService the following way :

```scala
case class JumpSpec(interface :  Flow[JumpCmd], priority : Int)
class PcPlugin() extends Plugin with JumpService{
  val jumpsSpec = ArrayBuffer[JumpSpec]()
  
  override def createJumpInterface(priority : Int): Flow[JumpCmd] = {
    val spec = JumpSpec(Flow(JumpCmd(32)), priority)
    jumpsSpec += spec
    return spec.interface
  }

  val logic = create late new Area{
    //Here, implement the PC logic and manage the jumpsSpec interfaces
  }
}
```

### Service usage

Then another plugin can retrieve and use this service by : 

```scala
class AnotherPlugin() extends Plugin {
  val setup = create early new Area {
    val jump = getService[JumpService].createJumpInterface(42)
  }

  val logic = create late new Area {
    setup.jump.valid := ???
    setup.jump.pc := ???
  }
}
```

### Service Pipeline definition

Some plugins may even create pipeline skeleton which can then be populated by other plugins. For instance : 

```scala
class FetchPlugin() extends Plugin with LockedImpl {
  val pipeline = create early new Pipeline{
    val stagesCount = 2
    val stages = Array.fill(stagesCount)(newStage())

    import spinal.lib.pipeline.Connection._
    //Connect every stage together 
    for((m, s) <- (stages.dropRight(1), stages.tail).zipped){
      connect(m, s)(M2S()) 
    }
  }

  val logic = create late new Area{
    lock.await() //Allow other plugins to make this blocking until they specified everything they wanted in the pipeline stages.
    pipeline.build()
  }
}
```

### Service Pipeline usage

For instance, the PcPlugin will want to introduce the PC value into the fetch pipeline : 

```scala
object PcPlugin extends AreaObject{
  val FETCH_PC = Stageable(UInt(32 bits))  //Define the concept of a FETCH_PC signal being usable through a pipeline
}

class PcPlugin() extends Plugin with ...{

  val setup = create early new Area{
    getService[FetchPlugin].retain() //We need to hold the FetchPlugin logic task until we create all the associated accesses
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val firstStage = fetch.pipeline.stages(0)

    firstStage(PcPlugin.FETCH_PC) := ???   //Assign the FETCH_PC value in firstStage of the pipeline. Other plugins may access it down stream. 
    fetch.release()
  }
}
```

# Execution units

You can spawn a execution unit by creating a new ExecutionUnitBase with a unique execution unit identifier : 

```scala 
    plugins += new ExecutionUnitBase("EU0")
```

Then you can populate that execution unit by adding new ExecutionUnitElementSimple with the same identifier : 


```scala
    plugins += new SrcPlugin("EU0")
    plugins += new IntAluPlugin("EU0")
    plugins += new ShiftPlugin("EU0")
```

Here is the example of a execution unit handeling : 

- mul/div
- jump/branches
- load/store
- CSR accesses
- ebreak/ecall/mret/wfi

```scala
    plugins += new ExecutionUnitBase("EU1", writebackCountMax = 1)
    plugins += new SrcPlugin("EU1")
    plugins += new MulPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new DivPlugin("EU1", writebackAt = 2)
    plugins += new BranchPlugin("EU1", writebackAt = 2, staticLatency = false)
    plugins += new LoadPlugin("EU1")
    plugins += new StorePlugin("EU1")
    plugins += new CsrAccessPlugin("EU1")(
      decodeAt = 0,
      readAt = 1,
      writeAt = 2,
      writebackAt = 2,
      staticLatency = false
    )
    plugins += new EnvCallPlugin("EU1")(rescheduleAt = 2)
```

### ShiftPlugin

Here is the ShiftPlugin as a example of ExecutionUnitElementSimple plugin: 

```scala
object ShiftPlugin extends AreaObject {
  val SIGNED = Stageable(Bool())
  val LEFT = Stageable(Bool())
}

class ShiftPlugin(euId : String, staticLatency : Boolean = true, aluStage : Int = 0) extends ExecutionUnitElementSimple(euId, staticLatency) {
  import ShiftPlugin._

  override def euWritebackAt = aluStage

  override val setup = create early new Setup{
    import SrcKeys._

    add(Rvi.SLL , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> True,  SIGNED -> False))
    add(Rvi.SRL , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRA , List(SRC1.RF, SRC2.RF), DecodeList(LEFT -> False, SIGNED -> True))
    add(Rvi.SLLI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> True , SIGNED -> False))
    add(Rvi.SRLI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> False))
    add(Rvi.SRAI, List(SRC1.RF, SRC2.I ), DecodeList(LEFT -> False, SIGNED -> True))
  }

  override val logic = create late new Logic{
    val process = new ExecuteArea(aluStage) {
      import stage._
      val ss = SrcStageables

      assert(Global.XLEN.get == 32)
      val amplitude  = ss.SRC2(4 downto 0).asUInt
      val reversed   = Mux[SInt](LEFT, ss.SRC1.reversed, ss.SRC1)
      val shifted = (S((SIGNED & ss.SRC1.msb) ## reversed) >> amplitude).resize(Global.XLEN bits)
      val patched = LEFT ? shifted.reversed | shifted

      wb.payload := B(patched)
    }
  }
}
```
