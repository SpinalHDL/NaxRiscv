# NaxRiscv

An RISC-V core currently characterised by : 

- Out of order execution
- Superscalar (ex : 2 decode and 2 issue)
- Under design, but currently working RV32IMA (can run freertos in simulation)
- Portable HDL, But target FPGA with distributed ram (Xilinx series 7 is the reference used so far)
- Target a (relatively) low area usage and high fmax (not the best IPC)
- Decentralized hardware elaboration (No toplevel, composed of plugins)
- Frontend implemented around a pipelining framework to ease customisation

# Running simulation

See src/test/cpp/naxriscv/README.md

# Framework

The toplevel of NaxRiscv only contains a framework which can schedule a list plugins. The framework itself does not create any hardware.

```scala
val framework = new Framework(plugins())
```

To give an overview of how much the design is splited between plugins, here is the list of them for one functional CPU :

```
    val plugins = ArrayBuffer[Plugin]()
    plugins += new DocPlugin()
    plugins += new StaticAddressTranslationPlugin(
      ioRange = _(31 downto 28) === 0x1
    )

    //FETCH
    plugins += new FetchPlugin()
    plugins += new FetchAddressTranslationPlugin()
    plugins += new PcPlugin()
    plugins += new FetchCachePlugin(
      cacheSize = 4096*4,
      wayCount = 4,
      injectionAt = 2,
      memDataWidth = Fetch.FETCH_DATA_WIDTH,
      reducedBankWidth = false
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
    plugins += new DecoderPredictionPlugin(
      flushOnBranch = false //TODO remove me (DEBUG)
    )
    plugins += new BtbPlugin(
      entries = 512,
      readAt = 0,
      hitAt = 1,
      jumpAt = 2
    )
    plugins += new GSharePlugin(
      memBytes = 4 KiB,
      historyWidth = 24,  //24 => 31979 / 32601 / 35356 / 1w => 49389 2w => 53474
      readAt = 0
    )

    //LOAD / STORE
    plugins += new LsuPlugin(
      lqSize = 16,
      sqSize = 16,
      loadToCacheBypass = true,
      lqToCachePipelined = true,
      hazardPedictionEntries = 512*8,
      hazardPredictionTagWidth = 16,
      loadTranslationParameter  = StaticAddressTranslationParameter(rspAt = 1),
      storeTranslationParameter = StaticAddressTranslationParameter(rspAt = 1),
      loadFeedAt = 0 //TODO
    )
    plugins += new DataCachePlugin(
      memDataWidth = Global.XLEN,
      cacheSize    = 4096*4,
      wayCount     = 4,
      refillCount = 2,
      writebackCount = 2,
      reducedBankWidth = false,
      loadRspAt = 2 //TODO optimise timings to reduce to 2 again
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

    //EXECUTION UNITES
    plugins += new ExecutionUnitBase("EU0")
    plugins += new SrcPlugin("EU0")
    plugins += new IntAluPlugin("EU0")
    plugins += new ShiftPlugin("EU0")

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
