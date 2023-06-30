// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.sandbox.doc
import naxriscv.Global
import naxriscv.execute.{ExecutionUnitBase, ExecutionUnitElementSimple, SimdAddPlugin}
import naxriscv.interfaces.{CommitService, INSTRUCTION_SIZE, PC_READ, RS1, RS2, SingleDecoding}
import naxriscv.riscv.IntRegFile
import naxriscv.riscv.IntRegFile.TypeB
import naxriscv.utilities.Plugin
import spinal.core._

////
////
////import naxriscv._
////import naxriscv.Global._
////import naxriscv.execute._
////import naxriscv.fetch._
////import naxriscv.frontend._
////import naxriscv.interfaces.JumpService
////import naxriscv.utilities.{DataBase, Framework, NaxScope, Plugin, Service}
////import spinal.core._
////import spinal.core.fiber.{Lock, hardFork, soon}
////import spinal.lib._
////import spinal.lib.pipeline.Pipeline
////
////import scala.collection.mutable.ArrayBuffer
////
////object Pres {
////  SpinalVerilog {
////    val plugins = ArrayBuffer[Plugin]()
////    // .. Add some plugins
////    new NaxRiscv(plugins)
////  }
////
////  class NaxRiscv(val plugins : Seq[Plugin]) extends Component{
////    val database = new DataBase
////    val framework = NaxScope(database) on new Framework(plugins)
////  }
////
////  plugins ++= List(
////    new PcPlugin(),
////    new FetchCachePlugin(16 KB),
////    new DecoderPlugin(),
////    new DispatchPlugin(slotCount = 32),
////    new CommitPlugin()
////    ...
////  )
////
////  plugins ++= List(
////    new ExecutionUnitBase("ALU0"),
////    new IntFormatPlugin("ALU0"),
////    new SrcPlugin("ALU0"),
////    new IntAluPlugin("ALU0"),
////    new ShiftPlugin("ALU0" ),
////    new BranchPlugin("ALU0")
////  )
////
////
////
////}
////
////
////object Pres2{
////  class PcPlugin() extends Plugin with JumpService{
////    override def newJumpPort(priority: Int) = { ... }
////
////    val logic = create late new Area {
////      // Create the hardware
////      val pc = Reg(UInt(32 bits))
////        ...
////    }
////  }
////
////  trait JumpService extends Service{
////    def newJumpPort(priority : Int) : Flow[UInt]
////  }
////
////  class CommitPlugin() extends Plugin {
////    val setup = create early new Area {
////      // Ask other plugins things
////      val jump = getService[JumpService].newJumpPort()
////    }
////    val logic = create late new Area {
////      // Create the hardware
////      setup.jump.valid := ???
////      setup.jump.payload := ???
////      ...
////    }
////  }
////}
////
////object Pres3{
////  class FetchPlugin() extends Plugin {
////    val pipeline = create early new Pipeline {
////      val stages = Array.fill(4)(newStage())
////    }
////
////    val lock = Lock()
////    val builder = create late new Area{
////      lock.await()
////      pipeline.build()
////    }
////  }
////
////  object PC extends Stageable(UInt(32 bits))
////  class PcPlugin extends Plugin ... {
////    ...
////    val logic = create late new Area{
////      ...
////      val pcReg = Reg(UInt(32 bits))
////      getService[FetchPlugin].pipeline.stages(0)(PC) := pcReg
////    }
////  }
////
////  class FetchCachePlugin extends Plugin ... {
////    ...
////    val logic = create late new Area{
////      ...
////      getService[FetchPlugin].pipeline.stages(1)(PC) === wayTag
////    }
////  }
////}
//
//object Pres4{
//  val ADD4 = IntRegFile.TypeR(M"0000000----------000-----0001011")
//
//  class SimdAddPlugin(euId : String) extends ExecutionUnitElementSimple(euId, staticLatency = true) {
//    override def euWritebackAt = 0
//    override val setup = create early new Setup{
//      add(SimdAddPlugin.ADD4)
//    }
//
//    override val logic = create late new Logic{
//      val process = new ExecuteArea(stageId = 0) {
//        val rs1 = stage(eu(IntRegFile, RS1)).asUInt
//        val rs2 = stage(eu(IntRegFile, RS2)).asUInt
//
//        val rd = UInt(32 bits)
//        rd( 7 downto  0) := rs1( 7 downto  0) + rs2( 7 downto  0)
//        rd(16 downto  8) := rs1(16 downto  8) + rs2(16 downto  8)
//        rd(23 downto 16) := rs1(23 downto 16) + rs2(23 downto 16)
//        rd(31 downto 24) := rs1(31 downto 24) + rs2(31 downto 24)
//
//        wb.payload := rd.asBits
//      }
//    }
//  }
//}

//object Pres5{
//  import Global.PC
//
//  val BEQ  =  SingleDecoding(
//    key = M"-----------------000-----1100011",
//    resources = List(
//      IntRegFile -> RS1,
//      IntRegFile -> RS2,
//      PC_READ,
//      INSTRUCTION_SIZE
//    )
//  )
//
//  class BranchPlugin(euId : String) extends Plugin{
//    val setup = create early new Area{
//      val eu = findService[ExecutionUnitBase](_.euId == euId)
//      eu.add(BEQ)
//
//      val commit   = getService[CommitService]
//      val schedule = commit.newSchedulePort(canJump = true, canTrap = true)
//    }
//    //...
//    val logic = create late new Area{
//      val stage = setup.eu.getExecute(stageId = 0)
//      val target = stage(PC) + ???
//      // ...
//      setup.reschedule.valid := isFireing && SEL && (MISSPREDICTED || MISSALIGNED)
//      setup.reschedule.trap  := MISSALIGNED
//      setup.reschedule.pcTarget := target
//    }
//  }
//
//
//}
