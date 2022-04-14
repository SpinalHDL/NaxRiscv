package naxriscv.execute

import spinal.core._
import spinal.lib._
import naxriscv._
import naxriscv.riscv._
import naxriscv.riscv.IntRegFile
import naxriscv.interfaces.{RS1, RS2}
import naxriscv.utilities.Plugin

//This plugin example will add a new instruction named SIMD_ADD which do the following :
//
//RD : Regfile Destination, RS : Regfile Source
//RD( 7 downto  0) = RS1( 7 downto  0) + RS2( 7 downto  0)
//RD(16 downto  8) = RS1(16 downto  8) + RS2(16 downto  8)
//RD(23 downto 16) = RS1(23 downto 16) + RS2(23 downto 16)
//RD(31 downto 24) = RS1(31 downto 24) + RS2(31 downto 24)
//
//Instruction encoding :
//0000000----------000-----0001011   <- Custom0 func3=0 func7=0
//       |RS2||RS1|   |RD |
//
//Note :  RS1, RS2, RD positions follow the RISC-V spec and are common for all instruction of the ISA


object SimdAddPlugin{
  //Define the instruction type and encoding that we wll use
  val ADD4 = IntRegFile.TypeR(M"0000000----------000-----0001011")
}

//ExecutionUnitElementSimple Is a base class which will be coupled to the pipeline provided by a ExecutionUnitBase with
//the same euId. It provide quite a few utilities to ease the implementation of custom instruction.
//Here we will implement a plugin which provide SIMD add on the register file.
//staticLatency=true specify that our plugin will never halt the pipeling, allowing the issue queue to statically
//wake up instruction which depend on its result.
class SimdAddPlugin(val euId : String) extends ExecutionUnitElementSimple(euId, staticLatency = true) {
  //We will assume our plugin is fully combinatorial
  override def euWritebackAt = 0

  //The setup code is by plugins to specify things to each others before it is too late
  //create early blockOfCode will
  override val setup = create early new Setup{
    //Let's assume we only support RV32 for now
    assert(Global.XLEN.get == 32)

    //Specify to the ExecutionUnitBase that the current plugin will implement the ADD4 instruction
    add(SimdAddPlugin.ADD4)
  }

  override val logic = create late new Logic{
    val process = new ExecuteArea(stageId = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = stage(eu(IntRegFile, RS1)).asUInt
      val rs2 = stage(eu(IntRegFile, RS2)).asUInt

      //Do some computation
      val rd = UInt(32 bits)
      rd( 7 downto  0) := rs1( 7 downto  0) + rs2( 7 downto  0)
      rd(16 downto  8) := rs1(16 downto  8) + rs2(16 downto  8)
      rd(23 downto 16) := rs1(23 downto 16) + rs2(23 downto 16)
      rd(31 downto 24) := rs1(31 downto 24) + rs2(31 downto 24)

      //Provide the computation value for the writeback
      wb.payload := rd.asBits
    }
  }
}



object SimdAddNaxGen extends App{
  import naxriscv.compatibility._
  import naxriscv.utilities._

  def plugins = {
    val l = Config.plugins(
      withRdTime = false,
      aluCount    = 2,
      decodeCount = 2
    )
    l += new SimdAddPlugin("ALU0")
    l += new SimdAddPlugin("ALU1")
    l
  }

  val spinalConfig = SpinalConfig(inlineRom = true)
  spinalConfig.addTransformationPhase(new MemReadDuringWriteHazardPhase)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  val report = spinalConfig.generateVerilog(new NaxRiscv(plugins))
  report.toplevel.framework.getService[DocPlugin].genC()
}


/*

cd ext/NaxSoftware/baremetal/simdAdd
make clean rv32im

./obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/simdAdd/build/rv32im/simdAdd.elf --spike-disable --pass-symbol pass --fail-symbol fail --trace

 */