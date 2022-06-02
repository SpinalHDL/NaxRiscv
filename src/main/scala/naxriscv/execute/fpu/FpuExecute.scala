package naxriscv.execute.fpu

import naxriscv.Global.{RVD, RVF, XLEN}
import naxriscv.{DecodeList, DecodeListType, Frontend, ROB}
import naxriscv.execute.{ExecutionUnitBase, SrcKeys, SrcPlugin, SrcStageables}
import naxriscv.interfaces._
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, FloatRegFile, IntRegFile, Rvfd, Rvi}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

object FpuExecute extends AreaObject {
  val SEL = Stageable(Bool())
  val OPCODE = Stageable(FpuOpcode())
  val FORMAT = Stageable(FpuFormat())
  val ARG    = Stageable(Bits(2 bits))
}

class FpuExecute(euId : String) extends Plugin{
  import FpuExecute._

  create config{
    RVF.set(true)
    RVD.set(true)
    val doc = getService[DocPlugin]
    if(RVF) doc.property("RVF", true)
    if(RVD) doc.property("RVD", true)
  }

  val setup = create early new Area {
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    eu.retain()

    eu.setDecodingDefault(SEL, False)

    def add(microOp: MicroOp, decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding :+ (SEL -> True))
    }
    def arg(v : Int) = ARG -> B(v, 2 bits)
    add(Rvfd.FMUL_D,  DecodeList(OPCODE -> FpuOpcode.MUL, FORMAT -> FpuFormat.DOUBLE))
    add(Rvfd.FADD_D,  DecodeList(OPCODE -> FpuOpcode.ADD, FORMAT -> FpuFormat.DOUBLE, arg(0)))
    add(Rvfd.FSUB_D,  DecodeList(OPCODE -> FpuOpcode.ADD, FORMAT -> FpuFormat.DOUBLE, arg(1)))
    add(Rvfd.FMADD_D, DecodeList(OPCODE -> FpuOpcode.FMA, FORMAT -> FpuFormat.DOUBLE, arg(0)))

    val floatCmd = master(Stream(FpuFloatCmd(RVD, ROB.ID_WIDTH)))
  }

  val logic = create late new Area{
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._

    val forked = RegInit(False) setWhen(setup.floatCmd.fire) clearWhen(!isStuck)
    haltIt(setup.floatCmd.isStall)
    setup.floatCmd.valid := isValid && FpuExecute.SEL && !forked
    setup.floatCmd.opcode    := OPCODE
    setup.floatCmd.arg       := ARG
    setup.floatCmd.rs(0)     := eu.apply(FloatRegFile, RS1)
    setup.floatCmd.rs(1)     := eu.apply(FloatRegFile, RS2)
    setup.floatCmd.rs(2)     := eu.apply(FloatRegFile, RS3)
    setup.floatCmd.format    := (if(RVD) stage(FORMAT) else FpuFormat.FLOAT())
    setup.floatCmd.roundMode := FpuRoundMode.RDN()
    setup.floatCmd.robId     := ROB.ID

    eu.release()
  }
}



//TODO FPU list
/*
- There maybe a few duplicate rob read during onCommit / onFree
- Implement LSU load nan-boxing
- Do not track renaming of RS3 for the integer regfile
- FpuWriteback better wakeups (anticipate timings, shave cycles, register file bypass)
 */



/*

obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/play_fpu/build/rv64imafd/play_fpu.elf --pass-symbol pass --fail-symbol fail --seed 72255458 --trace --trace-ref


 */