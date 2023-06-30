// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute.fpu

import naxriscv.Global.{RVD, RVF, XLEN}
import naxriscv.execute.ExecutionUnitBase
import naxriscv.interfaces._
import naxriscv.riscv.{Const, FloatRegFile, IntRegFile, Rvfd}
import naxriscv.utilities.{DocPlugin, Plugin}
import naxriscv.{DecodeList, DecodeListType, Frontend, ROB}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

object FpuIntegerExecute extends AreaObject {
  val SEL = Stageable(Bool())
  val OPCODE = Stageable(FpuOpcode())
  val ARG    = Stageable(Bits(2 bits))
}

class FpuIntegerExecute(euId : String) extends Plugin{
  import FpuFloatExecute._

  val setup = create early new Area {
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    eu.retain()

    eu.setDecodingDefault(SEL, False)

    def add(microOp: MicroOp, decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding :+ (SEL -> True))
    }
    def arg(v : Int) = ARG -> B(v, 2 bits)
    def op(v : FpuOpcode.E) = OPCODE -> v
    val f64 = FORMAT -> FpuFormat.DOUBLE
    val f32 = FORMAT -> FpuFormat.FLOAT

    import FpuOpcode._

    add(Rvfd.FMV_W_X  , DecodeList(op(FMV_W_X), f32))

    add(Rvfd.FCVT_S_WU, DecodeList(op(I2F)     , f32, arg(0)))
    add(Rvfd.FCVT_S_W , DecodeList(op(I2F)     , f32, arg(1)))

    if(XLEN.get == 64){
      add(Rvfd.FCVT_S_LU, DecodeList(op(I2F)    , f32, arg(2)))
      add(Rvfd.FCVT_S_L , DecodeList(op(I2F)    , f32, arg(3)))
    }

    if(RVD){
      add(Rvfd.FCVT_D_WU, DecodeList(op(I2F)     , f64, arg(0)))
      add(Rvfd.FCVT_D_W , DecodeList(op(I2F)     , f64, arg(1)))

      if(XLEN.get == 64){
        add(Rvfd.FMV_D_X  , DecodeList(op(FMV_W_X), f64))

        add(Rvfd.FCVT_D_LU, DecodeList(op(I2F)    , f64, arg(2)))
        add(Rvfd.FCVT_D_L , DecodeList(op(I2F)    , f64, arg(3)))
      }
    }

    val intCmd = master(Stream(FpuIntCmd(XLEN.get == 64, ROB.ID_WIDTH)))
  }

  val logic = create late new Area{
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._

    val forked = RegInit(False) setWhen(setup.intCmd.fire) clearWhen(!isStuck)
    haltIt(setup.intCmd.isStall)
    setup.intCmd.valid := isValid && FpuFloatExecute.SEL && !forked
    setup.intCmd.opcode    := OPCODE
    setup.intCmd.arg       := ARG
    setup.intCmd.rs1       := eu.apply(IntRegFile, RS1)
    setup.intCmd.format    := (if(RVD) stage(FORMAT) else FpuFormat.FLOAT())

    val instrRounding = Frontend.MICRO_OP(Const.funct3Range)
    val roundMode = (instrRounding === B"111") ? getService[FpuWriteback].getRoundingMode() | instrRounding
    setup.intCmd.roundMode.assignFromBits(roundMode)
    setup.intCmd.robId     := ROB.ID

    eu.release()
  }
}



//TODO FPU list
/*
- There maybe a few duplicate rob read during onCommit / onFree
- Implement LSU load nan-boxing
- Do not track renaming of RS3 for the integer regfile
- FpuWriteback better wakeups (anticipate timings, shave cycles, register file bypass)
- Test pipeline flush integration !!!
- do not report quiet nan ? (merge.NV := !RS.quiet)
- Implement nan boxing in the rounding logic
- fmv.s.x
 */



/*

obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/play_fpu/build/rv64imafd/play_fpu.elf --pass-symbol pass --fail-symbol fail --seed 72255458 --trace --trace-ref

testfloat_gen -n 400000 f32 | xxd -r -p > f32.bin

kinds=(f ui i)
for kind in ${kinds[@]}; do
  testfloat_gen -n 400000 ${kind}32  | xxd -r -p | xxd -g 4 -e | xxd -r > ${kind}32.bin
  testfloat_gen -n 400000 ${kind}64  | xxd -r -p | xxd -g 8 -e | xxd -r > ${kind}64.bin
done


 */