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

object FpuFloatExecute extends AreaObject {
  val SEL = Stageable(Bool())
  val OPCODE = Stageable(FpuOpcode())
  val FORMAT = Stageable(FpuFormat())
  val ARG    = Stageable(Bits(2 bits))
}

class FpuFloatExecute(euId : String) extends Plugin{
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

    add(Rvfd.FMV_X_W  , DecodeList(op(FMV_X_W) , f32))

    add(Rvfd.FADD_S   , DecodeList(op(ADD)     , f32, arg(0)))
    add(Rvfd.FSUB_S   , DecodeList(op(ADD)     , f32, arg(1)))
    add(Rvfd.FMUL_S   , DecodeList(op(MUL)     , f32))
    add(Rvfd.FDIV_S   , DecodeList(op(DIV)     , f32))
    add(Rvfd.FSQRT_S  , DecodeList(op(SQRT)    , f32))

    add(Rvfd.FMADD_S  , DecodeList(op(FMA)     , f32, arg(0)))
    add(Rvfd.FMSUB_S  , DecodeList(op(FMA)     , f32, arg(1)))
    add(Rvfd.FNMADD_S , DecodeList(op(FMA)     , f32, arg(3)))
    add(Rvfd.FNMSUB_S , DecodeList(op(FMA)     , f32, arg(2)))

    add(Rvfd.FSGNJ_S  , DecodeList(op(SGNJ)    , f32, arg(0)))
    add(Rvfd.FSGNJN_S , DecodeList(op(SGNJ)    , f32, arg(1)))
    add(Rvfd.FSGNJX_S , DecodeList(op(SGNJ)    , f32, arg(2)))

    add(Rvfd.FMIN_S   , DecodeList(op(MIN_MAX) , f32, arg(0)))
    add(Rvfd.FMAX_S   , DecodeList(op(MIN_MAX) , f32, arg(1)))

    add(Rvfd.FLE_S    , DecodeList(op(CMP)     , f32, arg(0)))
    add(Rvfd.FEQ_S    , DecodeList(op(CMP)     , f32, arg(2)))
    add(Rvfd.FLT_S    , DecodeList(op(CMP)     , f32, arg(1)))

    add(Rvfd.FCLASS_S , DecodeList(op(FCLASS)  , f32))

    add(Rvfd.FCVT_WU_S, DecodeList(op(F2I)     , f32, arg(0)))
    add(Rvfd.FCVT_W_S , DecodeList(op(F2I)     , f32, arg(1)))

    if(XLEN.get == 64){
      add(Rvfd.FCVT_LU_S, DecodeList(op(F2I)    , f32, arg(2)))
      add(Rvfd.FCVT_L_S , DecodeList(op(F2I)    , f32, arg(3)))
    }
    
    if(RVD){
      add(Rvfd.FADD_D   , DecodeList(op(ADD)     , f64, arg(0)))
      add(Rvfd.FSUB_D   , DecodeList(op(ADD)     , f64, arg(1)))
      add(Rvfd.FMUL_D   , DecodeList(op(MUL)     , f64))
      add(Rvfd.FDIV_D   , DecodeList(op(DIV)     , f64))
      add(Rvfd.FSQRT_D  , DecodeList(op(SQRT)    , f64))

      add(Rvfd.FMADD_D  , DecodeList(op(FMA)     , f64, arg(0)))
      add(Rvfd.FMSUB_D  , DecodeList(op(FMA)     , f64, arg(1)))
      add(Rvfd.FNMADD_D , DecodeList(op(FMA)     , f64, arg(3)))
      add(Rvfd.FNMSUB_D , DecodeList(op(FMA)     , f64, arg(2)))

      add(Rvfd.FSGNJ_D  , DecodeList(op(SGNJ)    , f64, arg(0)))
      add(Rvfd.FSGNJN_D , DecodeList(op(SGNJ)    , f64, arg(1)))
      add(Rvfd.FSGNJX_D , DecodeList(op(SGNJ)    , f64, arg(2)))

      add(Rvfd.FMIN_D   , DecodeList(op(MIN_MAX) , f64, arg(0)))
      add(Rvfd.FMAX_D   , DecodeList(op(MIN_MAX) , f64, arg(1)))

      add(Rvfd.FLE_D    , DecodeList(op(CMP)     , f64, arg(0)))
      add(Rvfd.FEQ_D    , DecodeList(op(CMP)     , f64, arg(2)))
      add(Rvfd.FLT_D    , DecodeList(op(CMP)     , f64, arg(1)))

      add(Rvfd.FCLASS_D , DecodeList(op(FCLASS)  , f64))

      add(Rvfd.FCVT_WU_D, DecodeList(op(F2I)     , f64, arg(0)))
      add(Rvfd.FCVT_W_D , DecodeList(op(F2I)     , f64, arg(1)))
      add(Rvfd.FCVT_D_S , DecodeList(op(FCVT_X_X), f32))
      add(Rvfd.FCVT_S_D , DecodeList(op(FCVT_X_X), f64))

      if(XLEN.get == 64){
        add(Rvfd.FMV_X_D  , DecodeList(op(FMV_X_W), f64))

        add(Rvfd.FCVT_LU_D, DecodeList(op(F2I)    , f64, arg(2)))
        add(Rvfd.FCVT_L_D , DecodeList(op(F2I)    , f64, arg(3)))
      }
    }

    val floatCmd = master(Stream(FpuFloatCmd(RVD, ROB.ID_WIDTH)))
  }

  val logic = create late new Area{
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._

    val forked = RegInit(False) setWhen(setup.floatCmd.fire) clearWhen(!isStuck)
    haltIt(setup.floatCmd.isStall)
    setup.floatCmd.valid := isValid && FpuFloatExecute.SEL && !forked
    setup.floatCmd.opcode    := OPCODE
    setup.floatCmd.arg       := ARG
    setup.floatCmd.rs(0)     := eu.apply(FloatRegFile, RS1)
    setup.floatCmd.rs(1)     := eu.apply(FloatRegFile, RS2)
    setup.floatCmd.rs(2)     := eu.apply(FloatRegFile, RS3)
    setup.floatCmd.format    := (if(RVD) stage(FORMAT) else FpuFormat.FLOAT())

    val instrRounding = Frontend.MICRO_OP(Const.funct3Range)
    val roundMode = (instrRounding === B"111") ? getService[FpuWriteback].getRoundingMode() | instrRounding
    setup.floatCmd.roundMode.assignFromBits(roundMode)
    setup.floatCmd.robId     := ROB.ID

    eu.release()
  }
}



//TODO FPU list
/*
- There maybe a few duplicate rob read during onCommit / onFree
- Do not track renaming of RS3 for the integer regfile
- FpuWriteback better wakeups (anticipate timings, shave cycles, register file bypass)
- Test pipeline flush integration !!!
- do not report quiet nan ? (merge.NV := !RS.quiet)
- spike does trap if round mode is wrong
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