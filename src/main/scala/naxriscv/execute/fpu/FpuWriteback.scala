package naxriscv.execute.fpu

import naxriscv.{DecodeList, Frontend, Global, ROB}
import naxriscv.interfaces.{CommitService, CsrService, DecoderService, MicroOp, PrivilegedService, RegfileService, RobService, ScheduleReason, WakeRegFile, WakeRegFileService, WakeRob, WakeRobService}
import naxriscv.misc.RegFilePlugin
import naxriscv.riscv.{CSR, FloatRegFile, IntRegFile, Rvfd}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
import naxriscv.Global._
import naxriscv.frontend.FrontendPlugin

import scala.collection.mutable.ArrayBuffer

object FpuWriteback extends AreaObject {
  val INT_FLAGS = Stageable(new FpuFlags)
  val FLOAT_FLAGS = Stageable(new FpuFlags)

  val INT_FLAGS_ENABLE = Stageable(Bool())
  val FLOAT_FLAGS_ENABLE = Stageable(Bool())
  val FP_DIRTY = Stageable(Bool())
}


class FpuWriteback extends Plugin  with WakeRobService with WakeRegFileService{
  import FpuWriteback._

  override def wakeRobs    = List(logic.get.float.wakeRob, logic.get.integer.wakeRob)
  override def wakeRegFile = List(logic.get.float.wakeRf,  logic.get.integer.wakeRf)

  def getRoundingMode() : Bits = logic.rm

  val setup = create early new Area{
    val floatCompletion = Flow(FpuFloatCompletion(ROB.ID_WIDTH, 32 + Global.RVD.get.toInt*32))
    val integerWriteback = Stream(FpuIntWriteBack(ROB.ID_WIDTH, 32 + Global.RVD.get.toInt*32))
    val rob = getService[RobService]
    val decoder = getService[DecoderService]
    val commit = getService[CommitService]
    val rfFloat = findService[RegfileService](_.rfSpec == FloatRegFile)
    val rfInteger = findService[RegfileService](_.rfSpec == IntRegFile)
    val floatWrite   = rfFloat.newWrite(false, 1)
    val integerWrite = rfInteger.newWrite(false, 1)
    val robFloatcompletion = rob.newRobCompletion()
    val robIntegercompletion = rob.newRobCompletion()
    val csr = getService[CsrService]

    getService[DocPlugin].property("FPU_ROB_TO_FLAG_COUNT", COMMIT_COUNT.get)
    val unschedule = out Bool()


    val floatList, intList, lsuList = ArrayBuffer[MicroOp]()
    floatList ++= List(
      Rvfd.FMV_W_X  ,Rvfd.FADD_S   ,Rvfd.FSUB_S   ,Rvfd.FMUL_S   ,Rvfd.FDIV_S   ,Rvfd.FSQRT_S  ,Rvfd.FMADD_S  ,
      Rvfd.FMSUB_S  ,Rvfd.FNMADD_S ,Rvfd.FNMSUB_S ,Rvfd.FSGNJ_S  ,Rvfd.FSGNJN_S ,Rvfd.FSGNJX_S ,Rvfd.FMIN_S   ,
      Rvfd.FMAX_S   ,Rvfd.FCVT_S_WU, Rvfd.FCVT_S_W
    )
    intList ++= List(
      Rvfd.FMV_X_W, Rvfd.FCVT_WU_S, Rvfd.FCVT_W_S,Rvfd.FCLASS_S, Rvfd.FLE_S    ,Rvfd.FEQ_S    ,Rvfd.FLT_S
    )
    lsuList ++= List(Rvfd.FLW, Rvfd.FSW)

    if(XLEN.get == 64){
      floatList ++= List(
        Rvfd.FCVT_S_LU, Rvfd.FCVT_S_L
      )
      intList ++= List(
        Rvfd.FCVT_LU_S, Rvfd.FCVT_L_S
      )
      lsuList ++= List(Rvfd.FLD, Rvfd.FSD)
    }

    if(RVD){
      floatList ++= List(
        Rvfd.FADD_D   , Rvfd.FSUB_D   , Rvfd.FMUL_D   , Rvfd.FDIV_D   , Rvfd.FSQRT_D  , Rvfd.FMADD_D  , Rvfd.FMSUB_D  ,
        Rvfd.FNMADD_D , Rvfd.FNMSUB_D , Rvfd.FSGNJ_D  , Rvfd.FSGNJN_D , Rvfd.FSGNJX_D , Rvfd.FMIN_D   , Rvfd.FMAX_D   ,
        Rvfd.FCVT_D_WU, Rvfd.FCVT_D_W , Rvfd.FCVT_D_S , Rvfd.FCVT_S_D
      )
      intList ++= List(
        Rvfd.FCVT_WU_D, Rvfd.FCVT_W_D, Rvfd.FCLASS_D, Rvfd.FLE_D    , Rvfd.FEQ_D    , Rvfd.FLT_D
      )
      if(XLEN.get == 64){
        floatList ++= List(
          Rvfd.FMV_D_X, Rvfd.FCVT_D_LU, Rvfd.FCVT_D_L
        )
        intList ++= List(
          Rvfd.FMV_X_D, Rvfd.FCVT_LU_D, Rvfd.FCVT_L_D
        )
      }
    }


    for(s <- List(INT_FLAGS_ENABLE, FLOAT_FLAGS_ENABLE, FP_DIRTY)){
      decoder.addDecodingToRob(s)
      decoder.addMicroOpDecodingDefault(s, False)
    }
    for(e <- floatList) decoder.addMicroOpDecoding(e, DecodeList(FLOAT_FLAGS_ENABLE -> True))
    for(e <- intList) decoder.addMicroOpDecoding(e, DecodeList(INT_FLAGS_ENABLE -> True))
    (floatList++intList++lsuList--List(Rvfd.FMV_X_D, Rvfd.FMV_X_W, Rvfd.FSW, Rvfd.FSD)).foreach{ e =>
      decoder.addMicroOpDecoding(e , DecodeList(FP_DIRTY -> True))
    }

    rob.retain()
    csr.retain()
  }

  val logic = create late new Area {
    val s = setup.get

    import s._


    val flags = Reg(FpuFlags())
    flags.NV init (False)
    flags.DZ init (False)
    flags.OF init (False)
    flags.UF init (False)
    flags.NX init (False)

    val rm = Reg(Bits(3 bits)) init (0)
    csr.readWrite(CSR.FCSR, 5 -> rm)
    csr.readWrite(CSR.FCSR, 0 -> flags)
    csr.readWrite(CSR.FRM, 0 -> rm)
    csr.readWrite(CSR.FFLAGS, 0 -> flags)

    //Flush the pipeline if the rounding mode changed
    csr.onWrite(CSR.FRM, false){
      when(csr.onWriteBits(0, 3 bits) =/= rm) {
        csr.onWriteFlushPipeline()
      }
    }
    csr.onWrite(CSR.FCSR, false){
      when(csr.onWriteBits(5, 3 bits) =/= rm) {
        csr.onWriteFlushPipeline()
      }
    }

    setup.unschedule := RegNext(getService[CommitService].reschedulingPort(true).valid) init (False)


    val float = new Area {
      val physical = rob.readAsyncSingle(decoder.PHYS_RD, floatCompletion.robId)

      val wakeRob = Flow(WakeRob())
      wakeRob.valid := floatCompletion.fire
      wakeRob.robId := floatCompletion.robId

      val wakeRf = Flow(WakeRegFile(decoder.REGFILE_RD, decoder.PHYS_RD, needBypass = false))
      wakeRf.valid := floatCompletion.fire
      wakeRf.physical := physical
      wakeRf.regfile := decoder.REGFILE_RD.rfToId(FloatRegFile)

      floatWrite.valid := floatCompletion.fire
      floatWrite.robId := floatCompletion.robId
      floatWrite.data := floatCompletion.value
      floatWrite.address := physical

      rob.writeSingle(
        key = FLOAT_FLAGS,
        value = floatCompletion.flags,
        robId = floatCompletion.robId,
        enable = floatCompletion.valid
      )

      robFloatcompletion.valid := floatCompletion.fire
      robFloatcompletion.id := floatCompletion.robId
    }

    val integer = new Area {
      val physical = rob.readAsyncSingle(decoder.PHYS_RD, integerWriteback.robId)

      val wakeRob = Flow(WakeRob())
      wakeRob.valid := integerWriteback.fire
      wakeRob.robId := integerWriteback.robId

      val wakeRf = Flow(WakeRegFile(decoder.REGFILE_RD, decoder.PHYS_RD, needBypass = false))
      wakeRf.valid := integerWriteback.fire
      wakeRf.physical := physical
      wakeRf.regfile := decoder.REGFILE_RD.rfToId(IntRegFile)

      integerWrite.valid := integerWriteback.fire
      integerWrite.robId := integerWriteback.robId
      integerWrite.data := integerWriteback.value
      integerWrite.address := physical

      rob.writeSingle(
        key = INT_FLAGS,
        value = integerWriteback.flags,
        robId = integerWriteback.robId,
        enable = integerWriteback.valid
      )

      robIntegercompletion.valid := integerWriteback.fire
      robIntegercompletion.id := integerWriteback.robId

      integerWriteback.ready := True
    }

    val onCommit = new Area {
      val event = commit.onCommit()
      val intFlags = rob.readAsync(INT_FLAGS, Global.COMMIT_COUNT, event.robId)
      val intEnable = rob.readAsync(INT_FLAGS_ENABLE, Global.COMMIT_COUNT, event.robId)
      val floatFlags = rob.readAsync(FLOAT_FLAGS, Global.COMMIT_COUNT, event.robId)
      val floatEnable = rob.readAsync(FLOAT_FLAGS_ENABLE, Global.COMMIT_COUNT, event.robId)
      val fpDirty = rob.readAsync(FP_DIRTY, Global.COMMIT_COUNT, event.robId)

      val masks = for (i <- 0 until COMMIT_COUNT) yield (intFlags(i).asBits.andMask(intEnable(i)) | floatFlags(i).asBits.andMask(floatEnable(i))).andMask(event.mask(i))
      val aggregated = masks.reduce(_ | _).as(FpuFlags())

      flags.NX setWhen (aggregated.NX)
      flags.UF setWhen (aggregated.UF)
      flags.OF setWhen (aggregated.OF)
      flags.DZ setWhen (aggregated.DZ)
      flags.NV setWhen (aggregated.NV)

      when((B(fpDirty) & event.mask).orR) {
        getService[PrivilegedService].setFpDirty()
      }
    }

    val whitebox = new AreaRoot{
      val fpuRobToFlags = List.tabulate(COMMIT_COUNT)(i => new Area {
        val robId = Verilator.public(onCommit.event.robId + i)
        val mask  = Verilator.public(CombInit(onCommit.masks(i)))
      })
    }

    rob.release()
    csr.release()
  }
}
