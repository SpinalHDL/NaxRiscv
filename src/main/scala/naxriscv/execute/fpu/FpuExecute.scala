package naxriscv.execute.fpu

import naxriscv.Global.{RVD, RVF, XLEN}
import naxriscv.{DecodeListType, Frontend, ROB}
import naxriscv.execute.{ExecutionUnitBase, SrcKeys, SrcPlugin, SrcStageables}
import naxriscv.interfaces.{DecoderService, MicroOp}
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, Rvfd, Rvi}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

object FpuExecute{
  val SEL = Stageable(Bool())
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

    add(Rvfd.FMUL_D, Nil)
  }

  val logic = create late new Area{
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._


    eu.release()
  }
}



//TODO FPU list
/*
- There maybe a few duplicate rob read during onCommit / onFree
 */



/*

obj_dir/VNaxRiscv --load-elf ../../../../ext/NaxSoftware/baremetal/play_fpu/build/rv64imafd/play_fpu.elf --pass-symbol pass --fail-symbol fail --seed 72255458 --trace --trace-ref


 */