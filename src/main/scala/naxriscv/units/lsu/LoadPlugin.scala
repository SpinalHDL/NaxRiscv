package naxriscv.units.lsu

import naxriscv.interfaces.{MicroOp, RD, RfResource}
import naxriscv.riscv.Rvi
import spinal.core._
import spinal.lib._
import naxriscv.units.{ExecutionUnitBase, SrcKeys, SrcPlugin, SrcStageables}
import naxriscv.utilities._
import spinal.lib.pipeline.Stageable

object LoadPlugin extends AreaObject{
  val SEL = Stageable(Bool())
  val UNSIGNED = Stageable(Bool())
}

class LoadPlugin(euId : String) extends Plugin{
  import LoadPlugin._
  val setup = create early new Area {
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    def add(microOp: MicroOp, srcKeys: List[SrcKeys], decoding: eu.DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding :+ (SEL -> True))
      if (srcKeys.nonEmpty) {
        getService[SrcPlugin](euId).specify(microOp, srcKeys)
      }
    }

    val sk = SrcKeys
    val srcOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I)
    add(Rvi.LB , srcOps, Nil)
    add(Rvi.LH , srcOps, Nil)
    add(Rvi.LW , srcOps, Nil)
    add(Rvi.LBU, srcOps, Nil)
    add(Rvi.LHU, srcOps, Nil)

  }

  val logic = create late new Area{
    val eu = setup.eu
    val addressCalc = new Area {
      val stage = eu.getExecute(0)
      SrcStageables.ADD_SUB
    }
    eu.release()
  }
}
