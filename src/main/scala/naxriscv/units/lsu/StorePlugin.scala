package naxriscv.units.lsu

import naxriscv.interfaces.MicroOp
import naxriscv.riscv.Rvi
import naxriscv.units.{ExecutionUnitBase, SrcKeys, SrcPlugin, SrcStageables}
import naxriscv.utilities._
import spinal.core._


class StorePlugin(euId : String) extends Plugin{
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
    val srcOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
    add(Rvi.SB , srcOps, Nil)
    add(Rvi.SH , srcOps, Nil)
    add(Rvi.SW , srcOps, Nil)
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
