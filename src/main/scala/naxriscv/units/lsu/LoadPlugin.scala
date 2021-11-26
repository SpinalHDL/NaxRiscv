package naxriscv.units.lsu

import naxriscv.Frontend
import naxriscv.interfaces.{MicroOp, RD, RfResource}
import naxriscv.riscv.{Const, Rvi}
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
    val lsu = getService[LsuQueuePlugin]
    eu.retain()

    val port = lsu.newLoadPort()
    eu.addRobStageable(lsu.keys.LSU_ID)

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
    val lsu = getService[LsuQueuePlugin]
    val stage = eu.getExecute(0)
    import stage._

    val func3 = Frontend.MICRO_OP(Const.funct3Range)
    setup.port.valid := isFireing && SEL
    setup.port.address := U(SrcStageables.ADD_SUB)
    setup.port.id := lsu.keys.LQ_ID.resized
    setup.port.size := U(func3(1 downto 0))
    setup.port.unsigned := func3(2)
    out(setup.port)

    eu.release()
  }
}
