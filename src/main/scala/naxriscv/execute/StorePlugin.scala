package naxriscv.execute

import naxriscv.Frontend
import naxriscv.interfaces.{MicroOp, RS2}
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, IntRegFile, Rvi}
import naxriscv.utilities._
import spinal.core._
import spinal.lib.pipeline.Stageable

object StorePlugin extends AreaObject{
  val SEL = Stageable(Bool())
}

class StorePlugin(euId : String) extends Plugin{
  import StorePlugin._
  val setup = create early new Area {
    val eu = getService[ExecutionUnitBase](euId)
    val lsu = getService[LsuPlugin]
    eu.retain()

    val port = lsu.newStorePort()
    eu.addRobStageable(lsu.keys.LSU_ID)
    eu.setDecodingDefault(SEL, False)

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
    val lsu = getService[LsuPlugin]
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._
    val func3 = Frontend.MICRO_OP(Const.funct3Range)
    setup.port.valid := isFireing && SEL
    setup.port.address := U(SrcStageables.ADD_SUB)
    setup.port.data    := eu(IntRegFile, RS2)
    setup.port.sqId := lsu.keys.LSU_ID.resized
    setup.port.robId := ExecutionUnitKeys.ROB_ID
    setup.port.size := U(func3(1 downto 0))
    eu.release()
  }
}
