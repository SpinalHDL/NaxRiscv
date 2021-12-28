package naxriscv.execute

import naxriscv.{Frontend, ROB}
import naxriscv.interfaces.{AddressTranslationService, DecoderService, MicroOp}
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, Rvi}
import naxriscv.utilities._
import spinal.core._
import spinal.lib.pipeline.Stageable
import naxriscv.Global._

object LoadPlugin extends AreaObject{
  val SEL = Stageable(Bool())
  val UNSIGNED = Stageable(Bool())
}

class LoadPlugin(euId : String) extends Plugin{
  import LoadPlugin._
  val setup = create early new Area {
    val eu = getService[ExecutionUnitBase](euId)
    val lsu = getService[LsuPlugin]
    eu.retain()

    val port = lsu.newLoadPort()
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
    val srcOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I)
    add(Rvi.LB , srcOps, Nil)
    add(Rvi.LH , srcOps, Nil)
    add(Rvi.LW , srcOps, Nil)
    add(Rvi.LBU, srcOps, Nil)
    add(Rvi.LHU, srcOps, Nil)
  }

  val logic = create late new Area{
    val eu = setup.eu
    val lsu = getService[LsuPlugin]
    val decoder = getService[DecoderService]
    val stage = eu.getExecute(0)
    import stage._

    val func3 = Frontend.MICRO_OP(Const.funct3Range)
    setup.port.valid := isFireing && SEL
    setup.port.robId := ROB.ID
    setup.port.lqId := lsu.keys.LSU_ID.resized
    setup.port.address := U(SrcStageables.ADD_SUB)
    setup.port.size := U(func3(1 downto 0))
    setup.port.unsigned := func3(2)
    setup.port.physicalRd := decoder.PHYS_RD
    setup.port.writeRd := decoder.WRITE_RD
    setup.port.pc := PC

    eu.release()
  }

  val earlyPc = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val stage = eu.pipeline.fetch(eu.pipeline.fetch.size-2)
    setup.port.earlySample := stage.isReady
    setup.port.earlyPc := stage(PC)
  }
}
