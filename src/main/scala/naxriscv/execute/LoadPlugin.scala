package naxriscv.execute

import naxriscv.{DecodeListType, Frontend, ROB}
import naxriscv.interfaces.{AddressTranslationService, DecoderService, MicroOp}
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, Rvi}
import naxriscv.utilities._
import spinal.core._
import spinal.lib.pipeline.Stageable
import naxriscv.Global._

import scala.collection.mutable.ArrayBuffer

object LoadPlugin extends AreaObject{
  val SEL      = Stageable(Bool())
  val UNSIGNED = Stageable(Bool())
  val LR       = Stageable(Bool())
}

class LoadPlugin(val euId : String) extends Plugin{
  import LoadPlugin._
  val setup = create early new Area {
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    val lsu = getService[LsuPlugin]
    eu.retain()

    val port = lsu.newLoadPort()
    eu.addRobStageable(lsu.keys.LSU_ID)
    eu.setDecodingDefault(SEL, False)

    def add(microOp: MicroOp, srcKeys: List[SrcKeys], decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding :+ (SEL -> True))
      if (srcKeys.nonEmpty) {
        findService[SrcPlugin](_.euId == euId).specify(microOp, srcKeys)
      }
    }

    val sk = SrcKeys
    val loads = ArrayBuffer(Rvi.LB , Rvi.LH , Rvi.LW , Rvi.LBU, Rvi.LHU)
    if(XLEN.get == 64) loads ++= List(Rvi.LD, Rvi.LWU)

    for(op <- loads) add(op, List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I), List(LR -> False))
    add(Rvi.LR,  List(sk.Op.SRC1, sk.SRC1.RF),  List(LR -> True))
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
    setup.port.address := U(SrcStageables.ADD_SUB).resized
    setup.port.size := U(func3(1 downto 0))
    setup.port.unsigned := func3(2)
    setup.port.physicalRd := decoder.PHYS_RD
    setup.port.writeRd := decoder.WRITE_RD
    setup.port.pc := PC
    setup.port.lr := LR

    eu.release()
  }

  val earlyPc = create late new Area{
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    val stage = eu.pipeline.fetch(eu.pipeline.fetch.size-2)
    setup.port.earlySample := stage.isReady
    setup.port.earlyPc := stage(PC)
  }
}
