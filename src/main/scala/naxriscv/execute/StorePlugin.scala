package naxriscv.execute

import naxriscv.{DecodeListType, Frontend, ROB}
import naxriscv.interfaces.{DecoderService, MicroOp, RS2}
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, IntRegFile, Rvi}
import naxriscv.utilities._
import spinal.core._
import spinal.lib.pipeline.Stageable

object StorePlugin extends AreaObject{
  val SEL = Stageable(Bool())
  val AMO = Stageable(Bool())
  val SC = Stageable(Bool())
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

    def add(microOp: MicroOp, srcKeys: List[SrcKeys], decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding :+ (SEL -> True))
      if (srcKeys.nonEmpty) {
        getService[SrcPlugin](euId).specify(microOp, srcKeys)
      }
    }

    val sk = SrcKeys
    val srcOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
    add(Rvi.SB , srcOps, List(AMO -> False, SC -> False))
    add(Rvi.SH , srcOps, List(AMO -> False, SC -> False))
    add(Rvi.SW , srcOps, List(AMO -> False, SC -> False))


    val amos = List(
      Rvi.AMOSWAP, Rvi.AMOADD, Rvi.AMOXOR, Rvi.AMOAND, Rvi.AMOOR,
      Rvi.AMOMIN, Rvi.AMOMAX, Rvi.AMOMINU, Rvi.AMOMAXU
    )
    amos.foreach(add(_, List(sk.Op.SRC1, sk.SRC1.RF), List(AMO -> True, SC -> False)))
  }

  val logic = create late new Area{
    val lsu = getService[LsuPlugin]
    val decoder = getService[DecoderService]
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._
    val func3 = Frontend.MICRO_OP(Const.funct3Range)
    setup.port.valid := isFireing && SEL
    setup.port.address := U(SrcStageables.ADD_SUB)
    setup.port.data    := eu(IntRegFile, RS2)
    setup.port.sqId := lsu.keys.LSU_ID.resized
    setup.port.robId := ROB.ID
    setup.port.size := U(func3(1 downto 0))
    setup.port.sc  := SC
    setup.port.amo := AMO
    setup.port.swap := Frontend.MICRO_OP(27)
    setup.port.op  := Frontend.MICRO_OP(29, 3 bits)
    setup.port.physicalRd := decoder.PHYS_RD
    setup.port.writeRd := decoder.WRITE_RD
    eu.release()
  }
}
