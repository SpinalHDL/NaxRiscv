package naxriscv.execute

import naxriscv.Global.{RVD, RVF, XLEN}
import naxriscv.frontend.RfDependencyPlugin
import naxriscv.{DecodeListType, Frontend, ROB}
import naxriscv.interfaces.{DecoderService, MicroOp, RS2}
import naxriscv.lsu.LsuPlugin
import naxriscv.riscv.{Const, IntRegFile, Rvfd, Rvi}
import naxriscv.utilities._
import spinal.core._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable.ArrayBuffer

object StorePlugin extends AreaObject{
  val SEL = Stageable(Bool())
  val AMO = Stageable(Bool())
  val SC = Stageable(Bool())
}

class StorePlugin(val euId : String) extends Plugin{
  import StorePlugin._
  val setup = create early new Area {
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    val lsu = getService[LsuPlugin]
    val rfDep = getService[RfDependencyPlugin]
    eu.retain()

    val port = lsu.newStorePort()
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
    val srcOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
    val stores = ArrayBuffer(Rvi.SB, Rvi.SH, Rvi.SW)
    if(XLEN.get == 64) stores ++= List(Rvi.SD)
    if(RVF) stores ++= List(Rvfd.FSW)
    if(RVD) stores ++= List(Rvfd.FSD)

    for(store <- stores) add(store, srcOps, List(AMO -> False, SC -> False))

    val amos = List(
      Rvi.AMOSWAP, Rvi.AMOADD, Rvi.AMOXOR, Rvi.AMOAND, Rvi.AMOOR,
      Rvi.AMOMIN, Rvi.AMOMAX, Rvi.AMOMINU, Rvi.AMOMAXU
    )
    for(amo <- amos) add(amo, List(sk.Op.SRC1, sk.SRC1.RF), List(AMO -> True, SC -> False))
    add(Rvi.SC, List(sk.Op.SRC1, sk.SRC1.RF), List(AMO -> False, SC -> True))

    val all = stores ++ amos ++ List(Rvi.SC)
    for(op <- all) rfDep.issueSkipRs(op, 1) //Handled by the LSU in oder to improve address checking
  }

  val logic = create late new Area{
    val lsu = getService[LsuPlugin]
    val decoder = getService[DecoderService]
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._
    val func3 = Frontend.MICRO_OP(Const.funct3Range)
    setup.port.valid := isFireing && SEL
    setup.port.address := U(SrcStageables.ADD_SUB).resized
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
