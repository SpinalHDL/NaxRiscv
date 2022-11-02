package naxriscv.execute

import naxriscv.Global.{PC, RVD, RVF, XLEN}
import naxriscv.frontend._
import naxriscv.interfaces.{DecoderService, MicroOp, RS1, RS2}
import naxriscv.lsu2.Lsu2Plugin
import naxriscv.riscv.{Const, FloatRegFile, IntRegFile, Rvfd, Rvi}
import naxriscv.utilities._
import naxriscv.{DecodeListType, Frontend, ROB}
import spinal.core._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer


object AguPlugin extends AreaObject{
  val SEL = Stageable(Bool())
  val AMO = Stageable(Bool())
  val SC = Stageable(Bool())
  val LR = Stageable(Bool())
  val LOAD = Stageable(Bool())
}

class AguPlugin(val euId : String) extends Plugin{
  import AguPlugin._
  val setup = create early new Area {
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    val lsu = getService[Lsu2Plugin]
    val rfDep = getService[RfDependencyPlugin]
    eu.retain()

    val port = lsu.newAguPort()
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

    //Store section
    val srcOps = List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.S)
    val stores = ArrayBuffer(Rvi.SB, Rvi.SH, Rvi.SW)
    if(XLEN.get == 64) stores ++= List(Rvi.SD)

    for(store <- stores) add(store, srcOps, List(AMO -> False, SC -> False))
    if(RVF) add(Rvfd.FSW, srcOps, List(AMO -> False, SC -> False))
    if(RVD) add(Rvfd.FSD, srcOps, List(AMO -> False, SC -> False))

    val amos = List(
      Rvi.AMOSWAP, Rvi.AMOADD, Rvi.AMOXOR, Rvi.AMOAND, Rvi.AMOOR,
      Rvi.AMOMIN, Rvi.AMOMAX, Rvi.AMOMINU, Rvi.AMOMAXU
    )
    for(amo <- amos) add(amo, List(sk.Op.SRC1, sk.SRC1.RF), List(AMO -> True, SC -> False, LOAD -> False))
    add(Rvi.SC, List(sk.Op.SRC1, sk.SRC1.RF), List(AMO -> False, SC -> True, LOAD -> False))


//    val all = stores ++ amos ++ List(Rvi.SC)
//    for(op <- all) rfDep.issueSkipRs(op, 1) //Handled by the LSU in oder to improve address checking

    //Load section
    val loads = ArrayBuffer(Rvi.LB , Rvi.LH , Rvi.LW , Rvi.LBU, Rvi.LHU)
    if(XLEN.get == 64) loads ++= List(Rvi.LD, Rvi.LWU)
    if(RVF) loads ++= List(Rvfd.FLW)
    if(RVD) loads ++= List(Rvfd.FLD)

    for(op <- loads) add(op, List(sk.Op.ADD, sk.SRC1.RF, sk.SRC2.I), List(LR -> False, LOAD -> True))
    add(Rvi.LR,  List(sk.Op.SRC1, sk.SRC1.RF),  List(LR -> True, LOAD -> True))
  }

  val logic = create late new Area{
    val lsu = getService[Lsu2Plugin]
    val decoder = getService[DecoderService]
    val eu = setup.eu
    val stage = eu.getExecute(0)
    import stage._
    val func3 = Frontend.MICRO_OP(Const.funct3Range)
    val fired = RegInit(False) setWhen(setup.port.valid) clearWhen(isReady || isRemoved)
    setup.port.valid := isValid && SEL && !fired
    setup.port.address := U(SrcStageables.ADD_SUB).resized
    setup.port.robId := ROB.ID
    setup.port.size := U(func3(1 downto 0))
    setup.port.sc  := SC
    setup.port.amo := AMO
    setup.port.swap := Frontend.MICRO_OP(27)
    setup.port.op  := Frontend.MICRO_OP(29, 3 bits)
    setup.port.physicalRd := decoder.PHYS_RD
    setup.port.regfileRd := decoder.REGFILE_RD
    setup.port.writeRd := decoder.WRITE_RD

    setup.port.aguId := lsu.keys.LSU_ID.resized
    setup.port.unsigned := func3(2)
    setup.port.pc := PC
    setup.port.lr := LR
    setup.port.load := LOAD

    setup.port.data := eu.apply(IntRegFile, RS2).resized
    if(RVF) when(decoder.REGFILE_RS(RS2) === decoder.REGFILE_RS(RS2).rfToId(FloatRegFile)) {
      setup.port.data := eu.apply(FloatRegFile, RS2).resized
    }
    eu.release()
  }

  val earlyPc = create late new Area{
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    val stage = eu.pipeline.fetch(eu.pipeline.fetch.size-2)
    setup.port.earlySample := stage.isReady
    setup.port.earlyPc := stage(PC)
  }
}
