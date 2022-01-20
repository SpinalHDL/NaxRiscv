package naxriscv.execute

import naxriscv.Global._
import naxriscv.interfaces.{CommitService, DecoderService, MicroOp, ScheduleReason}
import naxriscv.lsu.LsuPlugin
import naxriscv.misc.PrivilegedPlugin
import naxriscv.riscv.{CSR, Const, Rvi}
import naxriscv.utilities._
import naxriscv.{DecodeList, DecodeListType, Frontend, ROB}
import spinal.core._
import spinal.lib.pipeline.Stageable

object EnvCallPlugin extends AreaObject{
  val ECALL = Stageable(Bool())
  val EBREAK = Stageable(Bool())
  val XRET = Stageable(Bool())
  val WFI = Stageable(Bool())
  val FENCE = Stageable(Bool())

  val CAUSE_XRET = CSR.MCAUSE_ENUM.ECALL_USER
}

class EnvCallPlugin(euId : String)(rescheduleAt : Int = 0) extends Plugin{
  import EnvCallPlugin._

  val setup = create early new Area {
    val commit = getService[CommitService]
    val priv = getService[PrivilegedPlugin]
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    val reschedule = commit.newSchedulePort(canTrap = true, canJump = false)

    def add(microOp: MicroOp, decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding)
      eu.setStaticCompletion(microOp, rescheduleAt)
    }

    List(ECALL, EBREAK, XRET, WFI).foreach(eu.setDecodingDefault(_, False))
    add(Rvi.WFI   , DecodeList(WFI    -> True))
    add(Rvi.ECALL , DecodeList(ECALL  -> True))
    add(Rvi.EBREAK, DecodeList(EBREAK -> True))
    add(Rvi.MRET  , DecodeList(XRET   -> True))
    add(Rvi.FENCE , DecodeList(FENCE   -> True))
    if(priv.implementSupervisor) add(Rvi.SRET  , DecodeList(XRET   -> True))
    if(priv.implementUserTrap)   add(Rvi.URET  , DecodeList(XRET   -> True))

    eu.addRobStageable(PC) //Used by ebreak
  }

  val logic = create late new Area{
    val eu = setup.eu
    val priv = getService[PrivilegedPlugin]
    val stage = eu.getExecute(rescheduleAt)
    import stage._

    setup.reschedule.valid      := isValid && (EBREAK || ECALL || XRET)
    setup.reschedule.robId      := ROB.ID
    setup.reschedule.tval       := B(PC).andMask(EBREAK) //That's what spike do
    setup.reschedule.skipCommit := EBREAK || ECALL
    setup.reschedule.reason     := ScheduleReason.ENV
    setup.reschedule.cause.assignDontCare()

    val xretPriv = Frontend.MICRO_OP(29 downto 28).asUInt
    when(XRET){
      setup.reschedule.cause            := CAUSE_XRET //the reschedule cause isn't the final value which will end up into XCAUSE csr
      setup.reschedule.tval(1 downto 0) := xretPriv.asBits
      when(xretPriv < priv.getPrivilege()){
        setup.reschedule.cause      := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
        setup.reschedule.reason     := ScheduleReason.TRAP
        setup.reschedule.skipCommit := True
      }
    }
    when(EBREAK){
      setup.reschedule.cause      := CSR.MCAUSE_ENUM.BREAKPOINT
    }
    when(ECALL){
      setup.reschedule.cause      := CSR.MCAUSE_ENUM.ECALL_MACHINE //the reschedule cause isn't the final value which will end up into XCAUSE csr
    }

    eu.release()
  }
}
