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
    if(priv.implementSupervisor) add(Rvi.SRET  , DecodeList(XRET   -> True))
    if(priv.implementUserTrap)   add(Rvi.URET  , DecodeList(XRET   -> True))
  }

  val logic = create late new Area{
    val eu = setup.eu
    val priv = getService[PrivilegedPlugin]
    val stage = eu.getExecute(rescheduleAt)
    import stage._

    val mretPriv = Frontend.MICRO_OP(29)
    val sretPriv = Frontend.MICRO_OP(28)

    val xretTrap = False
    xretTrap setWhen(mretPriv && !priv.hasMachinePriv)
    if(priv.implementSupervisor) xretTrap setWhen(sretPriv && !priv.hasSupervisorPriv)
    xretTrap clearWhen(!XRET)

    setup.reschedule.valid      := isValid && (EBREAK || ECALL || XRET)
    setup.reschedule.robId      := ROB.ID
//    setup.reschedule.trap       := EBREAK || ECALL || xretTrap
    setup.reschedule.tval       := 0
    setup.reschedule.skipCommit := EBREAK || ECALL || xretTrap
    setup.reschedule.reason     := ScheduleReason.ENV
    setup.reschedule.cause.assignDontCare()

    when(XRET){
      setup.reschedule.cause      := CSR.MCAUSE_ENUM.ECALL_USER //the reschedule cause isn't the final value which will end up into XCAUSE csr
    }
    when(EBREAK){
      setup.reschedule.cause      := CSR.MCAUSE_ENUM.BREAKPOINT
    }
    when(ECALL){
      setup.reschedule.cause      := CSR.MCAUSE_ENUM.ECALL_MACHINE //the reschedule cause isn't the final value which will end up into XCAUSE csr
    }
    when(xretTrap){
      setup.reschedule.cause      := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      setup.reschedule.reason     := ScheduleReason.TRAP
      setup.reschedule.skipCommit := True
    }

    eu.release()
  }
}
