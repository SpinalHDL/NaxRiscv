package naxriscv.execute

import naxriscv.Global._
import naxriscv.fetch.{FetchCachePlugin, FetchPlugin}
import naxriscv.frontend.DispatchPlugin
import naxriscv.interfaces.{AddressTranslationService, CommitService, DecoderService, MicroOp, ScheduleReason}
import naxriscv.lsu.LsuPlugin
import naxriscv.misc.PrivilegedPlugin
import naxriscv.riscv.{CSR, Const, Rvi}
import naxriscv.utilities._
import naxriscv.{DecodeList, DecodeListType, Frontend, ROB}
import spinal.core._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.pipeline.Stageable

object EnvCallPlugin extends AreaObject{
  val ECALL = Stageable(Bool())
  val EBREAK = Stageable(Bool())
  val XRET = Stageable(Bool())
  val WFI = Stageable(Bool())
  val FENCE = Stageable(Bool())
  val FENCE_I = Stageable(Bool())
  val FENCE_VMA = Stageable(Bool())

  val CAUSE_XRET = CSR.MCAUSE_ENUM.ECALL_USER
  val CAUSE_FLUSH = CSR.MCAUSE_ENUM.ECALL_SUPERVISOR
}

class EnvCallPlugin(euId : String)(rescheduleAt : Int = 0) extends Plugin{
  import EnvCallPlugin._

  val setup = create early new Area {
    val dispatch = getService[DispatchPlugin]
    val commit = getService[CommitService]
    val priv = getService[PrivilegedPlugin]
    val fetch = getService[FetchPlugin]
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()
    fetch.retain()

    val reschedule = commit.newSchedulePort(canTrap = true, canJump = false)

    def add(microOp: MicroOp, decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding)
      eu.setStaticCompletion(microOp, rescheduleAt)
    }

    List(ECALL, EBREAK, XRET, WFI, FENCE, FENCE_I, FENCE_VMA).foreach(eu.setDecodingDefault(_, False))
    add(Rvi.WFI   , DecodeList(WFI    -> True))
    add(Rvi.ECALL , DecodeList(ECALL  -> True))
    add(Rvi.EBREAK, DecodeList(EBREAK -> True))
    add(Rvi.MRET  , DecodeList(XRET   -> True))
    add(Rvi.FENCE , DecodeList(FENCE   -> True))
    add(Rvi.FENCE_I  , DecodeList(FENCE_I   -> True))
    add(Rvi.SFENCE_VMA , DecodeList(FENCE_VMA   -> True))
    if(priv.implementSupervisor) add(Rvi.SRET  , DecodeList(XRET   -> True))
    if(priv.implementUserTrap)   add(Rvi.URET  , DecodeList(XRET   -> True))

    dispatch.fenceOlder(Rvi.FENCE_I)   //Ensure we do not generate uncommitted flushes
    dispatch.fenceOlder(Rvi.SFENCE_VMA)

    eu.addRobStageable(PC) //Used by ebreak
  }

  val logic = create late new Area{
    val s = setup.get
    val eu = setup.eu
    val priv = getService[PrivilegedPlugin]
    val stage = eu.getExecute(rescheduleAt)
    import stage._
    import s._

    setup.reschedule.valid      := isValid && (EBREAK || ECALL || XRET || FENCE_I || FENCE_VMA)
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

    when(FENCE_I || FENCE_VMA){
      setup.reschedule.cause      := CAUSE_FLUSH
    }

    //Handle FENCE.I and FENCE.VMA
    val flushes = new StateMachine{
      val IDLE, RESCHEDULE, VMA_FETCH_FLUSH, VMA_FETCH_WAIT, LSU_FLUSH, WAIT_LSU = new State
      setEntry(IDLE)

      fetch.getStage(0).haltIt(!isActive(IDLE))

      val vmaPort = getService[AddressTranslationService].invalidatePort
      val fetchPort = getService[FetchCachePlugin].invalidatePort
      val lsuPort   = getService[LsuPlugin].flushPort

      val vmaInv, fetchInv = Reg(Bool())

      //Enforce pipeline availability for the delayed build of the FSM
      stage(FENCE_I)
      stage(FENCE_VMA)

      IDLE whenIsActive{
        when(isValid && (FENCE_I || FENCE_VMA)){
          vmaInv   := FENCE_VMA
          fetchInv := FENCE_I
          when(isReady){
            goto(RESCHEDULE)
          }
        }
      }

      RESCHEDULE whenIsActive{
        when(commit.reschedulingPort().valid){
          goto(VMA_FETCH_FLUSH)
        }
      }

      VMA_FETCH_FLUSH whenIsActive {
        vmaPort.request   setWhen(vmaInv)
        fetchPort.request setWhen(fetchInv)
        goto(VMA_FETCH_WAIT)
      }

      VMA_FETCH_WAIT whenIsActive{
        when(fetchPort.served) {
          goto(LSU_FLUSH)
        }
        when(vmaPort.served){
          goto(IDLE)
        }
      }

      LSU_FLUSH whenIsActive{
        lsuPort.request   := True
        goto(WAIT_LSU)
      }

      WAIT_LSU whenIsActive{
        when(lsuPort.served){
          goto(IDLE)
        }
      }
    }

    fetch.release()
    eu.release()
  }
}
