package naxriscv.execute

import naxriscv.Global._
import naxriscv.fetch.{FetchCachePlugin, FetchPlugin}
import naxriscv.frontend.DispatchPlugin
import naxriscv.interfaces.{AddressTranslationService, CommitService, DecoderService, MicroOp, ScheduleReason}
import naxriscv.lsu.{LsuFlusher, LsuPlugin}
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
  val FLUSH_DATA = Stageable(Bool())

  val CAUSE_XRET = CSR.MCAUSE_ENUM.ECALL_USER
  val CAUSE_FLUSH = CSR.MCAUSE_ENUM.ECALL_SUPERVISOR
  val CAUSE_REDO = CSR.MCAUSE_ENUM.ECALL_HYPERVISOR
}

class EnvCallPlugin(val euId : String)(var rescheduleAt : Int = 0) extends Plugin{
  import EnvCallPlugin._

  val setup = create early new Area {
    val dispatch = getService[DispatchPlugin]
    val commit = getService[CommitService]
    val priv = getService[PrivilegedPlugin]
    val fetch = getService[FetchPlugin]
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    eu.retain()
    fetch.retain()

    val reschedule = commit.newSchedulePort(canTrap = true, canJump = false)

    def add(microOp: MicroOp, decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.addDecoding(microOp, decoding)
      eu.setCompletion(microOp, rescheduleAt)
    }

    List(ECALL, EBREAK, XRET, WFI, FENCE, FENCE_I, FENCE_VMA, FLUSH_DATA).foreach(eu.setDecodingDefault(_, False))
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


    add(Rvi.FLUSH_DATA  , DecodeList(FLUSH_DATA   -> True))
    dispatch.fenceOlder(Rvi.FLUSH_DATA)

    eu.addRobStageable(PC) //Used by ebreak
  }

  val logic = create late new Area{
    val s = setup.get
    val eu = setup.eu
    val priv = getService[PrivilegedPlugin]
    val stage = eu.getExecute(rescheduleAt)
    import stage._
    import s._

    setup.reschedule.valid      := isValid && (EBREAK || ECALL || XRET || FENCE_I || FLUSH_DATA || FENCE_VMA)
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

    when(FENCE_I || FENCE_VMA || FLUSH_DATA){
      setup.reschedule.cause      := CAUSE_FLUSH
    }

    //Handle FENCE.I and FENCE.VMA
    val flushes = new StateMachine{
      val vmaPort = getService[AddressTranslationService].invalidatePort
      val fetchPort = getService[FetchCachePlugin].invalidatePort
      val lsuPort   = getServiceOption[LsuFlusher] match {
        case Some(lsu) => lsu.getFlushPort()
        case None => println("No LSU plugin for the EnvCallPlugin flush ???"); null
      }

      val IDLE, RESCHEDULE, VMA_FETCH_FLUSH, VMA_FETCH_WAIT = new State()
      val LSU_FLUSH, WAIT_LSU = (lsuPort != null) generate new State
      setEntry(IDLE)

      fetch.getStage(0).haltIt(!isActive(IDLE))

      val vmaInv, fetchInv, flushData = Reg(Bool())

      IDLE whenIsActive{
        when(isValid) {
          vmaInv := FENCE_VMA
          fetchInv := FENCE_I
          flushData := FLUSH_DATA
          when(isReady) {
            when(FENCE_I || FENCE_VMA) {
              goto(RESCHEDULE)
            }
            when(FLUSH_DATA) {
              goto(if(lsuPort != null) LSU_FLUSH else IDLE)
            }
          }
        }
      }

      RESCHEDULE whenIsActive{
        when(commit.reschedulingPort(onCommit = true).valid){
          goto(VMA_FETCH_FLUSH)
        }
      }

      VMA_FETCH_FLUSH whenIsActive {
        vmaPort.cmd.valid   setWhen(vmaInv)
        fetchPort.cmd.valid setWhen(fetchInv)
        goto(VMA_FETCH_WAIT)
      }

      VMA_FETCH_WAIT whenIsActive{
        when(fetchPort.rsp.valid) {
          goto(if(lsuPort != null) LSU_FLUSH else IDLE)
        }
        when(vmaPort.rsp.valid){
          goto(IDLE)
        }
      }

      if(lsuPort != null) {
        LSU_FLUSH whenIsActive {
          lsuPort.cmd.valid := True
          lsuPort.cmd.withFree := flushData
          goto(WAIT_LSU)
        }

        WAIT_LSU whenIsActive {
          when(lsuPort.rsp.valid) {
            goto(IDLE)
          }
        }
      }

      build()
    }

    fetch.release()
    eu.release()
  }
}
