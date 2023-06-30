// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.utilities
import naxriscv.Frontend.DISPATCH_MASK
import naxriscv.{Frontend, ROB}
import naxriscv.execute.ExecutionUnitBase
import naxriscv.fetch._
import naxriscv.frontend.{DispatchPlugin, FrontendPlugin}
import naxriscv.lsu.{DataCachePlugin, LsuPlugin}
import naxriscv.lsu2.Lsu2Plugin
import naxriscv.misc.{CommitPlugin, MmuPlugin, PrivilegedConfig, PrivilegedPlugin, RobPlugin}
import spinal.core._
import spinal.lib.Timeout
class XilinxDebug extends Plugin {
  val logic = create late new Area{
    def patch(that : Data) = that.addAttribute("mark_debug", "true")
    framework.plugins.foreach{
      case p : FetchCachePlugin => {
        patch(p.logic.refill.valid)
      }
      case p : DataCachePlugin => {
        p.logic.cache.writeback.slots.foreach(s => patch(s.valid))
        p.logic.cache.refill.slots.foreach(s => patch(s.valid))
        patch(p.lockPort)
        patch(p.logic.cache.io.load.cmd.valid)
        patch(p.logic.cache.io.load.cmd.unlocked)
        patch(p.logic.cache.io.load.rsp.valid)
        patch(p.logic.cache.io.load.rsp.redo)
        patch(p.logic.cache.io.load.rsp.refillSlot)
        patch(p.logic.cache.io.load.rsp.refillSlotAny)
        patch(p.logic.cache.io.load.rsp.fault)

        patch(p.logic.cache.io.store.cmd.valid)
        patch(p.logic.cache.io.store.cmd.generation)
        patch(p.logic.cache.io.store.rsp.valid)
        patch(p.logic.cache.io.store.rsp.redo)
        patch(p.logic.cache.io.store.rsp.fault)
        patch(p.logic.cache.io.store.rsp.generationKo)
        patch(p.logic.cache.io.store.rsp.refillSlot)
        patch(p.logic.cache.io.store.rsp.refillSlotAny)
      }
      case p : LsuPlugin => {
        patch(p.logic.special.enabled)
        patch(p.logic.special.isIo)
        p.logic.sq.regs.foreach(e => patch(e.data.inRf))
        patch(p.logic.sq.ptr.commit)
      }
      case p : Lsu2Plugin => {
        p.logic.builders.get
        patch(p.logic.special.enabled)
        patch(p.logic.special.isIo)
        patch(p.logic.special.atomic.stateReg)
        patch(p.logic.sq.ptr.commit)
        patch(p.logic.sq.ptr.writeBack)
        patch(p.logic.sq.ptr.free)
        p.logic.sharedPip.stages.foreach{s =>
          patch(s.valid)
          patch(s(ROB.ID))
          patch(s(p.keys.IS_LOAD))
        }
        patch(p.logic.sharedPip.stages.last(p.keys.IS_IO))
        patch(p.logic.sharedPip.stages.last(p.keys.CTRL))
        patch(p.logic.sharedPip.stages.last(p.keys.HIT_SPECULATION))
      }
      case p : MmuPlugin => {
        patch(p.logic.refill.busy)
        patch(p.logic.refill.stateReg)
      }
      case p : PrivilegedPlugin => {
        patch(p.logic.machine.mstatus.mie)
        patch(p.logic.machine.mstatus.mpie)
        patch(p.logic.machine.mstatus.mpp)

        patch(p.logic.machine.mip.meip)
        patch(p.logic.machine.mip.mtip)
        patch(p.logic.machine.mip.msip)

        patch(p.logic.machine.mie.meie)
        patch(p.logic.machine.mie.mtie)
        patch(p.logic.machine.mie.msie)

        patch(p.logic.supervisor.sstatus.sie)
        patch(p.logic.supervisor.sstatus.spie)
        patch(p.logic.supervisor.sstatus.spp)

        patch(p.logic.supervisor.sip.seipOr)
        patch(p.logic.supervisor.sip.seipSoft)

        patch(p.logic.supervisor.sie.seie)

        patch(p.setup.privilege)



        patch(p.logic.fsm.trap.fire)
        patch(p.logic.fsm.trap.interrupt)
        patch(p.logic.fsm.trap.code)
        patch(p.logic.fsm.trap.targetPrivilege)
        patch(p.logic.fsm.trap.debug)
        patch(p.logic.fsm.trap.dcause)
        patch(p.logic.fsm.trap.debugException)

      }
      case p : CommitPlugin => {
        patch(p.logic.reschedule.valid)
        patch(p.logic.reschedule.pcTarget)
        patch(p.logic.reschedule.trap)
        patch(p.logic.reschedule.cause)
        patch(p.logic.reschedule.reason)
        val dcacheCmd = getService[DataCachePlugin].logic.cache.io.store.cmd
        val icacheInv = getService[FetchCachePlugin].logic.invalidate
        val flushingStuff = dcacheCmd.valid && dcacheCmd.flush || !icacheInv.done
        def addTimeout(cycles : Int): Unit ={
          val noCommit = Timeout(cycles)
          when(p.onCommit().mask.orR || flushingStuff) {noCommit.clear()}
          patch(noCommit.state.setName("noCommitTrigger" + cycles))
        }
        addTimeout(256)
        addTimeout(1000)
        addTimeout(2000)
        patch(p.logic.whitebox.robToPc.valid)
        patch(p.logic.whitebox.robToPc.robId)
        p.logic.whitebox.robToPc.pc.foreach(patch)
        p.logic.ptr.stage.apply(0 until Frontend.DECODE_COUNT)(Frontend.MICRO_OP).foreach(patch)
        patch(p.logic.ptr.commit)
        patch(p.logic.ptr.alloc)
        patch(p.logic.ptr.free)
        patch(p.logic.whitebox.commit)
      }
      case p : FetchPlugin => p.pipeline.stages.foreach(s => patch(s.valid))
      case p : FrontendPlugin => {
        p.builder.get
        p.pipeline.stagesSet.foreach(s => patch(s.valid))
        p.pipeline.decoded.internals.request.halts.foreach(e => patch(e))
        p.pipeline.dispatch.internals.request.halts.foreach(e => patch(e))
        patch(p.pipeline.dispatch(ROB.ID))
        p.pipeline.dispatch(0 until Frontend.DECODE_COUNT)(DISPATCH_MASK).foreach(patch)
      }
      case p : ExecutionUnitBase if p.euId == "EU0" => {
        p.pipeline.stagesSet.foreach(s => patch(s.valid))
        p.pipeline.stagesSet.foreach(s => patch(s(ROB.ID)))
      }
      case p : DebugScratchCsrPlugin => patch(p.logic.data)
      case p : RobPlugin => {
        p.logic.get
        p.completions.foreach(e => patch(e.bus))
      }
      case p : DispatchPlugin => {
        patch(p.logic.push.fenceYounger)
        patch(p.logic.push.fenceYoungerLast)
        patch(p.logic.push.commitNotWaitingOnUs)
      }
      case _ =>
    }
  }
}
