package naxriscv.utilities
import naxriscv.{Frontend, ROB}
import naxriscv.execute.ExecutionUnitBase
import naxriscv.fetch._
import naxriscv.frontend.FrontendPlugin
import naxriscv.lsu.{DataCachePlugin, LsuPlugin}
import naxriscv.misc.{CommitPlugin, MmuPlugin}
import spinal.core._
import spinal.lib.Timeout
class XilinxDebug extends Plugin {
  val logic = create late new Area{
    def patch(that : Data) = that.addAttribute("mark_debug", "true")
    framework.plugins.foreach{
      case p : FetchCachePlugin => patch(p.logic.refill.valid)
      case p : DataCachePlugin => {
        p.logic.cache.writeback.slots.foreach(s => patch(s.valid))
        p.logic.cache.refill.slots.foreach(s => patch(s.valid))
      }
      case p : LsuPlugin => {
        patch(p.logic.special.enabled)
        patch(p.logic.special.isIo)
        p.logic.sq.regs.foreach(e => patch(e.data.inRf))
        patch(p.logic.sq.ptr.commit)
      }
      case p : MmuPlugin => {
        patch(p.logic.refill.busy)
        patch(p.logic.refill.stateReg)
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
      case p : FrontendPlugin => p.pipeline.stagesSet.foreach(s => patch(s.valid))
      case p : ExecutionUnitBase if p.euId == "EU0" => {
        p.pipeline.stagesSet.foreach(s => patch(s.valid))
        p.pipeline.stagesSet.foreach(s => patch(s(ROB.ID)))
      }
      case _ =>
    }
  }
}
