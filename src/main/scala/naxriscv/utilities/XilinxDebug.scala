package naxriscv.utilities
import naxriscv.fetch._
import naxriscv.lsu.{DataCachePlugin, LsuPlugin}
import naxriscv.misc.{CommitPlugin, MmuPlugin}
import spinal.core._
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
      }
      case p : MmuPlugin => {
        patch(p.logic.refill.busy)
        patch(p.logic.refill.stateReg)
      }
      case p : CommitPlugin => {
        patch(p.logic.reschedule.valid)
        patch(p.logic.reschedule.pcTarget)
        patch(p.logic.reschedule.trap)
      }
      case _ =>
    }
  }
}
