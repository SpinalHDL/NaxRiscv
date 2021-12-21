package naxriscv.backend

import spinal.core._
import naxriscv.utilities.Plugin
import spinal.lib.CountOne

class CommitDebugFilterPlugin(factors : Seq[Int]) extends Plugin{
  val logic = create late new Area{
    val commits = CountOne(getService[CommitPlugin].onCommit().mask) << 16
    val filters = for(f <- factors) yield new Area{
      val value = Reg(UInt(32 bits)) init(0)
      value := value + U(S(commits - value) |>> f)
    }
  }
}
