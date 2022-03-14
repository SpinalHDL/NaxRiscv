package naxriscv

import naxriscv.execute._
import naxriscv.utilities.Plugin

object Tweek {
  def aluTwoCycle(plugins: Seq[Plugin]): Unit = plugins.foreach {
    case p : IntAluPlugin => p.writebackAt = 1
    case p : ShiftPlugin  => p.writebackAt = 1
    case p : BranchPlugin => p.writebackAt = 1
    case _ =>
  }
  def mulDivEnvWbAt(plugins: Seq[Plugin], at : Int): Unit = plugins.foreach {
    case p : MulPlugin => p.writebackAt = at
    case p : DivPlugin  => p.writebackAt = at
    case p : CsrAccessPlugin => p.writebackAt = at
    case _ =>
  }
}
