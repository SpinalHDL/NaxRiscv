// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv

import naxriscv.execute._
import naxriscv.utilities.Plugin

object Tweek {
  def euWritebackAt(plugins: Seq[Plugin], euId : String, at : Int): Unit = plugins.foreach {
    case p : IntAluPlugin    if p.euId == euId => p.writebackAt = 1
    case p : ShiftPlugin     if p.euId == euId => p.writebackAt = 1
    case p : BranchPlugin    if p.euId == euId => p.writebackAt = 1
    case p : MulPlugin       if p.euId == euId=> p.writebackAt = at
    case p : DivPlugin       if p.euId == euId=> p.writebackAt = at
    case p : CsrAccessPlugin if p.euId == euId=> p.writebackAt = at
    case _ =>
  }
}
