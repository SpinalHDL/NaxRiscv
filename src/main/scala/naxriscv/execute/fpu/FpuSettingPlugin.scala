// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute.fpu

import naxriscv.Global.{RVD, RVF}
import naxriscv.utilities.{DocPlugin, Plugin}

class FpuSettingPlugin(val rvf : Boolean, val rvd : Boolean) extends Plugin{
  create config{
    RVF.set(rvf)
    RVD.set(rvd)
    val doc = getService[DocPlugin]
    if(RVF) doc.property("RVF", rvf)
    if(RVD) doc.property("RVD", rvd)
  }
}
