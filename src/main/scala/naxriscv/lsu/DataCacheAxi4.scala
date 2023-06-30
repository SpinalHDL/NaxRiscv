// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.lsu

import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer

class DataCacheAxi4(dataWidth : Int) extends Plugin{
  val logic = create late new Area{
    val cache = getService[DataCachePlugin]
    val native = cache.mem.setAsDirectionLess
    val axi = master(native.resizer(dataWidth).toAxi4())
    Axi4SpecRenamer(axi)
  }
}
