// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import naxriscv.utilities.Plugin
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer

class FetchAxi4(ramDataWidth : Int,
                ioDataWidth : Int,
                toPeripheral : FetchL1Cmd => Bool
                ) extends Plugin{
  val logic = create late new Area{
    val cache = getService[FetchCachePlugin]
    val native = cache.mem.setAsDirectionLess

    val (ram, peripheral) = native.split(toPeripheral)
    val axiRam = master(ram.resizer(ramDataWidth).toAxi4())
    val axiPeripheral  = master(peripheral.resizer(ioDataWidth).toAxiLite4())

    Axi4SpecRenamer(axiRam)
    AxiLite4SpecRenamer(axiPeripheral)
  }
}
