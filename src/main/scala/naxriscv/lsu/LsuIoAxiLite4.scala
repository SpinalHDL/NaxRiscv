// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.lsu

import naxriscv.lsu2.Lsu2Plugin
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer

class LsuPeripheralAxiLite4(ioDataWidth : Int, 
                            reg_stage_cmd : Boolean = false,
                            reg_stage_ret: Boolean = true) extends Plugin{
  val logic = create late new Area{
    val peripheralBus = getServiceOption[LsuPlugin].map(_.peripheralBus).getOrElse(getServiceOption[Lsu2Plugin].map(_.peripheralBus).get)
    val native = peripheralBus.setAsDirectionLess
    val resized = native.resize(ioDataWidth)
    val axiRaw = resized.toAxiLite4()
    val axi = master(cloneOf(axiRaw))
    if (reg_stage_cmd) {
      axi.aw << axiRaw.aw.halfPipe()
      axi.w  << axiRaw.w.halfPipe()
      axi.ar << axiRaw.ar.halfPipe()
    } else {
      axi.aw << axiRaw.aw
      axi.w  << axiRaw.w
      axi.ar << axiRaw.ar
    }
    if (reg_stage_ret) {
      axi.b.halfPipe()  >> axiRaw.b
      axi.r.halfPipe()  >> axiRaw.r
    } else {
      axi.b  >> axiRaw.b
      axi.r  >> axiRaw.r
    }
    AxiLite4SpecRenamer(axi)
  }
}
