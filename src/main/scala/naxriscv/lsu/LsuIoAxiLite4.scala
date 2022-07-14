package naxriscv.lsu

import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer

class LsuPeripheralAxiLite4 extends Plugin{
  val logic = create late new Area{
    val cache = getService[LsuPlugin]
    val native = cache.peripheralBus.setAsDirectionLess
    val axiRaw = native.toAxiLite4()
    val axi = master(cloneOf(axiRaw))
    axi.aw << axiRaw.aw.halfPipe()
    axi.w  << axiRaw.w.halfPipe()
    axi.b.halfPipe()  >> axiRaw.b
    axi.ar << axiRaw.ar
    axi.r.halfPipe()  >> axiRaw.r
    AxiLite4SpecRenamer(axi)
  }
}
