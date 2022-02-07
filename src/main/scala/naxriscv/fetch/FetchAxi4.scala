package naxriscv.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import naxriscv.utilities.Plugin
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer

class FetchAxi4 extends Plugin{
  val logic = create late new Area{
    val cache = getService[FetchCachePlugin]
    val native = cache.mem.setAsDirectionLess

    val (io, ram) = native.ioSplit()
    val axiRam = master(ram.toAxi4())
    val axiIo  = master(io.toAxiLite4())

    Axi4SpecRenamer(axiRam)
    AxiLite4SpecRenamer(axiIo)
  }
}
