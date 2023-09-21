package naxriscv.misc

import naxriscv.fetch.FetchCachePlugin
import spinal.core._
import spinal.lib._
import naxriscv.interfaces.CsrService
import naxriscv.lsu.DataCachePlugin
import naxriscv.utilities.Plugin

class PerfCsrPlugin extends Plugin{
  val setup = create early new Area {
    val csr = getService[CsrService]
    csr.retain()
  }

  val logic = create late new Area {
    val csr = getService[CsrService]

    def add(csrId : Int, probe : Bool) = new Area{
      val counter = Reg(UInt(32 bits)) init(0)
      when(probe.pull()){counter := counter + 1}
      csr.readWrite(counter, csrId)
    }

    val i = getService[FetchCachePlugin]
    val iRefill = add(0x840, i.logic.refill.fire)


    val d = getService[DataCachePlugin]
    val dRefill = add(0x844, d.logic.cache.refill.push.fire)
    val dWriteback = add(0x845, d.logic.cache.writeback.push.fire)

    csr.release()
  }
}
