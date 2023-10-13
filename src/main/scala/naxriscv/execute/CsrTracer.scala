// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute

import naxriscv.interfaces._
import naxriscv.utilities.{DocPlugin, Plugin, WithRfWriteSharedSpec}
import spinal.core._
import spinal.lib._

class CsrTracer() extends Plugin{
  val setup = create early new Area{
    val csr = getService[CsrService]
    csr.retain()
  }

  val logic = create late new Area {
    val s = setup.get
    import s._

    val csrId = 0x8FF

    val flow = Flow(Bits(8 bits))
    flow.valid := False
    flow.payload := 0

    val flowOut = master(flow.stage())

    //Flush the pipeline if the rounding mode changed
    csr.onWrite(csrId, true){
      flow.valid := True
      flow.payload := csr.onWriteBits.resized
    }

    csr.release()
  }
}
