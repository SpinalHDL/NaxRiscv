package naxriscv.platform.asic

import naxriscv.ROB
import naxriscv.misc.{RegFileLatch, RegFileReadParameter, RegFileWriteParameter}
import naxriscv.utilities.{DataBase, NaxScope}
import spinal.core._
import spinal.core.internals.{MemTopology, PhaseContext, PhaseMemBlackBoxingWithPolicy, PhaseNetlist}
import spinal.lib._


object TestRfGen extends App {
  SpinalSky130() {
    val database = new DataBase
    val framework = NaxScope(database)
    framework on {
      ROB.SIZE.set(64)
      new RegFileLatch(
        addressWidth = 4,
        dataWidth = 2,
        readsParameter = List.fill(1)(RegFileReadParameter(false, false)),
        writesParameter = List.fill(1)(RegFileWriteParameter(false)),
        headZero = false
      ).setDefinitionName("rf")
    }
  }
}
