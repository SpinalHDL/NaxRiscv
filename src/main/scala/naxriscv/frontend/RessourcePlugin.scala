package naxriscv.frontend

import naxriscv.interfaces.{DecoderService, Encoding, ExecuteUnitService, RenamerService, RfAllocationService, Riscv}
import naxriscv.pipeline._
import naxriscv.frontend.Frontend._
import naxriscv.utilities.Plugin
import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class RessourcePlugin() extends Plugin {

  val setup = create early new Area{
    getService[FrontendPlugin].retain()

    val intAllocService = getService[RfAllocationService](Riscv.integer.regfile)
    val intAllocPort = List.fill(DISPATCH_COUNT)(intAllocService.newAllocPort())

    val translationService = getService[RenamerService]()
    val translationPort = List.fill(DISPATCH_COUNT)(translationService.newTranslationPort())
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val stage = frontend.pipeline.renamed
    import stage._

    val blocked = False
    haltIt(blocked)

    val slot = for(slotId <- 0 until DISPATCH_COUNT) yield new Area {
      val valid = stage.isValid && (DISPATCH_MASK, slotId)

      // RD allocation
      when(valid && (Riscv.WRITE_RD,slotId)){

      }

      // RS translations
      for(rsId <- 0 until 2){
        when(valid && (Riscv.READ_RS(rsId),slotId)){

        }
      }
    }


    frontend.release()
  }
}