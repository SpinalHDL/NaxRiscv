package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.AddressTranslationService
import naxriscv.utilities.Plugin
import spinal.core._

class StaticAddressTranslationPlugin extends Plugin with AddressTranslationService{

  override def newTranslationPort() = ???

  val setup = create early new Area{

  }

  val logic = create late new Area{

  }
}
