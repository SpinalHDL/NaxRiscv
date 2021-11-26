package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.AddressTranslationService
import naxriscv.utilities.Plugin
import spinal.core._

class StaticAddressTranslationPlugin(peripheralRange : SInt => Bool) extends Plugin with AddressTranslationService{
  override def virtualWidth = Global.XLEN.get
  override def physicalWidth = Global.XLEN.get
  override def newTranslationPort(arg: Any) = ???

  val setup = create early new Area{

  }

  val logic = create late new Area{

  }
}
