package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.AddressTranslationService
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib.pipeline.Stageable

class StaticAddressTranslationPlugin(peripheralRange : SInt => Bool) extends Plugin with AddressTranslationService{
  override def virtualWidth = Global.XLEN.get
  override def physicalWidth = Global.XLEN.get
  override def newTranslationPort(arg: Any) = ???

  override val PC = Stageable(UInt(Global.XLEN.get bits)).setName("PC")

  val setup = create early new Area{

  }

  val logic = create late new Area{

  }
}
