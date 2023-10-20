package naxriscv.platform.asic

import spinal.core._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.com.usb.udc._

object UsbDeviceCtrlGen extends App {
  SpinalVerilog(new UsbDeviceCtrl(
    p = UsbDeviceCtrlParameter(
      addressWidth = 10
    ),
    bmbParameter = BmbParameter(
      addressWidth = 17,
      dataWidth = 32,
      sourceWidth = 0,
      contextWidth = 0,
      lengthWidth = 2
    )
  ).setDefinitionName("top"))
}