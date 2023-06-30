// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.misc

import naxriscv.Global._
import naxriscv._
import naxriscv.interfaces.{AddressTranslationPortUsage, AddressTranslationRsp, AddressTranslationService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.{Stage, Stageable}

import scala.collection.mutable.ArrayBuffer

case class StaticAddressTranslationParameter(rspAt : Int)

class StaticAddressTranslationPlugin( var physicalWidth : Int,
                                      var ioRange : UInt => Bool,
                                      var fetchRange : UInt => Bool) extends Plugin with AddressTranslationService{
  override def withTranslation = true

  create config {
    PHYSICAL_WIDTH.set(physicalWidth)
    VIRTUAL_WIDTH.set((PHYSICAL_WIDTH.get + 1) min XLEN.get)
    VIRTUAL_EXT_WIDTH.set(VIRTUAL_WIDTH.get +  (VIRTUAL_WIDTH < XLEN).toInt)
    PC_WIDTH.set(VIRTUAL_EXT_WIDTH)
    TVAL_WIDTH.set(VIRTUAL_EXT_WIDTH)
  }
  case class Spec(stages: Seq[Stage], preAddress: Stageable[UInt], p: StaticAddressTranslationParameter, rsp : AddressTranslationRsp)
  val specs = ArrayBuffer[Spec]()
  override def newStorage(pAny: Any) : Any = "dummy <3"


  override def invalidatePort = setup.invalidatePort

  override def newTranslationPort(stages: Seq[Stage],
                         preAddress: Stageable[UInt],
                         allowRefill : Stageable[Bool],
                         usage: AddressTranslationPortUsage,
                         portSpec: Any,
                         storageSpec: Any): AddressTranslationRsp = {
    val p = portSpec.asInstanceOf[StaticAddressTranslationParameter]
    specs.addRet(new Spec(stages, preAddress, p, new AddressTranslationRsp(this, 0, stages(p.rspAt), wayCount = 0){
      import rspStage._
      import keys._

      REDO := False
      TRANSLATED := preAddress
      IO := ioRange(TRANSLATED)
      ALLOW_EXECUTE := True
      ALLOW_READ := True
      ALLOW_WRITE := True
      PAGE_FAULT := False
      ACCESS_FAULT := False
      wake := True

      ALLOW_EXECUTE clearWhen(!fetchRange(TRANSLATED))
      pipelineLock.release()
    })).rsp
  }

  val setup = create early new Area{
    val invalidatePort = FlowCmdRsp().setIdleAll()
    invalidatePort.rsp.valid setWhen(RegNext(invalidatePort.cmd.valid) init(False))
  }

  val logic = create late new Area{
    lock.await()
  }
}
