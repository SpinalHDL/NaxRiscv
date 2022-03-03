package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.{AddressTranslationPortUsage, AddressTranslationRsp, AddressTranslationService, PulseHandshake}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.{Stage, Stageable}

import scala.collection.mutable.ArrayBuffer

case class StaticAddressTranslationParameter(rspAt : Int)

class StaticAddressTranslationPlugin(var ioRange : UInt => Bool,
                                     var fetchRange : UInt => Bool) extends Plugin with AddressTranslationService{
  override def preWidth = Global.XLEN.get
  override def postWidth = Global.XLEN.get
  override def withTranslation = true

  create config {
    Global.PC_WIDTH.set(preWidth)
    Global.PC_TRANSLATED_WIDTH.set(postWidth)
  }
  case class Spec(stages: Seq[Stage], preAddress: Stageable[UInt], p: StaticAddressTranslationParameter, rsp : AddressTranslationRsp)
  val specs = ArrayBuffer[Spec]()
  override def newStorage(pAny: Any) : Any = "dummy <3"


  override def invalidatePort = setup.invalidatePort

  override def newTranslationPort(stages: Seq[Stage],
                         preAddress: Stageable[UInt],
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
      wake := True

      ALLOW_EXECUTE clearWhen(!fetchRange(TRANSLATED))
      pipelineLock.release()
    })).rsp
  }

  val setup = create early new Area{
    val invalidatePort = PulseHandshake(NoData).idle()
    invalidatePort.served setWhen(RegNext(invalidatePort.request) init(False))
  }

  val logic = create late new Area{
    lock.await()
  }
}
