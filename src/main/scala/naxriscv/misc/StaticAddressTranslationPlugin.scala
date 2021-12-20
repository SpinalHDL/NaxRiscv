package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.{AddressTranslationRsp, AddressTranslationService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.{Stage, Stageable}
import scala.collection.mutable.ArrayBuffer

case class StaticAddressTranslationParameter(rspAt : Int)

class StaticAddressTranslationPlugin(ioRange : UInt => Bool) extends Plugin with AddressTranslationService{
  override def preWidth = Global.XLEN.get
  override def postWidth = Global.XLEN.get
  override def withTranslation = true

  Global.PC_WIDTH.set(preWidth)

  case class Spec(stages: Seq[Stage], preAddress: Stageable[UInt], p: StaticAddressTranslationParameter, rsp : AddressTranslationRsp)
  val specs = ArrayBuffer[Spec]()

  override def newTranslationPort(stages: Seq[Stage], preAddress: Stageable[UInt], pAny: Any) = {
    val p = pAny.asInstanceOf[StaticAddressTranslationParameter]
    specs.addRet(new Spec(stages, preAddress, p, new AddressTranslationRsp(this, 0, stages(p.rspAt)){
      import rspStage._
      import keys._

      REDO := False
      TRANSLATED := preAddress
      IO := ioRange(TRANSLATED)
      ALLOW_EXECUTE := True
      ALLOW_READ := True
      ALLOW_WRITE := True
      PAGE_FAULT := False
      WAKER := 0
      WAKER_ANY := False
    })).rsp
  }


  override def wakerCount = 0
  override def wakes = B(0)

  val setup = create early new Area{

  }

  val logic = create late new Area{
    lock.await()

//    val ports = for(spec <- specs) yield new Area{
//      import spec._
//      val stage = stages(spec.p.rspAt)
//      import stage._
//
//      spec.rsp.TRANSLATED := spec.preAddress
//      spec.rsp.IO := ioRange(spec.rsp.TRANSLATED)
//    }
  }
}
