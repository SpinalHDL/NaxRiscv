package naxriscv.units

import naxriscv.{Frontend, Global}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.core.fiber.Handle
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(euId : String, staticLatency : Boolean) extends Plugin with WakeRobService with WakeRegFileService { //TODO sharing of non static wakes ?
  withPrefix(euId)
  val SEL = Stageable(Bool())

  def completionAt : Int = writeBackAt
  def writeBackAt : Int

  override def wakeRobs = if(!staticLatency) List(logic.wake.rob) else Nil
  override def wakeRegFile = if(!staticLatency) List(logic.wake.rf) else Nil

  class Setup extends Area {
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    def add(microOp: MicroOp, srcKeys: List[SrcKeys], decoding: eu.DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.setStaticCompletion(microOp, completionAt)
      if (staticLatency && microOp.resources.exists{
        case RfResource(_, RD) => true
        case _ => false
      }) eu.setStaticWake(microOp, writeBackAt)
      eu.addDecoding(microOp, decoding :+ (SEL -> True))
      if (srcKeys.nonEmpty) {
        getService[SrcPlugin](euId).specify(microOp, srcKeys)
      }
    }

    eu.setDecodingDefault(SEL, False)
  }

  class Logic extends Area with PostInitCallback{
    val eu = getService[ExecutionUnitBase](euId)
    val decode = getService[DecoderService]

    val wbStage = eu.getExecute(writeBackAt)
    val wb = eu.newWriteback(IntRegFile, RD, wbStage, if(staticLatency) 0 else 1)
    wb.valid := wbStage(SEL)

    val wake = !staticLatency generate new Area{
      val fire = wbStage.isFireing && wbStage(SEL)
      val rob = Flow(WakeRob())
      val rf = Flow(WakeRegFile(decode.PHYS_RD, needBypass = false))

      rob.valid := fire
      rob.robId := wbStage(ExecutionUnitKeys.ROB_ID)

      rf.valid := fire && wbStage(decode.WRITE_RD)
      rf.physical := wbStage(decode.PHYS_RD)
    }
    override def postInitCallback() = {eu.release() ; this}
  }

  def setup : Handle[_ <: Setup]
  def logic : Handle[_ <: Logic]
}