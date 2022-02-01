package naxriscv.execute

import naxriscv.{DecodeListType, Frontend, Global, ROB}
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.core.fiber.Handle
import spinal.idslplugin.PostInitCallback
import spinal.lib.Flow
import spinal.lib.pipeline.Stageable

//This is a simple skeleton to ease the implementation of simple ExecutionUnit elements. It assume a single writeback and a single completion
abstract class ExecutionUnitElementSimple(euId : String, staticLatency : Boolean) extends Plugin { //TODO sharing of non static wakes ?
  withPrefix(euId)
  val SEL = Stageable(Bool())

  def euCompletionAt : Int = euWritebackAt
  def euWritebackAt : Int

  class Setup extends Area {
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()

    def add(microOp: MicroOp, srcKeys: List[SrcKeys], decoding: DecodeListType) = {
      eu.addMicroOp(microOp)
      eu.setStaticCompletion(microOp, euCompletionAt)
      if (staticLatency && microOp.resources.exists{
        case RfResource(_, RD) => true
        case _ => false
      }) eu.setStaticWake(microOp, euWritebackAt)
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

    val wbStage = eu.getExecute(euWritebackAt)
    val wb = eu.newWriteback(IntRegFile, RD, wbStage, if(staticLatency) 0 else 1)
    wb.valid := wbStage(SEL)

    val wake = !staticLatency generate new Area{
      val wakeRobsSel    = eu.newWakeRobsSelAt(euWritebackAt)
      val wakeRegFileSel = eu.newWakeRegFileSelAt(euWritebackAt)

      wakeRobsSel    := wbStage(SEL)
      wakeRegFileSel := wbStage(SEL)
    }

    override def postInitCallback() = {eu.release() ; this}

    class ExecuteArea(id : Int) extends Area{
      val stage = eu.getExecute(id)
    }
  }

  def setup : Handle[_ <: Setup]
  def logic : Handle[_ <: Logic]
}