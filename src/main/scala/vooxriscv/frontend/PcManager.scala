package vooxriscv.frontend

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import vooxriscv.pipeline._

import scala.collection.mutable.ArrayBuffer

trait JumpService{
  def createJumpInterface(stage : Stage, priority : Int = 0) : Flow[UInt] //High priority win
}


class PcManager(resetVector : BigInt) extends FrontendPlugin with JumpService{
  case class JumpInfo(interface :  Flow[UInt], stage: Stage, priority : Int)
  val jumpInfos = ArrayBuffer[JumpInfo]()
  override def createJumpInterface(stage: Stage, priority : Int = 0): Flow[UInt] = {
    assert(stage != null)
    val interface = Flow(UInt(32 bits))
    jumpInfos += JumpInfo(interface,stage, priority)
    interface
  }


  val logic = create late{
    val s0 = frontend.s0
    import s0._
    import Frontend._

    val jump = new Area {
      val sortedByStage = jumpInfos.sortWith((a, b) => {
        (frontend.precedenceOf(b.stage, a.stage)) ||
          ((a.stage == b.stage) && a.priority > b.priority)
      })
      val valids = sortedByStage.map(_.interface.valid)
      val pcs = sortedByStage.map(_.interface.payload)

      val pcLoad = Flow(PC)
      pcLoad.valid := jumpInfos.map(_.interface.valid).orR
      pcLoad.payload := MuxOH(OHMasking.first(valids.asBits), pcs)
    }

    val fetchPc = new Area{
      //PC calculation without Jump
      val output = Stream(UInt(32 bits))
      val pcReg = Reg(UInt(32 bits)) init(resetVector) addAttribute(Verilator.public)
      val correction = False
      val correctionReg = RegInit(False) setWhen(correction) clearWhen(output.fire)
      val corrected = correction || correctionReg
      val pcRegPropagate = False
      val booted = RegNext(True) init (False)
      val inc = RegInit(False) clearWhen(correction || pcRegPropagate) setWhen(output.fire) clearWhen(!output.valid && output.ready)
      val pc = pcReg + (inc ## B"00").asUInt


      val flushed = False

      if(RVC) when(inc) {
        pc(1) := False
      }

      when(jump.pcLoad.valid) {
        correction := True
        pc := jump.pcLoad.payload
        flushed := True
      }

      when(booted && (output.ready || correction || pcRegPropagate)){
        pcReg := pc
      }

      pc(0) := False
      if(!RVC) pc(1) := False

      val fetcherHalt = False
      output.valid := !fetcherHalt && booted
      output.payload := pc
    }

    PC := fetchPc.output.payload
  }
}
