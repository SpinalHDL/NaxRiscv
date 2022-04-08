package naxriscv.debug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag._


class JtagTunnel(ctrl : JtagTapInstructionCtrl, instructionWidth : Int) extends Area with JtagTapFunctions{
  val shiftBuffer = Reg(Bits(instructionWidth + 7 + 1 bits))
  val instruction = Reg(Bits(instructionWidth bits))

  val counter = Reg(UInt(2 bits))

  val sendCapture = False
  val sendShift   = False
  val sendUpdate  = False

  when(ctrl.reset){
    instruction := 0
  }

  when(ctrl.enable){
    when(ctrl.capture){
      sendCapture := True
      counter := 0
    }
    when(ctrl.shift){
      shiftBuffer := (ctrl.tdi ## shiftBuffer) >> 1
      when(counter =/= 3) {
        counter := counter + 1
      } otherwise {
        sendShift := True
      }
    }
    when(ctrl.update){
      when(!shiftBuffer.msb){
        instruction := shiftBuffer.resized
      } otherwise {
        sendUpdate := True
      }
    }
  }

  val tdoBuffer = RegNext(False) //Don't ask me why XDXD
  ctrl.tdo := tdoBuffer

  def map(userCtrl : JtagTapInstructionCtrl, instructionId : Int): Unit ={
    val hit = instruction === instructionId
    userCtrl.tdi     := ctrl.tdi
    userCtrl.enable  := hit
    userCtrl.capture := hit && sendCapture
    userCtrl.shift   := hit && sendShift
    userCtrl.update  := hit && sendUpdate
    userCtrl.reset   := ctrl.reset
    when(hit) { tdoBuffer := userCtrl.tdo }
  }

  override def idcode(value: Bits)(instructionId: Int) = ???
  override def read[T <: Data](data: T, light : Boolean = false)(instructionId: Int) = {
    ???
  }
  override def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) = {
    ???
  }
  override def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int) = {
    ???
  }
  override def readAndWrite[T<: Data](captureData: T, updateData: T, captureReady: Bool, updateValid:Bool)(instructionId: Int) = {
    val area = new JtagTapInstructionReadWrite(captureData, updateData, captureReady)
    map(area.ctrl, instructionId)
    updateValid := area.ctrl.enable && area.ctrl.update
    val counter = Reg(UInt(log2Up(widthOf(captureData) + 1) bits))
    when(area.ctrl.capture){
      counter := 0
    }
    when(counter =/= widthOf(captureData)){
      when(area.ctrl.shift) {
        counter := counter + 1
      }
    } otherwise {
      area.ctrl.shift := False
    }
    area
  }

  override def isUpdating(instructionId: Int) = instruction === instructionId && sendUpdate
  override def isCapturing(instructionId: Int) = instruction === instructionId && sendCapture
  override def isReseting() = ctrl.reset
}