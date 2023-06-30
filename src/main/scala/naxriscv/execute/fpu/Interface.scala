// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute.fpu

import spinal.core._
import spinal.lib._

//https://slideplayer.fr/slide/13631868/83/images/49/e%3D0+et+m%E2%89%A00+%EF%83%A8+N+est+d%C3%A9normalis%C3%A9.jpga
object FloatMode extends SpinalEnum{
  val ZERO, INF, NAN, NORMAL = newElement()
}



case class FloatUnpacked(exponentMax : Int,
                         exponentMin : Int,
                         mantissaWidth: Int) extends Bundle{
  val mode = FloatMode()
  val quiet = Bool() // if mode is NAN
  val sign = Bool()
  val exponent = new AFix(exponentMax, exponentMin, 0)
  val mantissa = AFix.U(0 exp, -mantissaWidth exp)

  def isNan = mode === FloatMode.NAN
  def isNormal = mode === FloatMode.NORMAL
  def isZero = mode === FloatMode.ZERO
  def isInfinity = mode === FloatMode.INF
  def isNanSignaling = isNan && !quiet

  def setNormal    = mode := FloatMode.NORMAL
  def setZero      = mode := FloatMode.ZERO
  def setInfinity  = mode := FloatMode.INF
  def setNan       = { mode := FloatMode.NAN; quiet := False }
  def setNanQuiet  = { mode := FloatMode.NAN; quiet := True }

  def invert(enable : Bool) = {
    val ret = cloneOf(this)
    ret := this
    ret.sign.removeAssignments() := this.sign ^ enable
    ret
  }
}

case class FpuParameter(rvd : Boolean,
                        rv64 : Boolean,
                        robIdWidth : Int,
                        portCount : Int,
                        withAdd : Boolean = true,
                        withMul : Boolean = true,
                        withDiv : Boolean = true,
                        withSqrt : Boolean = true){
  val rsFloatWidth = 32 + rvd.toInt*32
  val rsIntWidth = 32 + rv64.toInt*32
  val exponentWidth = if(rvd) 11 else 8
  val mantissaWidth = if(rvd) 52 else 23
}

object FpuOpcode extends SpinalEnum{
  val MUL, ADD, FMA, I2F, F2I, CMP, DIV, SQRT, MIN_MAX, SGNJ, FMV_X_W, FMV_W_X, FCLASS, FCVT_X_X = newElement()
}

object FpuFormat extends SpinalEnum{
  val FLOAT, DOUBLE = newElement()
}

object FpuRoundMode extends SpinalEnum(){
  val RNE, RTZ, RDN, RUP, RMM = newElement()
  defaultEncoding = SpinalEnumEncoding("opt")(
    RNE -> 0,
    RTZ -> 1,
    RDN -> 2,
    RUP -> 3,
    RMM -> 4
  )
}
object FpuRoundModeInstr extends SpinalEnum(){
  val RNE, RTZ, RDN, RUP, RMM, DYN = newElement()
  defaultEncoding = SpinalEnumEncoding("opt")(
    RNE -> 0,
    RTZ -> 1,
    RDN -> 2,
    RUP -> 3,
    RMM -> 4,
    DYN -> 7
  )
}

case class FpuFloatCmd(rvd : Boolean, robIdWidth : Int, withRs : Boolean = true) extends Bundle {
  val opcode = FpuOpcode()
  val arg = Bits(2 bits)
  val rs = withRs generate Vec.fill(3)(Bits((if(rvd) 64 else 32) bits))
  val format = FpuFormat()
  val roundMode = FpuRoundMode()
  val robId = UInt(robIdWidth bits)
//  val scheduleId = UInt(robIdWidth bits)

  def withoutRs() : FpuFloatCmd = {
    val ret = FpuFloatCmd(rvd, robIdWidth, false)
    ret.assignSomeByName(this)
    ret
  }
}

case class FpuFlags() extends Bundle{
  val NX,  UF,  OF,  DZ,  NV = Bool()
  def clear(): Unit ={
    List(NX,  UF,  OF,  DZ,  NV).foreach(_ := False)
  }
}

case class FpuFloatWriteback(robIdWidth : Int, valueWidth : Int) extends Bundle{
  val robId = UInt(robIdWidth bits)
  val flags = FpuFlags()
  val value = Bits(valueWidth bits)
}

case class FpuFloatWake(robIdWidth : Int) extends Bundle{
  val robId = UInt(robIdWidth bits)
}

case class FpuIntCmd(rv64 : Boolean, robIdWidth : Int) extends Bundle {
  val opcode = FpuOpcode()
  val arg = Bits(2 bits)
  val rs1 = Bits(32+rv64.toInt*32 bits)
  val format = FpuFormat()
  val roundMode = FpuRoundMode()
  val robId = UInt(robIdWidth bits)
}



case class FpuIntWriteback(robIdWidth : Int, rsIntWidth : Int) extends Bundle{
  val flags = FpuFlags()
  val robId = UInt(robIdWidth bits)
  val value = Bits(rsIntWidth bits)
}


case class FpuPort(p : FpuParameter) extends Bundle with IMasterSlave {
  val floatCmd       = Stream(FpuFloatCmd(p.rvd, p.robIdWidth))
  val floatWriteback = Flow(FpuFloatWriteback(p.robIdWidth, p.rsFloatWidth))
  val floatWake      = Flow(FpuFloatWake(p.robIdWidth))
  val intCmd         = Stream(FpuIntCmd(p.rv64, p.robIdWidth))
  val intWriteback   = Stream(FpuIntWriteback(p.robIdWidth, p.rsIntWidth))
  val unschedule     = Bool()

  override def asMaster(): Unit = {
    master(floatCmd, intCmd)
    slave(floatWake, floatWriteback, intWriteback)
    out(unschedule)
  }
}
