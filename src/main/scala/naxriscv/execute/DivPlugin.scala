// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute

import naxriscv.DecodeList
import naxriscv.Global._
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.{DivRadix4, MulSpliter}
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable


object DivPlugin extends AreaObject {
  val REM = Stageable(Bool())
  val DIV_RESULT = Stageable(Bits(XLEN bits))
}

class DivPlugin(val euId : String,
                var writebackAt : Int,
                var cmdAt : Int = 0,
                var rspAt : Int = 1,
                var splitWidthA : Int = 16,
                var splitWidthB : Int = 16) extends ExecutionUnitElementSimple(euId, staticLatency = false) {
  import DivPlugin._
  import RsUnsignedPlugin._

  override def euWritebackAt = writebackAt

  override val setup = create early new Setup{
    getServiceOption[PrivilegedService].foreach(_.addMisa('M'))

    add(Rvi.DIV , List(), DecodeList(REM -> False, RS1_SIGNED -> True,  RS2_SIGNED -> True))
    add(Rvi.DIVU, List(), DecodeList(REM -> False, RS1_SIGNED -> False, RS2_SIGNED -> False))
    add(Rvi.REM , List(), DecodeList(REM -> True , RS1_SIGNED -> True,  RS2_SIGNED -> True))
    add(Rvi.REMU, List(), DecodeList(REM -> True , RS1_SIGNED -> False, RS2_SIGNED -> False))

    if(XLEN.get == 64){
      add(Rvi.DIVW , List(), DecodeList(REM -> False, RS1_SIGNED -> True,  RS2_SIGNED -> True))
      add(Rvi.DIVUW, List(), DecodeList(REM -> False, RS1_SIGNED -> False, RS2_SIGNED -> False))
      add(Rvi.REMW , List(), DecodeList(REM -> True , RS1_SIGNED -> True,  RS2_SIGNED -> True))
      add(Rvi.REMUW, List(), DecodeList(REM -> True , RS1_SIGNED -> False, RS2_SIGNED -> False))

      for(op <- List(Rvi.DIVW , Rvi.DIVUW, Rvi.REMW , Rvi.REMUW)){
        signExtend(op, 31)
        eu.addDecoding(op, DecodeList(IS_W -> True))
      }

      for(op <- List(Rvi.DIV , Rvi.DIVU, Rvi.REM , Rvi.REMU)){
        eu.addDecoding(op, DecodeList(IS_W -> False))
      }
    }
  }

  override val logic = create late new Logic{
    val splits = MulSpliter(XLEN.get + 1, XLEN.get+ 1, splitWidthA, splitWidthB, true, true)
    val finalWidth = XLEN*2+2
    val sumSplitAt = splits.size/2

    val keys = new AreaRoot{
      val DIV_REVERT_RESULT = Stageable(Bool())
    }
    import keys._

    val div = DivRadix4(width = XLEN.get)
    def twoComplement(that: Bits, enable: Bool): UInt = (Mux(enable, ~that, that).asUInt + enable.asUInt)

    val feed = new ExecuteArea(cmdAt) {
      import stage._

      DIV_REVERT_RESULT := (RS1_REVERT ^ (RS2_REVERT && !REM)) && !(RS2_FORMATED === 0 && RS2_SIGNED && !REM) //RS2_SIGNED == RS1_SIGNED anyway

      val cmdSent = RegInit(False) setWhen (div.io.cmd.fire) clearWhen (isReady || isFlushed)
      div.io.cmd.valid := isValid && SEL && !cmdSent
      div.io.cmd.a := RS1_UNSIGNED.resized
      div.io.cmd.b := RS2_UNSIGNED.resized
      div.io.flush := isFlushed // Quite some assumption here

      if(cmdAt != writebackAt){
        haltIt(isValid && SEL && !cmdSent)
      }
    }

    val rsp = new ExecuteArea(rspAt){
      import stage._

      div.io.rsp.ready := isReady
      haltIt(isValid && SEL && !div.io.rsp.valid)

      val selected = REM ? div.io.rsp.remain otherwise div.io.rsp.result

      DIV_RESULT := twoComplement(B(selected), DIV_REVERT_RESULT).asBits.resized
    }

    val writeback = new ExecuteArea(writebackAt){
      import stage._

      wb.payload := DIV_RESULT
    }
  }
}
