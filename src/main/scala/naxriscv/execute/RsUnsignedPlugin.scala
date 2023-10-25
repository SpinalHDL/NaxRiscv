// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.execute

import naxriscv.DecodeList
import naxriscv.Global._
import naxriscv.interfaces._
import naxriscv.riscv._
import naxriscv.utilities.{DivRadix4, MulSpliter, Plugin}
import spinal.core._
import spinal.lib.pipeline.Stageable


object RsUnsignedPlugin extends AreaObject {
  val SIGNED = Stageable(Bool())
  val IS_W = Stageable(Bool())
  val RS1_REVERT, RS2_REVERT = Stageable(Bool())
  val RS1_FORMATED, RS2_FORMATED = Stageable(Bits(XLEN bits))
  val RS1_UNSIGNED, RS2_UNSIGNED = Stageable(UInt(XLEN bits))
}

class RsUnsignedPlugin(val euId : String) extends Plugin{
  import RsUnsignedPlugin._
  withPrefix(euId)

  val setup = create early new Area{
    val eu = getServicesOf[ExecutionUnitBase].find(_.euId == euId).get
    eu.retain()
  }

  val logic = create late new Area{
    val eu = getServicesOf[ExecutionUnitBase].find(_.euId == euId).get
    val stage = eu.getExecute(0)
    import stage._

    val rs1 = stage(eu(IntRegFile, RS1))
    val rs2 = stage(eu(IntRegFile, RS2))

    RS1_FORMATED := CombInit(rs1)
    RS2_FORMATED := CombInit(rs2)

    if (XLEN.get == 64) when(IS_W) {
      RS1_FORMATED(63 downto 32) := (default -> (SIGNED && rs1(31)))
      RS2_FORMATED(63 downto 32) := (default -> (SIGNED && rs2(31)))
    }

    RS1_REVERT := SIGNED && RS1_FORMATED.msb
    RS2_REVERT := SIGNED && RS2_FORMATED.msb

    def twoComplement(that: Bits, enable: Bool): UInt = (Mux(enable, ~that, that).asUInt + enable.asUInt)
    RS1_UNSIGNED := twoComplement(RS1_FORMATED, RS1_REVERT)
    RS2_UNSIGNED := twoComplement(RS2_FORMATED, RS2_REVERT)

    eu.release()
  }
}
