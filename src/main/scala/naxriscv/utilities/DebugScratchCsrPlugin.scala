// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.utilities

import naxriscv.Global.{COMMIT_COUNT, RVD, XLEN}
import naxriscv.{DecodeList, Global, ROB}
import naxriscv.execute.fpu.{FpuFlags, FpuFloatWriteback, FpuIntWriteback, FpuWriteback}
import naxriscv.execute.fpu.FpuWriteback.{FLOAT_FLAGS, FLOAT_FLAGS_ENABLE, FP_DIRTY, INT_FLAGS, INT_FLAGS_ENABLE}
import naxriscv.interfaces.{CommitService, CsrService, DecoderService, MicroOp, PrivilegedService, RegfileService, RobService, WakeRegFile, WakeRegFileService, WakeRob, WakeRobService}
import naxriscv.riscv.{CSR, FloatRegFile, IntRegFile, Rvfd}
import spinal.core._
import spinal.lib.{Flow, Stream, slave}

import scala.collection.mutable.ArrayBuffer



class DebugScratchCsrPlugin(val count : Int, val csrBase : Int = 0x800) extends Plugin {
  val setup = create early new Area{
    val csr = getService[CsrService]
    csr.retain()
  }

  val logic = create late new Area {
    val s = setup.get
    import s._

    val data = Vec.fill(count)(Reg(UInt(XLEN bits)))
    for((e, i) <- data.zipWithIndex){
      csr.write(e, csrBase+i)
    }
    csr.release()
  }
}

