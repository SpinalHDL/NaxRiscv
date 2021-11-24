package naxriscv.units

import naxriscv.{Frontend, Global}
import naxriscv.interfaces.{MicroOp, RS1, RS2}
import naxriscv.riscv.{IMM, IntRegFile}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.pipeline.Stageable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SrcStageables extends AreaObject {
  val SRC1, SRC2 = Stageable(SInt(Global.XLEN bits))
  val ADD_SUB = Stageable(SInt(Global.XLEN bits))
  val LESS = Stageable(Bool())
  val REVERT, ZERO, UNSIGNED = Stageable(Bool())
}

class SrcKeys
class Src1Keys extends SrcKeys
class Src2Keys extends SrcKeys
class OpKeys   extends SrcKeys
object SrcKeys extends AreaObject {
  val Op = new Area{
    val ADD = new OpKeys
    val SUB = new OpKeys
    val SRC1 = new OpKeys
    val LESS = new OpKeys
    val LESS_U = new OpKeys
  }
  val SRC1 = new Area{
    val RF = new Src1Keys
    val U  = new Src1Keys
  }
  val SRC2 = new Area{
    val RF = new Src2Keys
    val I  = new Src2Keys
    val S  = new Src2Keys
    val PC = new Src2Keys
  }
}

class SrcPlugin(euId : String) extends Plugin{
  withPrefix(euId)
  override def uniqueIds = List(euId)


  val spec = mutable.LinkedHashMap[MicroOp, mutable.LinkedHashSet[SrcKeys]]()
  def specify(microOp: MicroOp, keys: List[SrcKeys]) = {
    val e = spec.getOrElseUpdate(microOp, mutable.LinkedHashSet[SrcKeys]())
    for(k <- keys){
      assert(!e.contains(k))
      e += k
    }
  }

  val setup = create early new Area{
    val eu = getService[ExecutionUnitBase](euId)
    eu.retain()
  }

  val logic = create late new Area{
    val eu = getService[ExecutionUnitBase](euId)
    val ss = SrcStageables
    val sk = SrcKeys

    val keys = spec.flatMap(_._2).toSeq.distinctLinked
    val opKeys   = keys.filter(_.isInstanceOf[OpKeys]).toSeq
    val src1Keys = keys.filter(_.isInstanceOf[Src1Keys]).toSeq
    val src2Keys = keys.filter(_.isInstanceOf[Src2Keys]).toSeq

    val SRC1_CTRL = Stageable(Bits(log2Up(src1Keys.size) bits))
    val SRC2_CTRL = Stageable(Bits(log2Up(src2Keys.size) bits))

    val src1ToEnum = src1Keys.zipWithIndex.map{case(k,i) => k -> B(i, widthOf(SRC1_CTRL) bits)}.toMap
    val src2ToEnum = src2Keys.zipWithIndex.map{case(k,i) => k -> B(i, widthOf(SRC2_CTRL) bits)}.toMap

    def has(keys : SrcKeys*) = keys.exists(keys.contains)

    for((op, keys) <- spec){

      val REVERT, ZERO = Stageable(Bool())
      eu.addDecoding(
        op,
        keys.toSeq.flatMap{
          case sk.Op.SRC1     => List(ss.REVERT -> False, ss.ZERO   -> True)
          case sk.Op.ADD      => List(ss.REVERT -> False, ss.ZERO   -> False)
          case sk.Op.SUB      => List(ss.REVERT -> True,  ss.ZERO   -> False)
          case sk.Op.LESS     => List(ss.REVERT -> True,  ss.ZERO   -> False, ss.UNSIGNED -> False)
          case sk.Op.LESS_U   => List(ss.REVERT -> True,  ss.ZERO   -> False, ss.UNSIGNED -> True)
          case key : Src1Keys => List(SRC1_CTRL -> src1ToEnum(key))
          case key : Src2Keys => List(SRC2_CTRL -> src2ToEnum(key))
        }
      )
    }

    val src = new Area{
      val stage = eu.getExecute(0)
      import stage._

      val imm = new IMM(Frontend.MICRO_OP)
      ss.SRC1 := SRC1_CTRL.muxListDc[SInt](src1Keys.map {
        case sk.SRC1.RF => src1ToEnum(sk.SRC1.RF) -> S(stage(eu(IntRegFile, RS1)))
        case sk.SRC1.U  => src1ToEnum(sk.SRC1.U ) -> S(imm.u)
      })

      ss.SRC2 := SRC2_CTRL.muxListDc[SInt](src2Keys.map {
        case sk.SRC2.RF => src2ToEnum(sk.SRC2.RF) -> S(stage(eu(IntRegFile, RS2)))
        case sk.SRC2.I  => src2ToEnum(sk.SRC2.I ) -> imm.i_sext
        case sk.SRC2.S  => src2ToEnum(sk.SRC2.S ) -> imm.s_sext
        case sk.SRC2.PC => src2ToEnum(sk.SRC2.PC) -> S(stage(Global.PC))
      })
    }

    val addsub = opKeys.nonEmpty generate new Area{
      val stage = eu.getExecute(0)
      import stage._

      val alwaysAdd = !has(sk.Op.SUB, sk.Op.LESS, sk.Op.LESS_U)
      val alwaysSub = !has(sk.Op.ADD)
      val withRevert = !alwaysAdd && !alwaysSub
      def carryIn(that: SInt) =
        if      (alwaysSub)  that + 1
        else if (withRevert) that + S(U(ss.REVERT, Global.XLEN bits))
        else                 that

      def ifElseMap[T](cond : Boolean)(value : T)(body : T => T) : T = if(cond) value else body(value)

      val rs2Patched =  CombInit(ifElseMap(!alwaysSub)(stage(ss.SRC2))(~_))
      if(withRevert) when(ss.REVERT){ rs2Patched :=  ~ss.SRC2  }
      if(has(sk.Op.SRC1)) when(ss.ZERO){ rs2Patched := 0 }
      ss.ADD_SUB := carryIn(ss.SRC1 + rs2Patched)

      // SLT, SLTU, branches
      if(has(sk.Op.LESS, sk.Op.LESS_U)) {
        ss.LESS := (ss.SRC1.msb === ss.SRC2.msb) ? ss.ADD_SUB.msb | Mux(ss.UNSIGNED, ss.SRC2.msb, ss.SRC1.msb)
      }
    }
    eu.release()
  }
}
