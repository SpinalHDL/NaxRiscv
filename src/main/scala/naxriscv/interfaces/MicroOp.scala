package naxriscv.interfaces

import naxriscv.Global
import spinal.core.MaskedLiteral

class Resource
class Arg(mapping : Seq[Any]) extends Resource
class RfAccess
class RfRead extends RfAccess
class RfWrite extends RfAccess
case class RfResource(rf : RegfileSpec, access : RfAccess) extends Resource

object RS1 extends RfRead
object RS2 extends RfRead
object RS3 extends RfRead
object RD  extends RfWrite

class MicroOp(val resources : Seq[Resource])
case class SingleDecoding(key : MaskedLiteral, override val resources : Seq[Resource]) extends MicroOp(resources)
case class MultiDecoding(key : MaskedLiteral, uop : Seq[MicroOp])

trait RegfileSpec{
  def sizeArch : Int
  def width : Int
  def x0AlwaysZero : Boolean
  def getName() : String

  def ->(access : RfAccess) = RfResource(this, access)
}

