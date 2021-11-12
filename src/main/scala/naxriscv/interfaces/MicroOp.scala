package naxriscv.interfaces

import naxriscv.Global
import spinal.core.{AreaObject, MaskedLiteral, Nameable}

class Resource
class Arg(mapping : Seq[Any]) extends Resource
class RfAccess extends Nameable
class RfRead extends RfAccess
class RfWrite extends RfAccess
case class RfResource(rf : RegfileSpec, access : RfAccess) extends Resource

object RS1 extends RfRead with AreaObject
object RS2 extends RfRead with AreaObject
object RS3 extends RfRead with AreaObject
object RD  extends RfWrite with AreaObject

class MicroOp(val resources : Seq[Resource])
case class SingleDecoding(key : MaskedLiteral, override val resources : Seq[Resource]) extends MicroOp(resources)
case class MultiDecoding(key : MaskedLiteral, uop : Seq[MicroOp])

trait RegfileSpec extends Nameable{
  def sizeArch : Int
  def width : Int
  def x0AlwaysZero : Boolean
  def getName() : String

  def ->(access : RfAccess) = RfResource(this, access)
}

