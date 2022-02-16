package naxriscv.utilities

import naxriscv.Global
import spinal.core.{BitCount, ScopeProperty}

import scala.collection.mutable

class Thing[T]
class DataBase{
  val storage = mutable.LinkedHashMap[Thing[_ <: Any], Any]()
  def update[T](key : Thing[T], value : T) = storage.update(key, value)
  def apply[T](key : Thing[T]) : T = storage.apply(key).asInstanceOf[T]
}

object NaxThing{
  def apply[T] = new NaxThing[T]

  implicit def toValue[T](p : NaxThing[T]) : T = p.get()

  class NaxPropertyInt(p: NaxThing[Int]) {
    def bits = BitCount(p.get())
  }
  implicit def toBits(p: NaxThing[Int]) : NaxPropertyInt = new NaxPropertyInt(p)
}

class NaxThing[T] extends Thing[T]{
  def get() : T = NaxScope.get.apply(this)
  def set(value : T) = NaxScope.update(this, value)
}

object NaxParameter {
  def apply[T] = new NaxThing[T]
}

object NaxScope extends ScopeProperty[DataBase]{
  def create(xlen : Int) : DataBase = {
    this.set(new DataBase)
    Global.XLEN.set(xlen)
    this.get
  }
}

