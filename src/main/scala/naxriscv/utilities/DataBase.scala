package naxriscv.utilities

import naxriscv.Global
import spinal.core.{BitCount, ScopeProperty}

import scala.collection.mutable

object ScopedThing{
  implicit def toValue[T](p : ScopedThing[T]) : T = p.get()

  class ThingIntPimper(p: ScopedThing[Int]) {
    def bits = BitCount(p.get())
  }
  implicit def thingIntPimperFunc(p: ScopedThing[Int]) : ThingIntPimper = new ThingIntPimper(p)
}

class ScopedThing[T](dataBase: ScopeProperty[DataBase]){
  def get() : T = dataBase.get.apply(this)
  def set(value : T) = dataBase.update(this, value)
}

class DataBase{
  val storage = mutable.LinkedHashMap[ScopedThing[_ <: Any], Any]()
  def update[T](key : ScopedThing[T], value : T) = storage.update(key, value)
  def apply[T](key : ScopedThing[T]) : T = storage.apply(key).asInstanceOf[T]
}

class DataBaseScope extends ScopeProperty[DataBase]{

}




//NaxRiscv elaboration assume it is running into a loaded NaxScope
object NaxScope extends DataBaseScope{
  def init(xlen : Int) = {
    Global.XLEN.set(xlen)
    Global.RVF.set(false)
    Global.RVD.set(false)
    Global.RV_DEBUG.set(false)
  }
}

//This can be used to globaly store anything in a NaxScope
class NaxThing[T] extends ScopedThing[T](NaxScope)

//The only purpose of this is to make usercode pretty <3
object NaxParameter {
  def apply[T] = new NaxThing[T]
}



