# SPDX-FileCopyrightText: 2023 NaxRiscv
#
# SPDX-License-Identifier: MIT

package naxriscv.utilities

import naxriscv.Global
import spinal.core.fiber.Handle
import spinal.core.{Area, BitCount, ScopeProperty}
import spinal.lib.generator_backup.Handle.initImplicit

import scala.collection.mutable



class DataBaseScope extends ScopeProperty[DataBase]


class DataBase{
  val storage = mutable.LinkedHashMap[ScopedThing[_ <: Any], Any]()
  def update[T](key : ScopedThing[T], value : T) = storage.update(key, value)
  def apply[T](key : ScopedThing[T]) : T = storage.apply(key).asInstanceOf[T]
  def getElseUpdate[T](key : ScopedThing[T], create : =>  T) : T = storage.getOrElseUpdate(key, create).asInstanceOf[T]
}

class ScopedThing[T](dataBase: ScopeProperty[DataBase]){
  def get : T = dataBase.get.apply(this)
  def set(value : T) = dataBase.update(this, value)
}

class ScopedHandle[T](dataBase: ScopeProperty[DataBase]) extends Area{
  val thing = new ScopedThing[Handle[T]](dataBase)
  var defaultLanda : Option[() => T] = None
  def setDefault(v : => T): this.type = { defaultLanda = Some(() => v) ; this }
  def getHandle() : Handle[T] = dataBase.get.getElseUpdate(thing, new Handle[T].setCompositeName(this))
  def get : T = {
    val h = getHandle()
    if(!h.isLoaded && defaultLanda.nonEmpty) h.load(defaultLanda.get())
    h.get
  }
  def set(value : T) = getHandle.load(value)
}


//A few implicit to make things smooth
object ScopedThing{
  implicit def toValue[T](p : ScopedThing[T]) : T = p.get

  class ThingIntPimper(p: ScopedThing[Int]) {
    def bits = BitCount(p.get)
  }
  implicit def thingIntPimperFunc(p: ScopedThing[Int]) : ThingIntPimper = new ThingIntPimper(p)
}
object ScopedHandle{
  implicit def toValue[T](p : ScopedHandle[T]) : T = p.get

  class ScopedHandleIntPimper(p: ScopedHandle[Int]) {
    def bits = BitCount(p.get)
  }
  implicit def thingIntPimperFunc(p: ScopedHandle[Int]) : ScopedHandleIntPimper = new ScopedHandleIntPimper(p)
}


//NaxRiscv elaboration assume it is running into a loaded NaxScope
object NaxScope extends DataBaseScope{

}

//This can be used to globaly store anything in a NaxScope
class NaxThing[T] extends ScopedHandle[T](NaxScope)

//The only purpose of this is to make usercode pretty <3
object NaxParameter {
  def apply[T] = new NaxThing[T]
}



