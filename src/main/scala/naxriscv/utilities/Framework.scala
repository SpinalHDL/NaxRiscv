package naxriscv.utilities

import spinal.core._
import spinal.core.fiber.{Handle, Lock}
import naxriscv.pipeline._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}


trait Plugin extends Area{
  this.setName(ClassName(this))

  val framework = Handle[Framework]()

  def create = new {
    def early[T](body : => T) : Handle[T] = {
      Handle{
        framework.buildLock.retain()
        val ret = framework.rework {
          body
        }
        framework.buildLock.release()
        ret
      }
    }
    def late[T](body : => T) : Handle[T] = {
      Handle{
        framework.buildLock.retain()
        val ret = framework.rework {
          framework.lateLock.await()
          body
        }
        framework.buildLock.release()
        ret
      }
    }
  }

  def getSubServices() : Seq[Any] = Nil

//  def getService[T](clazz : Class[T]) = framework.getService(clazz)
  def getService[T: ClassTag] : T = framework.getService[T]
}

class FrameworkConfig(){
  val plugins = ArrayBuffer[Plugin]()
}

class Framework(val plugins : Seq[Plugin]) extends Area{
  val lateLock = Lock()
  val buildLock = Lock()

  lateLock.retain()

  plugins.foreach(_.framework.load(this)) // Will schedule all plugins early tasks

  val lateUnlocker = Handle{
    //Will run after all plugins early tasks spawned
    lateLock.release()
  }

  val services = plugins ++ plugins.flatMap(_.getSubServices())

  def getService[T: ClassTag] : T = {
    val clazz = (classTag[T].runtimeClass)
    val filtered = services.filter(o => clazz.isAssignableFrom(o.getClass))
    assert(filtered.length == 1, s"??? ${clazz.getName}")
    filtered.head.asInstanceOf[T]
  }

  def getServiceWhere[T: ClassTag](filter : T => Boolean) : T = {
    val clazz = (classTag[T].runtimeClass)
    val filtered = services.filter(o => clazz.isAssignableFrom(o.getClass) && filter(o.asInstanceOf[T]))
    assert(filtered.length == 1, s"??? ${clazz.getName}")
    filtered.head.asInstanceOf[T]
  }

  def getServices = services
}


