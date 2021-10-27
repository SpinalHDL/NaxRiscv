package naxriscv.utilities

import spinal.core._
import spinal.core.fiber.{Handle, Lock}
import naxriscv.pipeline._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}

trait Service{
  def uniqueIds : Seq[Any] = Nil
}

trait Plugin extends Area with Service{
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

  def getSubServices() : Seq[Service] = Nil

  def getServicesOf[T <: Service : ClassTag] : Seq[T] = framework.getServicesOf[T]
  def getService[T <: Service : ClassTag] : T = framework.getService[T]
  def getService[T <: Service : ClassTag](id : Any) : T = framework.getService[T](id)
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

  def getServicesOf[T <: Service : ClassTag] : Seq[T] = {
    val clazz = (classTag[T].runtimeClass)
    val filtered = services.filter(o => clazz.isAssignableFrom(o.getClass))
    filtered.asInstanceOf[Seq[T]]
  }
  def getService[T <: Service : ClassTag] : T = {
    val filtered = getServicesOf[T]
    assert(filtered.length == 1, s"??? ${classTag[T].runtimeClass.getName}")
    filtered.head.asInstanceOf[T]
  }
  def getService[T <: Service : ClassTag](id : Any) : T = getServiceWhere[T](_.uniqueIds.contains(id))

  def getServiceWhere[T: ClassTag](filter : T => Boolean) : T = {
    val clazz = (classTag[T].runtimeClass)
    val filtered = services.filter(o => clazz.isAssignableFrom(o.getClass) && filter(o.asInstanceOf[T]))
    assert(filtered.length == 1, s"??? ${clazz.getName}")
    filtered.head.asInstanceOf[T]
  }

  def getServices = services
}


