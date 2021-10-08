package vooxriscv.frontend

import org.scalatest.tools.Framework
import spinal.core._
import spinal.core.fiber.{Handle, Lock}
import vooxriscv.pipeline._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PC_WIDTH extends ScopeProperty[Int]{
  def craft() = UInt(get bits)
}

object Frontend extends Area{
  this.setName("FETCH")

  val PC = Stageable(PC_WIDTH.craft())
  val RVC = ScopeProperty[Boolean]
}

trait FrontendPlugin{
  val frontend = Handle[Frontend]()

  def create = new {
    def early[T](body : => T) : Handle[T] = {
      Handle{
        frontend.buildLock.retain()
        val ret = frontend.rework {
          body
        }
        frontend.buildLock.release()
        ret
      }
    }
    def late[T](body : => T) : Handle[T] = {
      Handle{
        frontend.buildLock.retain()
        val ret = frontend.rework {
          frontend.lateLock.await()
          body
        }
        frontend.buildLock.release()
        ret
      }
    }
  }
}

class FrontendConfig(){
  val plugins = ArrayBuffer[FrontendPlugin]()
}

class Frontend(config : FrontendConfig) extends Pipeline{
  val s0, s1, s2, s3 = new Stage()

  import Connection._
  connect(s0, s1)(M2S(flushPreserveInput = true))
  connect(s1, s2)(M2S())
  connect(s2, s3)(M2S())

  val lateLock = Lock()
  val buildLock = Lock()

  lateLock.retain()
  buildLock.retain()

  config.plugins.foreach(_.frontend.load(this)) // Will schedule all plugins early tasks

  val lateUnlocker = Handle{
    //Will run after all plugins early tasks spawned
    lateLock.release()
    buildLock.release()
  }


  val logic = Handle{
    buildLock.await()
    this.build()
  }

  def getService[T](clazz : Class[T]) = {
    val filtered = config.plugins.filter(o => clazz.isAssignableFrom(o.getClass))
    assert(filtered.length == 1, s"??? ${clazz.getName}")
    filtered.head.asInstanceOf[T]
  }
}
object FrontendPlay extends App{
  SpinalVerilog(new Component {
    val config = new FrontendConfig
    config.plugins += new DecoderPlugin
    config.plugins += new AluPlugin
    val frontend = new Frontend(config)
  })
}

class AluPlugin extends FrontendPlugin{

  val setup = create early new Area{
    val decoder = frontend.getService(classOf[DecoderPlugin])
    decoder.lock.retain()
    println("a1")
  }

  val logic = create late new Area{
    println("a2")

     frontend.getService(classOf[DecoderPlugin]).lock.release()
  }
}

class DecoderPlugin extends FrontendPlugin{
  val lock = Lock()

  val setup = create early new Area{
    println("b1")
  }

  val logic = create late new Area{
    println("b2")
    lock.await()
    println("b3")
  }
}