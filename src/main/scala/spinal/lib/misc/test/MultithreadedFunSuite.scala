package spinal.lib.misc.test

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite

import java.io.OutputStream
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object HeavyLock

class MultithreadedFunSuite(threadCount : Int) extends AnyFunSuite {
  val finalThreadCount = if(threadCount > 0) threadCount else {
    new oshi.SystemInfo().getHardware.getProcessor.getLogicalProcessorCount
  }
  implicit val ec = ExecutionContext.fromExecutorService(
    new ForkJoinPool(finalThreadCount, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)
  )
  class Job(body : => Unit){
    val originalOutput = Console.out
    val buffer = mutable.Queue[Char]()
    var bufferEnabled = true
    def redirector() = new OutputStream{
      override def write(i: Int): Unit = synchronized {
        if(bufferEnabled) buffer += i.toChar
        else originalOutput.print(i.toChar)
      }
    }
    val future = Future{
      Console.withOut(redirector()){
        Console.withErr(redirector())(body)
      }
    }

    def join(): Unit = {
      Thread.sleep(50)
      synchronized{
        bufferEnabled = false
        buffer.foreach(originalOutput.print)
      }
      Await.result(future, Duration.Inf)
    }
  }

  def testMp(testName: String, testTags: Tag*)(testFun: => Unit) {
    val job = new Job(testFun)
    super.test(testName, testTags :_*)(job.join())
  }
  protected def testSingleThread(testName: String, testTags: Tag*)(testFun: => Unit) {
    super.test(testName, testTags :_*)(testFun)
  }
}
