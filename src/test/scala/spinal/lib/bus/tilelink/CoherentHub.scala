package spinal.lib.bus.tilelink

import naxriscv.utilities.AllocatorMultiPortMem
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.sim.SparseMemory

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object CoherentHubTesterUtils{
  def testInterconnect(cd : ClockDomain, upBuses : Seq[Bus], downBuses : Seq[Bus], orderings : List[Flow[CoherentHubOrdering]]) : Unit = {
    val mainMem = SparseMemory()
    val globalMem = SparseMemory(mainMem.seed)

    val ups = for(up <- upBuses) yield new Area{
      val orderingMap = Array.fill[() => Unit](1 << up.p.sourceWidth)(null)
      def ordering(source : Int)(body : => Unit) = orderingMap(source) = () => body
      def orderingFired(source : Int) = assert(!orderingMap.contains(source))
      val agent = new MasterAgent(up, cd){
        override def probeBlock(source: Int,param: Int,  address: Long, bytes : Int) = {
          probeAck(
            param   = Param.Report.NtoN,
            source  = source,
            address = address,
            bytes   = bytes
          )
        }
      }
    }


    val downs = for(down <- downBuses) yield new Area{
      val agent = new SlaveAgent(down, cd){
        override def onGet(source: Int, address: Long, bytes: Int) = {
          accessAckData(
            source = source,
            data = mainMem.readBytes(address, bytes)
          )
        }

        var  onPutPartialDataCounter = 0
        override def onPutPartialData(source: Int, address: Long, size: Int, mask: Array[Byte], data: Array[Byte]) = {
          val isLast = onPutPartialDataCounter == ((1 << size) >> down.p.dataBytesLog2Up)-1
          mainMem.write(address, data, mask)
          onPutPartialDataCounter += 1
          if(isLast){
            onPutPartialDataCounter = 0
            accessAck(
              source = source,
              size = size
            )
          }
        }
      }
    }

    cd.onSamplings{
      for (ordering <- orderings) {
        if(ordering.valid.toBoolean){
          val busId = ordering.upId.toInt
          val source = ordering.upSource.toInt
          val map = ups(busId).orderingMap
          map(source).apply()
          map(source) = null
        }
      }
    }


    for(_ <- 0 until 8) {
      val busId = 0
      val source = 1
      for (i <- 0 until 1) {
        val address = 0x1000 + i * 0x40
        val size = 0x40
        var ref: Array[Byte] = null
        ups(busId).ordering(source) {
          ref = globalMem.readBytes(address, size)
        }
        val data = ups(0).agent.get(source, address, size)
        ups(busId).orderingFired(source)
        println("*")
        println(data.mkString(" "))
        println(ref.mkString(" "))
        assert((data, ref).zipped.forall(_ == _))
        cd.waitSampling(10)
      }

      for (i <- 0 until 1) {
        val address = 0x1000 + i * 0x40
        val size = 0x40
        val data = new Array[Byte](size)
        Random.nextBytes(data)
        ups(busId).ordering(source) {
          globalMem.write(address, data)
        }
        val denied = ups(0).agent.putFullData(source, address, data)
        ups(busId).orderingFired(source)
        assert(!denied)
        cd.waitSampling(10)
      }

      for (i <- 0 until 1) {
        val address = 0x1000 + i * 0x40
        val size = 0x40
        val data = Array.fill[Byte](size)(Random.nextInt().toByte)
        val mask = Array.fill[Boolean](size)(Random.nextInt(2).toBoolean)
        ups(busId).ordering(source) {
          globalMem.write(address, data, mask)
        }
        val denied = ups(0).agent.putPartialData(source, address, data, mask)
        ups(busId).orderingFired(source)
        assert(!denied)
        cd.waitSampling(10)
      }
    }


  }
}
class CoherentHubTester extends AnyFunSuite {
  test("miaou"){
    SimConfig.withFstWave.compile(new CoherentHub(CoherentHubGen.basicConfig)).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000*10)
      CoherentHubTesterUtils.testInterconnect(dut.clockDomain, dut.io.ups, List(dut.io.downGet, dut.io.downPut), dut.io.ordering.all)
    }
  }
}

object CoherencyHubSynt extends App{
  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(new CoherentHub(CoherentHubGen.basicConfig))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}