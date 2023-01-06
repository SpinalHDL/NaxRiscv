package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim._
import spinal.lib.sim.SparseMemory

object CoherentHubTesterUtils{
  def testInterconnect(cd : ClockDomain, upBuses : Seq[Bus], downBuses : Seq[Bus], orderings : List[Flow[CoherentHubOrdering]]) : Unit = {
    val mainMem = SparseMemory()
    val globalMem = SparseMemory(mainMem.seed)

    val ups = for(up <- upBuses) yield new Area{
      val orderingMap = Array.fill[() => Unit](1 << up.p.sourceWidth)(null)
      def ordering(source : Int)(body : => Unit) = orderingMap(source) = () => body
      val agent = new MasterAgent(up, cd){
        override def probeBlock(param: Int, source: Int, address: Long, bytes : Int) = {
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

    val busId = 0
    val source = 1
    val address = 0x1000
    val size = 0x40
    var ref : Array[Byte] = null
    ups(busId).ordering(source){
      ref = globalMem.readBytes(address, size)
    }
    val data = ups(0).agent.get(address, size, source)
//    println(data.mkString(" "))
//    println(ref.mkString(" "))
    assert((data,ref).zipped.forall(_ == _))

    cd.waitSampling(100)
  }
}
class CoherentHubTester extends AnyFunSuite {
  test("miaou"){
    SimConfig.withFstWave.compile(new CoherentHub(CoherentHubGen.basicConfig)).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(500*10)
      CoherentHubTesterUtils.testInterconnect(dut.clockDomain, dut.io.ups, List(dut.io.down), dut.io.ordering.all)
    }
  }
}
