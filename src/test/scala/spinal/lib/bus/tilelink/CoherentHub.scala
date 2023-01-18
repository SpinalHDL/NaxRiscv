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

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object CoherentHubTesterUtils{
  def testInterconnect(cd : ClockDomain, upBuses : Seq[Bus], downBuses : Seq[Bus], orderings : List[Flow[CoherentHubOrdering]]) : Unit = {
    val mainMem = SparseMemory()
    val globalMem = SparseMemory(mainMem.seed)

    val blockSize = 64
    val ups = for(up <- upBuses) yield new Area{
      val agent = new MasterAgent(up, cd){
        override def onGrant(source: Int, address: Long, param: Int) = {
//          blocks(sourceToMaster(source) -> address) = Block(param)
        }

        override def probeBlock(source: Int,param: Int,  address: Long, bytes : Int) = {
//          def ok(param : Int) = probeAck(
//            param   = param,
//            source  = source,
//            address = address,
//            bytes   = bytes
//          )
//          blocks.get(sourceToMaster(source) -> address) match {
//            case Some(b) => b.cap < param match {
//              case false => ok(Param.Report.fromCap(b.cap))
//              case true  => b.retains match {
//                case 0 => ok(Param.Prune.fromTo(b.cap, param))
//                case _ => {
//                  b.probe match {
//                    case Some(x) => ???
//                    case None => b.probe = Some(Probe(source, param))
//                  }
//                }
//              }
//            }
//            case None => ok(Param.Report.NtoN)
//          }
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
          val map = ups(busId).agent.ordering.map
          map(source).apply()
          map(source) = null
        }
      }
    }




    for(r <- 0 until 8) {
      val busId = 0

      def acquireBlock(agent : MasterAgent,
                       source : Int,
                       param : Int,
                       address : Long,
                       bytes : Int): Block ={
        var ref: Array[Byte] = null
        val agent = ups(0).agent
        val block = agent.acquireBlock(source, param, address, bytes){
          ref = globalMem.readBytes(address, bytes)
        }
        block.ordering(globalMem.write(address, block.data))
        block.retain()
        println("*")
        println(block.data.mkString(" "))
        println(ref.mkString(" "))
        assert((block.data, ref).zipped.forall(_ == _))
        block
      }

      def get(agent : MasterAgent,
              source : Int,
              address : Long,
              bytes : Int): Unit ={
        var ref: Array[Byte] = null
        val data = ups(0).agent.get(source, address, bytes){
          ref = globalMem.readBytes(address, bytes)
        }
        println("*")
        println(data.mkString(" "))
        println(ref.mkString(" "))
        assert((data, ref).zipped.forall(_ == _))
      }
      def putFullData(agent : MasterAgent,
                      source : Int,
                      address : Long,
                      data : Array[Byte]): Unit ={
        assert(!ups(0).agent.putFullData(source, address, data){
          globalMem.write(address, data)
        })
      }
      def putPartialData(agent : MasterAgent,
                         source : Int,
                         address : Long,
                         data : Array[Byte],
                         mask : Array[Boolean]): Unit ={
        assert(!ups(0).agent.putPartialData(source, address, data, mask){
          globalMem.write(address, data, mask)
        })
      }

      def releaseData(agent : MasterAgent,
                      source : Int,
                      param : Int,
                      block : Block): Unit ={
        ups(0).agent.releaseData(source, param, block.address, block.data){
          globalMem.write(block.address, block.data)
        }
      }

//      for (i <- 0 until 1) {
//        acquireBlock(ups(busId).agent, 0, Param.Grow.NtoB, 0x1000 + i * 0x40, 0x40)
//        cd.waitSampling(10)
//      }

//      for (i <- 0 until 1) {
//        val data = Array.fill[Byte](0x40)(Random.nextInt().toByte)
//        acquireBlock(ups(busId).agent, 0, Param.Grow.NtoT, 0x1000 + i * 0x40, 0x40)
////        probeAckData(ups(busId).agent, 0, Param.Prune.TtoN, 0x1000 + i * 0x40, data)
//        cd.waitSampling(10)
//      }

      for (i <- 0 until 1) {
        val data = Array.fill[Byte](0x40)(Random.nextInt().toByte)
        val block = acquireBlock(ups(busId).agent, 0, Param.Grow.NtoT, 0x1000 + i * 0x40, 0x40)
        fork{
          cd.waitSamplingWhere(block.probe.nonEmpty)
          cd.waitSampling(Random.nextInt(100))
          block.dirty = true
          block.data(2) = (r+0x30).toByte
          block.release()
//          ups(busId).agent.block.changeBlockCap(0, 0x1000 + i * 0x40, Param.Cap.toN)
        }
        val block2 = acquireBlock(ups(busId).agent, 4, Param.Grow.NtoT, 0x1000 + i * 0x40, 0x40)
        block2.release()
//        releaseData(ups(busId).agent, 4, Param.Prune.TtoN, block)
        cd.waitSampling(10)
      }

      for (i <- 0 until 1) {
        get(ups(busId).agent, 0, 0x1000 + i * 0x40, 0x40)
        cd.waitSampling(10)
      }

      for (i <- 0 until 1) {
        val data = Array.fill[Byte](0x40)(Random.nextInt().toByte)
        putFullData(ups(busId).agent, 0, 0x1000 + i * 0x40, data)
        cd.waitSampling(10)
      }

      for (i <- 0 until 1) {
        val bytes = 0x40
        val data = Array.fill[Byte](bytes)(Random.nextInt().toByte)
        val mask = Array.fill[Boolean](bytes)(Random.nextInt(2).toBoolean)
        putPartialData(ups(busId).agent, 0, 0x1000 + i * 0x40, data, mask)
        cd.waitSampling(10)
      }
    }
  }
}

class CoherentHubTester extends AnyFunSuite {
  test("miaou"){
    SimConfig.withFstWave.compile(new CoherentHub(CoherentHubGen.basicConfig)).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(200000*10)
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