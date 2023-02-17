package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}

import scala.collection.mutable.ArrayBuffer

//case class MasterSupport(transfers : SlaveTransfers){
//  def mincover(that : MasterSupport): MasterSupport ={
//    that.copy(
//      transfers = transfers.mincover(that.transfers)
//    )
//  }
//}

case class SlaveSupport(transfers : M2sTransfers,
                        allowExecute : Boolean){
  def mincover(that : SlaveSupport): SlaveSupport ={
    that.copy(
      transfers = transfers.mincover(that.transfers),
      allowExecute = allowExecute || that.allowExecute
    )
  }

  def filter(m : M2sAgent) = m.copy()
}

case class M2sSupport(transfers : M2sTransfers,
                      dataWidth : Int,
                      allowExecute : Boolean){
  def mincover(that : M2sSupport): M2sSupport ={
    that.copy(
      transfers = transfers.mincover(that.transfers)
    )
  }
}

class InterconnectNode() extends Area {
  val bus = Handle[Bus]()
  val ups = ArrayBuffer[InterconnectConnection]()
  val downs = ArrayBuffer[InterconnectConnection]()
  val lock = Lock()
  val mapping = Handle[AddressMapping]()

  val dataBytes = Handle[Int]()
  def setDataBytes(bytes : Int) = this.dataBytes.load(bytes)

  val m2s = new Area{
    val proposed = Handle[M2sSupport]()
    val supported = Handle[M2sSupport]()
    val parameters = Handle[M2sParameters]()
  }
  val s2m = new Area{
    val parameters = Handle[S2mParameters]()
    parameters.load(    S2mParameters(
      List(S2mAgent(
        name = this,
        sinkId = SizeMapping(0,0),
        emits = S2mTransfers()
      ))
    )) //TODO remove
//    val support = Handle[SlaveSupport]()
  }
}

class InterconnectConnection(val m : InterconnectNode, val s : InterconnectNode) extends Area {
  m.downs += this
  s.ups += this
  def mapping = s.mapping

  val decoder, arbiter = new Area{
    val bus = Handle[Bus]()
    val m2s = new Area{
      val parameters = Handle[M2sParameters]()
    }
    val s2m = new Area{
      val parameters = Handle[S2mParameters]()
      parameters load S2mParameters( //TODO remove
        List(S2mAgent(
          name = this,
          sinkId = SizeMapping(0,0),
          emits = S2mTransfers()
        ))
      )
    }
  }

  val m2s = hardFork(new Area{
    soon(arbiter.m2s.parameters)
    arbiter.m2s.parameters.load(decoder.m2s.parameters)
  })

  val s2m = hardFork(new Area{
    soon(decoder.s2m.parameters)
    decoder.s2m.parameters.load(arbiter.s2m.parameters)
  })

  val logic = hardFork(new Area{
    decoder.bus >> arbiter.bus
  })
}

class Interconnect extends Area{
  val lock = Lock()
  val nodes = ArrayBuffer[InterconnectNode]()
  val connections = ArrayBuffer[InterconnectConnection]()
  def createNode(): InterconnectNode ={
    val node = new InterconnectNode()
    nodes += node
    node
  }

  def connect(m : InterconnectNode, s : InterconnectNode): Unit ={
    val c = new InterconnectConnection(m,s)
    c.setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")
    connections += c
  }

  val gen = hardFork{
    lock.await()

    for(n <- nodes) new Composite(n, "igen") {
//      val s2m = hardFork{
//        soon(n.slave.support)
//        n.downs.size match {
//          case 0 =>
//          case _ => {
//            n.slave.support.load{
//              n.downs.map(_.s.slave.support.get).reduce((a,b) => a.mincover(b))
//            }
//          }
//        }
//      }
//
//      val m2s = hardFork{
//        soon(n.master.support)
//        n.ups.size match {
//          case 0 =>
//          case _ => {
//            n.master.support.load{
//              n.ups.map(_.m.master.support.get).reduce((a,b) => a.mincover(b))
//            }
//          }
//        }
//      }

      val dataBytes = hardFork{
        n.ups.size match {
          case 0 =>
          case _ => {
            soon(n.dataBytes)
            n.setDataBytes(n.ups.map(_.m.dataBytes.get).max)
          }
        }
      }

      val arbiter = hardFork{
        soon(n.m2s.parameters)
        soon(n.m2s.proposed)
        soon(n.ups.map(_.arbiter.bus) :_*)
        n.ups.size match {
          case 0 =>
//          case 1 => {
//            n.master.parameters.load(n.ups(0).m.master.parameters)
//            n.bus << n.ups(0).mBus
//          }
          case _ => {
//            for(up <- n.ups){
//              up.arbiter.s2m.parameters.loadAsync(n.slave.parameters)
//            }
            n.m2s.proposed load M2sSupport(
              transfers    = n.ups.map(_.m.m2s.parameters.emits).reduce(_ mincover _),
              dataWidth    = n.ups.map(_.m.dataBytes.get).max,
              allowExecute = n.ups.exists(_.m.m2s.parameters.get.withExecute)
            )
            n.m2s.parameters.load(
              Arbiter.outputMastersFrom(n.ups.map(_.arbiter.m2s.parameters.get))
            )
            val arbiter = Arbiter(n.ups.map(_.m.bus.p.node))
            arbiter.setCompositeName(n.bus, "arbiter")
            (n.ups, arbiter.io.inputs.map(_.fromCombStage())).zipped.foreach(_.arbiter.bus.load(_))
            n.bus << arbiter.io.output
          }
        }
      }

      val decoder = hardFork{
        soon(n.s2m.parameters)
        soon(n.downs.map(_.decoder.bus) :_*)
        n.downs.size match {
          case 0 =>
//          case 1 => {
//            n.slave.parameters.load(n.downs(0).s.slave.parameters)
//            ???
//          }
          case _ => {
            for(down <- n.downs){
              down.decoder.m2s.parameters.loadAsync(Decoder.outputMastersFrom(n.m2s.parameters, down.s.m2s.supported))
            }

//            n.slave.parameters.load(Decoder.inputSlavesFrom(n.downs.map(_.decoder.s2m.parameters.get))) //TODO
            val decoder = Decoder(n.bus.p.node, n.downs.map(_.s.m2s.supported), n.downs.map(_.mapping))
            decoder.setCompositeName(n.bus, "decoder")
            (n.downs, decoder.io.outputs.map(_.combStage())).zipped.foreach(_.decoder.bus.load(_))
            decoder.io.input << n.bus
          }
        }
      }

      val busGen = hardFork{
        soon(n.bus)
        val p = NodeParameters(n.m2s.parameters, n.s2m.parameters, n.dataBytes).toBusParameter()
        n.bus.load(Bus(p))
      }
    }
  }
}

class CPU(ic : Interconnect) extends Area{
  val node = ic.createNode()
  node.setDataBytes(4)
  node.m2s.parameters.load(
    M2sParameters(
      List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            get = SizeRange.upTo(0x40),
            putFull = SizeRange.upTo( 0x40)
          ),
          addressRange = List(SizeMapping(0, 0x1000))
        ))
      ))
    )
  )
//  node.s2m.support.load(
//    MasterSupport(
//      transfers = S2mTransfers()
//    )
//  )
}

class Bridge(ic : Interconnect) extends Area{
  val node = ic.createNode()
  node.m2s.supported.loadAsync(node.m2s.proposed)
}

class UART(ic : Interconnect) extends Area{
  val node = ic.createNode()
//  node.s2m.parameters.load(
//    S2mParameters(
//      List(S2mAgent(
//        name = this,
//        sinkId = SizeMapping(0,0),
//        emits = S2mTransfers()
//      ))
//    )
//  )
  node.m2s.supported.loadAsync(
    M2sSupport(
      transfers = node.m2s.proposed.transfers.intersect(
        M2sTransfers(
          get = SizeRange.upTo( 0x1000),
          putFull = SizeRange.upTo(0x1000)
        )
      ),
      dataWidth = 4,
      allowExecute = false
    )
  )
}


object TopGen extends App{
  SpinalVerilog(new Component{
    val interconnect = new Interconnect()
    val cpu0 = new CPU(interconnect)
    val cpu1 = new CPU(interconnect)
    val uart0 = new UART(interconnect)
    val uart1 = new UART(interconnect)
    val bridgeA = new Bridge(interconnect)
    val bridgeB = new Bridge(interconnect)
    interconnect.connect(cpu0.node, bridgeA.node)
    interconnect.connect(cpu1.node, bridgeA.node)
    interconnect.connect(bridgeA.node, bridgeB.node)
    interconnect.connect(bridgeB.node, uart0.node)
    interconnect.connect(bridgeB.node, uart1.node)
    bridgeA.node.mapping.load(DefaultMapping)
    bridgeB.node.mapping.load(SizeMapping(0x100, 0x300))
    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
    uart1.node.mapping.load(SizeMapping(0x200, 0x100))

//    interconnect.connect(cpu0.node, uart0.node)
//    interconnect.connect(cpu1.node, uart0.node)
//    interconnect.connect(cpu0.node, uart1.node)
//    interconnect.connect(cpu1.node, uart1.node)
//    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//    uart1.node.mapping.load(SizeMapping(0x200, 0x100))
  })
}
