package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}

import scala.collection.mutable.ArrayBuffer

case class MasterSupport(transfers : SlaveTransfers){
  def mincover(that : MasterSupport): MasterSupport ={
    that.copy(
      transfers = transfers.mincover(that.transfers)
    )
  }
}

case class SlaveSupport(transfers : MasterTransfers,
                        allowExecute : Boolean){
  def mincover(that : SlaveSupport): SlaveSupport ={
    that.copy(
      transfers = transfers.mincover(that.transfers),
      allowExecute = allowExecute || that.allowExecute
    )
  }

  def filter(m : MasterParameters) = m.copy()
}

case class M2sSupport(dataWidth : Int, emit : MasterTransfers, allowExecute : Boolean)

class InterconnectNode() extends Area {
  val bus = Handle[Bus]()
  val ups = ArrayBuffer[InterconnectConnection]()
  val downs = ArrayBuffer[InterconnectConnection]()
  val lock = Lock()
  val mapping = Handle[AddressMapping]()

  val dataBytes = Handle[Int]()
  def setDataBytes(bytes : Int) = this.dataBytes.load(bytes)

  val master = new Area{
    val parameters = Handle[MastersParameters]()
    val support = Handle[MasterSupport]()
  }
  val slave = new Area{
    val parameters = Handle[SlavesParameters]()
    val support = Handle[SlaveSupport]()
  }
}

class InterconnectConnection(val m : InterconnectNode, val s : InterconnectNode) extends Area {
  m.downs += this
  s.ups += this
  def mapping = s.mapping

  val decoder, arbiter = new Area{
    val bus = Handle[Bus]()
    val master = new Area{
      val parameters = Handle[MastersParameters]()
//      val support = Handle[MasterSupport]()
    }
    val slave = new Area{
      val parameters = Handle[SlavesParameters]()
//      val support = Handle[SlaveSupport]()
    }
  }

  val m2s = hardFork(new Area{
    soon(arbiter.master.parameters)
    arbiter.master.parameters.load(decoder.master.parameters)
  })

  val s2m = hardFork(new Area{
    soon(decoder.slave.parameters)
    decoder.slave.parameters.load(arbiter.slave.parameters)
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
      val s2m = hardFork{
        soon(n.slave.support)
        n.downs.size match {
          case 0 =>
          case _ => {
            n.slave.support.load{
              n.downs.map(_.s.slave.support.get).reduce((a,b) => a.mincover(b))
            }
          }
        }
      }

      val m2s = hardFork{
        soon(n.master.support)
        n.ups.size match {
          case 0 =>
          case _ => {
            n.master.support.load{
              n.ups.map(_.m.master.support.get).reduce((a,b) => a.mincover(b))
            }
          }
        }
      }

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
        soon(n.master.parameters)
        soon(n.ups.map(_.arbiter.bus) :_*)
        n.ups.size match {
          case 0 =>
//          case 1 => {
//            n.master.parameters.load(n.ups(0).m.master.parameters)
//            n.bus << n.ups(0).mBus
//          }
          case _ => {
            for(up <- n.ups){
              up.arbiter.slave.parameters.loadAsync(n.slave.parameters)
            }
            n.master.parameters.load(
              Arbiter.outputMastersFrom(n.ups.map(_.arbiter.master.parameters.get))
            )
            val arbiter = Arbiter(n.ups.map(_.m.bus.p.node))
            arbiter.setCompositeName(n.bus, "arbiter")
            (n.ups, arbiter.io.inputs.map(_.fromCombStage())).zipped.foreach(_.arbiter.bus.load(_))
            n.bus << arbiter.io.output
          }
        }
      }

      val decoder = hardFork{
        soon(n.slave.parameters)
        soon(n.downs.map(_.decoder.bus) :_*)
        n.downs.size match {
          case 0 =>
//          case 1 => {
//            n.slave.parameters.load(n.downs(0).s.slave.parameters)
//            ???
//          }
          case _ => {
            for(down <- n.downs){
              down.decoder.master.parameters.loadAsync(Decoder.outputMastersFrom(n.master.parameters, down.s.slave.support))
            }
            n.slave.parameters.load(Decoder.inputSlavesFrom(n.downs.map(_.decoder.slave.parameters.get)))
            val decoder = Decoder(n.bus.p.node, n.downs.map(_.s.slave.support), n.downs.map(_.mapping))
            decoder.setCompositeName(n.bus, "decoder")
            (n.downs, decoder.io.outputs.map(_.combStage())).zipped.foreach(_.decoder.bus.load(_))
            decoder.io.input << n.bus
          }
        }
      }

      val busGen = hardFork{
        soon(n.bus)
        val p = NodeParameters(n.master.parameters, n.slave.parameters, n.dataBytes).toBusParameter()
        n.bus.load(Bus(p))
      }
    }
  }
}

class CPU(ic : Interconnect) extends Area{
  val node = ic.createNode()
  node.setDataBytes(4)
  node.master.parameters.load(
    MastersParameters(
      List(MasterParameters(
        name = this,
        mapping = List(MasterSource(
          id = SizeMapping(0, 4),
          emits = MasterTransfers(
            get = SizeRange.upTo(0x40),
            putFull = SizeRange.upTo( 0x40)
          ),
          addressRange = List(SizeMapping(0, 0x1000))
        ))
      ))
    )
  )
  node.master.support.load(
    MasterSupport(
      transfers = SlaveTransfers()
    )
  )
}

//class Bridge(ic : Interconnect) extends Area{
//  val up = ic.createNode()
//  val down = ic.createNode()
//  down.setMasterRequirements(up.masterRequirements)
//  down.setMasterCapabilities(up.masterCapabilities)
//  up.setSlaveRequirements(down.slaveRequirements)
//}

class UART(ic : Interconnect) extends Area{
  val node = ic.createNode()
  node.slave.parameters.load(
    SlavesParameters(
      List(SlaveParameters(
        name = this,
        sinkId = SizeMapping(0,0),
        emits = SlaveTransfers()
      ))
    )
  )
  node.slave.support.load(
    SlaveSupport(
      transfers = MasterTransfers(
        get = SizeRange.upTo( 0x1000),
        putFull = SizeRange.upTo(0x1000)
      ),
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
    val nodeA = interconnect.createNode()
    val nodeB = interconnect.createNode()
    interconnect.connect(cpu0.node, nodeA)
    interconnect.connect(cpu1.node, nodeA)
    interconnect.connect(nodeA, nodeB)
    interconnect.connect(nodeB, uart0.node)
    interconnect.connect(nodeB, uart1.node)
    nodeA.mapping.load(DefaultMapping)
    nodeB.mapping.load(SizeMapping(0x100, 0x300))
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
