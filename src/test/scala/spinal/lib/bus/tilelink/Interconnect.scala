package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}

import scala.collection.mutable.ArrayBuffer

case class M2sSupport(transfers : M2sTransfers,
                      dataWidth : Int,
                      allowExecute : Boolean){
  def mincover(that : M2sSupport): M2sSupport ={
    M2sSupport(
      transfers = transfers.mincover(that.transfers),
      dataWidth = that.dataWidth,
      allowExecute = this.allowExecute && that.allowExecute
    )
  }
}

class InterconnectNodeMode extends Nameable
object InterconnectNodeMode extends AreaRoot {
  val BOTH, MASTER, SLAVE = new InterconnectNodeMode
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
  }

  var mode = InterconnectNodeMode.BOTH
  def setSlaveOnly() = {mode = InterconnectNodeMode.SLAVE; this}
  def setMasterOnly() = {mode = InterconnectNodeMode.MASTER; this}
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

  def createSlave() = createNode().setSlaveOnly()
  def createMaster() = createNode().setMasterOnly()

  def connect(m : InterconnectNode, s : InterconnectNode): Unit ={
    val c = new InterconnectConnection(m,s)
    c.setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")
    connections += c
  }

  val gen = hardFork{
    lock.await()
    var error = false
    for(n <- nodes){
      n.mode match {
        case InterconnectNodeMode.MASTER =>
          if(n.ups.nonEmpty) { println(s"${n.getName()} has masters"); error = true }
          if(n.downs.isEmpty) { println(s"${n.getName()} has no slave"); error = true }
        case InterconnectNodeMode.BOTH =>
          if(n.ups.isEmpty) { println(s"${n.getName()} has no master"); error = true }
          if(n.downs.isEmpty) { println(s"${n.getName()} has no slave"); error = true }
        case InterconnectNodeMode.SLAVE =>
          if(n.ups.isEmpty) { println(s"${n.getName()} has no master"); error = true }
          if(n.downs.nonEmpty) { println(s"${n.getName()} has slaves"); error = true }
      }
    }
    if(error) SpinalError("Failed")

    for(n <- nodes) new Composite(n, "igen") {
      val dataBytes = hardFork{
        n.ups.size match {
          case 0 =>
          case _ => {
            soon(n.dataBytes)
            n.setDataBytes(n.ups.map(_.m.dataBytes.get).max)
          }
        }
      }

      val negociation = hardFork{
        if(n.ups.nonEmpty)   n.m2s.proposed load n.ups.map(_.m.m2s.proposed).reduce(_ mincover _)
        if(n.downs.nonEmpty) n.m2s.supported load n.downs.map(_.s.m2s.supported.get).reduce(_ mincover _)
      }

      val arbiter = hardFork{
        soon(n.m2s.parameters)
        soon(n.ups.map(_.arbiter.bus) :_*)
        n.ups.size match {
          case 0 =>
            soon(n.m2s.proposed)
            n.m2s.proposed load M2sSupport(
              transfers    = n.m2s.parameters.emits,
              dataWidth    = n.dataBytes.get,
              allowExecute = n.m2s.parameters.get.withExecute
            )
          case _ => {

            for(up <- n.ups){
              up.arbiter.s2m.parameters.loadAsync(n.s2m.parameters)
            }
            n.m2s.parameters load Arbiter.outputMastersFrom(
              n.ups.map(_.arbiter.m2s.parameters.get)
            )
            val arbiter = Arbiter(n.ups.map(up => NodeParameters(
              m = up.arbiter.m2s.parameters,
              s = up.arbiter.s2m.parameters,
              dataBytes = n.dataBytes
            )))
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
          case _ => {
            for(down <- n.downs){
              down.decoder.m2s.parameters.loadAsync(Decoder.outputMastersFrom(n.m2s.parameters, down.s.m2s.supported))
            }

            n.s2m.parameters.load(Decoder.inputSlavesFrom(n.downs.map(_.decoder.s2m.parameters.get)))
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
  val node = ic.createMaster()
  node.setDataBytes(4)
  node.m2s.parameters.load(
    M2sParameters(
      List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            get = SizeRange.upTo(0x40),
            putFull = SizeRange.upTo(0x40)
          ),
          addressRange = List(SizeMapping(0, 0x1000))
        ))
      ))
    )
  )
}

class VideoIn(ic : Interconnect) extends Area{
  val node = ic.createMaster()
  node.setDataBytes(4)
  node.m2s.parameters.load(
    M2sParameters(
      List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            putFull = SizeRange.upTo(0x40)
          ),
          addressRange = List(SizeMapping(0, 0x1000))
        ))
      ))
    )
  )
}

class VideoOut(ic : Interconnect) extends Area{
  val node = ic.createMaster()
  node.setDataBytes(4)
  node.m2s.parameters.load(
    M2sParameters(
      List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            get = SizeRange.upTo(0x40)
          ),
          addressRange = List(SizeMapping(0, 0x1000))
        ))
      ))
    )
  )
}



class Bridge(ic : Interconnect) extends Area{
  val node = ic.createNode()
//  node.m2s.supported.loadAsync(node.m2s.proposed)
}

class UART(ic : Interconnect) extends Area{
  val node = ic.createSlave()
  node.s2m.parameters.load(
    S2mParameters.simple(this)
  )
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

class ROM(ic : Interconnect) extends Area{
  val node = ic.createSlave()
  node.s2m.parameters.load(
    S2mParameters.simple(this)
  )
  node.m2s.supported.loadAsync(
    M2sSupport(
      transfers = node.m2s.proposed.transfers.intersect(
        M2sTransfers(
          get = SizeRange.upTo( 0x1000)
        )
      ),
      dataWidth = 4,
      allowExecute = false
    )
  )
}

class StreamOut(ic : Interconnect) extends Area{
  val node = ic.createSlave()
  node.s2m.parameters.load(
    S2mParameters.simple(this)
  )
  node.m2s.supported.loadAsync(
    M2sSupport(
      transfers = node.m2s.proposed.transfers.intersect(
        M2sTransfers(
          putFull = SizeRange.upTo( 0x1000)
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

    val bridgeA = new Bridge(interconnect)
    bridgeA.node.mapping.load(DefaultMapping)
    interconnect.connect(cpu0.node, bridgeA.node)
    interconnect.connect(cpu1.node, bridgeA.node)

    val bridgeB = new Bridge(interconnect)
    bridgeB.node.mapping.load(SizeMapping(0x100, 0x300))
    interconnect.connect(bridgeA.node, bridgeB.node)

    val videoIn0 = new VideoIn(interconnect)
    interconnect.connect(videoIn0.node, bridgeA.node)

    val videoOut0 = new VideoOut(interconnect)
    interconnect.connect(videoOut0.node, bridgeA.node)

    val uart0 = new UART(interconnect)
    interconnect.connect(bridgeB.node, uart0.node)
    uart0.node.mapping.load(SizeMapping(0x100, 0x100))

    val uart1 = new UART(interconnect)
    interconnect.connect(bridgeB.node, uart1.node)
    uart1.node.mapping.load(SizeMapping(0x200, 0x100))

    val bridgeC = new Bridge(interconnect)
    bridgeC.node.mapping.load(DefaultMapping)
    interconnect.connect(bridgeB.node, bridgeC.node)

    val bridgeD = new Bridge(interconnect)
    bridgeD.node.mapping.load(DefaultMapping)
    interconnect.connect(bridgeC.node, bridgeD.node)

    val rom0 = new ROM(interconnect)
    rom0.node.mapping.load(SizeMapping(0x300, 0x100))
    interconnect.connect(bridgeD.node, rom0.node)

    val streamOut = new StreamOut(interconnect)
    streamOut.node.mapping.load(SizeMapping(0x400, 0x100))
    interconnect.connect(bridgeD.node, streamOut.node)

//    val rom0 = new ROM(interconnect)
//    rom0.node.mapping.load(SizeMapping(0x300, 0x100))
//    interconnect.connect(cpu0.node, rom0.node)

//    val streamOut = new StreamOut(interconnect)
//    streamOut.node.mapping.load(SizeMapping(0x400, 0x100))
//    interconnect.connect(cpu0.node, streamOut.node)

//    interconnect.connect(cpu0.node, uart0.node)
//    interconnect.connect(cpu1.node, uart0.node)
//    interconnect.connect(cpu0.node, uart1.node)
//    interconnect.connect(cpu1.node, uart1.node)
//    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//    uart1.node.mapping.load(SizeMapping(0x200, 0x100))
  })
}
