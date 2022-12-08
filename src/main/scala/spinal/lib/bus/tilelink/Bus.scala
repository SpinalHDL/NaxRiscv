package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._

object Opcode extends AreaRoot{
  val A = new SpinalEnum{
    val ACQUIRE_BLOCK, ACQUIRE_PERM = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      ACQUIRE_BLOCK -> 6,
      ACQUIRE_PERM  -> 7
    )
  }

  val B = new SpinalEnum{
    val PROBE_BLOCK, PROBE_PERM = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      PROBE_BLOCK -> 6,
      PROBE_PERM  -> 7
    )
  }

  val C = new SpinalEnum{
    val PROBE_ACK, PROBEACK_DATA, RELEASE, RELEASE_DATA = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      PROBE_ACK     -> 4,
      PROBEACK_DATA -> 5,
      RELEASE       -> 6,
      RELEASE_DATA  -> 7
    )
  }

  val D = new SpinalEnum{
    val GRANT, GRANT_DATA, RELEASE_ACK = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      GRANT       -> 4,
      GRANT_DATA  -> 5,
      RELEASE_ACK -> 6
    )
  }
}



case class BusParameter(addressWidth  : Int,
                        dataWidth     : Int,
                        burstBytesMax : Int,
                        sourceWidth   : Int,
                        sinkWidth     : Int){
  val sizeMax       = log2Up(burstBytesMax)
  val sizeWidth     = log2Up(burstBytesMax+1)
  def address()     = UInt(addressWidth bits)
  def data()        = Bits(dataWidth bits)
  def mask()        = Bits(dataWidth/8 bits)
  def source()      = UInt(sourceWidth bits)
  def sink()        = UInt(sinkWidth bits)
  def size()        = UInt(sizeWidth bits)
  val A = new {
    def withData = false
  }
  val B = new {
    def withData = false
  }
}

case class ChannelA(p : BusParameter) extends Bundle {
  val opcode  = Opcode.A()
  val param   = Bits(3 bits)
  val size    = p.size()
  val source  = p.source()
  val address = p.address()
  val mask    = p.A.withData generate p.mask()
  val data    = p.A.withData generate p.data()
  val corrupt = p.A.withData generate Bool()
}
case class ChannelB(p : BusParameter) extends Bundle {
  val opcode  = Opcode.B()
  val param   = Bits(3 bits)
  val size    = p.size()
  val source  = p.source()
  val address = p.address()
  val mask    = p.B.withData generate p.mask()
  val data    = p.B.withData generate p.data()
  val corrupt = p.B.withData generate Bool()
}
case class ChannelC(p : BusParameter) extends Bundle {
  val opcode  = Opcode.C()
  val param   = Bits(3 bits)
  val size    = p.size()
  val source  = p.source()
  val address = p.address()
  val data    = p.data()
  val corrupt = Bool()
}
case class ChannelD(p : BusParameter) extends Bundle {
  val opcode  = Opcode.D()
  val param   = Bits(3 bits)
  val size    = p.size()
  val source  = p.source()
  val sink    = p.sink()
  val denied  = Bool()
  val data    = p.data()
  val corrupt = Bool()
}
case class ChannelE(p : BusParameter) extends Bundle {
  val sink    = p.sink()
}

case class Bus(p : BusParameter) extends Bundle with IMasterSlave{
  val A = Stream(ChannelA(p))
  val B = Stream(ChannelB(p))
  val C = Stream(ChannelC(p))
  val D = Stream(ChannelD(p))
  val E = Stream(ChannelE(p))

  override def asMaster() = {
    master(A, C, E)
    slave(B, D)
  }
}
