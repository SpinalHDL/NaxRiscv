package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Opcode extends AreaRoot{
  val A = new SpinalEnum{
    val PUT_FULL_DATA, PUT_PARTIAL_DATA, GET ,ACQUIRE_BLOCK, ACQUIRE_PERM = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      GET -> 4,
      PUT_FULL_DATA -> 0,
      PUT_PARTIAL_DATA -> 1,
      ACQUIRE_BLOCK -> 6,
      ACQUIRE_PERM -> 7
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
    val PROBE_ACK, PROBE_ACK_DATA, RELEASE, RELEASE_DATA = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      PROBE_ACK     -> 4,
      PROBE_ACK_DATA -> 5,
      RELEASE       -> 6,
      RELEASE_DATA  -> 7
    )
  }

  val D = new SpinalEnum{
    val ACCESS_ACK, ACCESS_ACK_DATA, GRANT, GRANT_DATA, RELEASE_ACK = newElement()
    defaultEncoding = SpinalEnumEncoding("enc")(
      ACCESS_ACK      -> 0,
      ACCESS_ACK_DATA -> 1,
      GRANT       -> 4,
      GRANT_DATA  -> 5,
      RELEASE_ACK -> 6
    )
  }
}

object Param{
  val Cap = new Area {
    val toT = 0
    val toB = 1
    val toN = 2
  }
  val Prune = new Area {
    val TtoB = 0
    val TtoN = 1
    val BtoN = 2
  }
  val Report = new Area {
    val TtoT = 3
    val BtoB = 4
    val NtoN = 5
  }
  val Grow = new Area {
    val NtoB = 0
    val NtoT = 1
    val BtoT = 2
  }
}





class BusFragment(val p : BusParameter, val withData : Boolean) extends Bundle {
  val size    = p.size()
}

case class ChannelA(override val p : BusParameter) extends BusFragment(p, p.withDataA) {
  val opcode  = Opcode.A()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val mask    = p.withDataA generate p.mask()
  val data    = p.withDataA generate p.data()
  val corrupt = p.withDataA generate Bool()
}
case class ChannelB(override val p : BusParameter) extends BusFragment(p, p.withDataB) {
  val opcode  = Opcode.B()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val mask    = p.withDataB generate p.mask()
  val data    = p.withDataB generate p.data()
  val corrupt = p.withDataB generate Bool()
}
case class ChannelC(override val p : BusParameter) extends BusFragment(p, true) {
  val opcode  = Opcode.C()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val data    = p.data()
  val corrupt = Bool()
  def isProbeKind()   = opcode === Opcode.C.PROBE_ACK || opcode === Opcode.C.PROBE_ACK_DATA
  def isReleaseKind() = opcode === Opcode.C.RELEASE   || opcode === Opcode.C.RELEASE_DATA
}
case class ChannelD(override val p : BusParameter) extends BusFragment(p, p.withDataD) {
  val opcode  = Opcode.D()
  val param   = Bits(3 bits)
  val source  = p.source()
  val sink    = p.sink()
  val denied  = p.withDataD generate Bool()
  val data    = p.withDataD generate p.data()
  val corrupt = p.withDataD generate Bool()
}
case class ChannelE(p : BusParameter) extends Bundle {
  val sink    = p.sink()
}

case class Bus(p : BusParameter) extends Bundle with IMasterSlave{
  val a = Stream(ChannelA(p))
  val b = p.withBCE generate Stream(ChannelB(p))
  val c = p.withBCE generate Stream(ChannelC(p))
  val d = Stream(ChannelD(p))
  val e = p.withBCE generate Stream(ChannelE(p))

  override def asMaster() = {
    master(a, c, e)
    slave(b, d)
  }

  def <<(m : Bus): Unit ={
    a << m.a
    d >> m.d
    if(p.withBCE){
      b >> m.b
      c << m.c
      e << m.e
    }
  }

  def withSourceOffset(offset : Int, width: Int): Bus ={
    val ret = Bus(p.copy(sourceWidth = width))
    ret << this
    ret.a.source.removeAssignments() := (this.a.source | offset).resized
    this.d.source.removeAssignments() := ret.d.source.resized
    if(p.withBCE){
      this.b.source.removeAssignments() := ret.b.source.resized
      ret.c.source.removeAssignments() := (this.c.source | offset).resized
    }
    ret
  }
}
