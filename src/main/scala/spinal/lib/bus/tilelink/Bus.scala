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
    def fromTo(from : Int, to : Int) = (from, to) match {
      case (Cap.toT, Cap.toB) => TtoB
      case (Cap.toT, Cap.toN) => TtoN
      case (Cap.toB, Cap.toN) => BtoN
    }
    val TtoB = 0
    val TtoN = 1
    val BtoN = 2
  }
  val Report = new Area {
    def fromCap(cap : Int) = cap match {
      case Cap.toT => TtoT
      case Cap.toB => BtoB
      case Cap.toN => NtoN
    }
    val TtoT = 3
    val BtoB = 4
    val NtoN = 5
  }
  val Grow = new Area {
    val NtoB = 0
    val NtoT = 1
    val BtoT = 2
  }

  def reportPruneToCap(param : Int) = param match {
    case Prune.TtoB  => Cap.toB
    case Prune.TtoN  => Cap.toN
    case Prune.BtoN  => Cap.toN
    case Report.TtoT => Cap.toT
    case Report.BtoB => Cap.toB
    case Report.NtoN => Cap.toN
  }
  def reportPruneToCap(from : Int, to : Int) = (from, to) match {
    case (Cap.toT, Cap.toB) => Prune.TtoB
    case (Cap.toT, Cap.toN) => Prune.TtoN
    case (Cap.toB, Cap.toN) => Prune.BtoN
    case (Cap.toT, Cap.toT) => Report.TtoT
    case (Cap.toB, Cap.toB) => Report.BtoB
    case (Cap.toN, Cap.toN) => Report.NtoN
  }
}





abstract class BusFragment(val p : BusParameter, val withData : Boolean) extends Bundle {
  def size      : UInt
  def withBeats : Bool
}

case class ChannelA(override val p : BusParameter) extends BusFragment(p, p.withDataA) {
  val opcode  = Opcode.A()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val size    = p.size()
  val mask    = p.withDataA generate p.mask()
  val data    = p.withDataA generate p.data()
  val corrupt = p.withDataA generate Bool()
  override def withBeats = p.withDataA.mux(List(Opcode.A.PUT_FULL_DATA(), Opcode.A.PUT_PARTIAL_DATA()).sContains(opcode), False)
}
case class ChannelB(override val p : BusParameter) extends BusFragment(p, p.withDataB) {
  val opcode  = Opcode.B()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val size    = p.size()
  val mask    = p.withDataB generate p.mask()
  val data    = p.withDataB generate p.data()
  val corrupt = p.withDataB generate Bool()
  override def withBeats = p.withDataB.mux(False, False) //TODO
}
case class ChannelC(override val p : BusParameter) extends BusFragment(p, true) {
  val opcode  = Opcode.C()
  val param   = Bits(3 bits)
  val source  = p.source()
  val address = p.address()
  val size    = p.size()
  val data    = p.data()
  val corrupt = Bool()
  def isProbeKind()   = opcode === Opcode.C.PROBE_ACK || opcode === Opcode.C.PROBE_ACK_DATA
  def isReleaseKind() = opcode === Opcode.C.RELEASE   || opcode === Opcode.C.RELEASE_DATA
  def isDataKind() = opcode === Opcode.C.PROBE_ACK_DATA   || opcode === Opcode.C.RELEASE_DATA
  override def withBeats = List(Opcode.C.PROBE_ACK_DATA(), Opcode.C.RELEASE_DATA()).sContains(opcode)
}
case class ChannelD(override val p : BusParameter) extends BusFragment(p, p.withDataD) {
  val opcode  = Opcode.D()
  val param   = Bits(3 bits)
  val source  = p.source()
  val sink    = p.sink()
  val size    = p.size()
  val denied  = Bool()
  val data    = p.withDataD generate p.data()
  val corrupt = p.withDataD generate Bool()
  override def withBeats = p.withDataD.mux(List(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.GRANT_DATA()).sContains(opcode), False)
  def withDontCareData(): ChannelD ={
    val ret = ChannelD(p.copy(withDataD = true))
    ret.assignSomeByName(this)
    ret.data.assignDontCare()
    ret.corrupt.assignDontCare()
    ret
  }

  override def clone = ChannelD(p)
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
