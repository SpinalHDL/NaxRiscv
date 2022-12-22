package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping



case class BusParameter(addressWidth  : Int,
                        dataBytes     : Int,
                        sizeBytes     : Int,
                        sourceWidth   : Int,
                        sinkWidth     : Int,
                        withBCE       : Boolean,
                        withDataA     : Boolean,
                        withDataB     : Boolean,
                        withDataD     : Boolean){
  val dataWidth   = dataBytes*8
  val sizeMax     = log2Up(sizeBytes)
  val sizeWidth   = log2Up(sizeMax+1)
  val beatMax     = (sizeBytes+dataBytes-1)/dataBytes
  val beatWidth   = log2Up(beatMax)

  val address     = HardType(UInt(addressWidth bits))
  val data        = HardType(Bits(dataWidth bits))
  val mask        = HardType(Bits(dataBytes bits))
  val source      = HardType(UInt(sourceWidth bits))
  val sink        = HardType(UInt(sinkWidth bits))
  val size        = HardType(UInt(sizeWidth bits))
}

object TransferSupport{
  def none = TransferSupport(0, 0)
  def apply(x: Int) : TransferSupport = TransferSupport(x, x)
}

case class TransferSupport(min : Int, max : Int){

  require (min <= max, s"Min transfer $min > max transfer $max")
  require (min >= 0 && max >= 0, s"TransferSupport must be positive, got: ($min, $max)")
  require (max == 0 || isPow2(max), s"TransferSupport must be a power of 2, got: $max")
  require (min == 0 || isPow2(min), s"TransferSupport must be a power of 2, got: $min")
  require (max == 0 || min != 0, s"TransferSize 0 is forbidden unless (0,0), got: ($min, $max)")

  def none = min == 0
  def some = !none
  def contains(x: Int) = isPow2(x) && min <= x && x <= max
  def containsLg(x: Int) = contains(1 << x)

  def contains(x: TransferSupport) = x.none || (min <= x.min && x.max <= max)

  def intersect(x: TransferSupport) =
    if (x.max < min || max < x.min) TransferSupport.none
    else TransferSupport(scala.math.max(min, x.min), scala.math.min(max, x.max))

  def mincover(x: TransferSupport) = {
    if (none) {
      x
    } else if (x.none) {
      this
    } else {
      TransferSupport(scala.math.min(min, x.min), scala.math.max(max, x.max))
    }
  }

  override def toString() = "TransferSupport[%d, %d]".format(min, max)
}

case class MasterTransfers(acquireT     : TransferSupport = TransferSupport.none,
                           acquireB     : TransferSupport = TransferSupport.none,
                           arithmetic   : TransferSupport = TransferSupport.none,
                           logical      : TransferSupport = TransferSupport.none,
                           get          : TransferSupport = TransferSupport.none,
                           putFull      : TransferSupport = TransferSupport.none,
                           putPartial   : TransferSupport = TransferSupport.none,
                           hint         : TransferSupport = TransferSupport.none,
                           probeAckData : TransferSupport = TransferSupport.none){

  def withBCE = acquireT.some || acquireB.some
  def withDataA = putFull.some
  def withDataD = get.some || acquireT.some || acquireB.some || logical.some || arithmetic.some

  def intersect(rhs: MasterTransfers) = MasterTransfers(
    acquireT     = acquireT    .intersect(rhs.acquireT),
    acquireB     = acquireB    .intersect(rhs.acquireB),
    arithmetic   = arithmetic  .intersect(rhs.arithmetic),
    logical      = logical     .intersect(rhs.logical),
    get          = get         .intersect(rhs.get),
    putFull      = putFull     .intersect(rhs.putFull),
    putPartial   = putPartial  .intersect(rhs.putPartial),
    hint         = hint        .intersect(rhs.hint),
    probeAckData = probeAckData.intersect(rhs.probeAckData))
  def mincover(rhs: MasterTransfers) = MasterTransfers(
    acquireT     = acquireT    .mincover(rhs.acquireT),
    acquireB     = acquireB    .mincover(rhs.acquireB),
    arithmetic   = arithmetic  .mincover(rhs.arithmetic),
    logical      = logical     .mincover(rhs.logical),
    get          = get         .mincover(rhs.get),
    putFull      = putFull     .mincover(rhs.putFull),
    putPartial   = putPartial  .mincover(rhs.putPartial),
    hint         = hint        .mincover(rhs.hint),
    probeAckData = probeAckData.mincover(rhs.probeAckData))
  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: TransferSupport, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(acquireT,   "T"),
      str(acquireB,   "B"),
      str(arithmetic, "A"),
      str(logical,    "L"),
      str(get,        "G"),
      str(putFull,    "F"),
      str(putPartial, "P"),
      str(hint,       "H"))
    flags.mkString
  }
  // Prints out the actual information in a user readable way
  def infoString = {
    s"""acquireT = ${acquireT}
       |acquireB = ${acquireB}
       |arithmetic = ${arithmetic}
       |logical = ${logical}
       |get = ${get}
       |putFull = ${putFull}
       |putPartial = ${putPartial}
       |hint = ${hint}
       |
       |""".stripMargin
  }
  val sizeBytes = List(
    acquireT.max,
    acquireB.max,
    arithmetic.max,
    logical.max,
    get.max,
    putFull.max,
    putPartial.max,
    probeAckData.max
  ).max
}

object MasterTransfers {
  def unknownEmits = MasterTransfers(
    acquireT   = TransferSupport(1, 4096),
    acquireB   = TransferSupport(1, 4096),
    arithmetic = TransferSupport(1, 4096),
    logical    = TransferSupport(1, 4096),
    get        = TransferSupport(1, 4096),
    putFull    = TransferSupport(1, 4096),
    putPartial = TransferSupport(1, 4096),
    hint       = TransferSupport(1, 4096))
  def unknownSupports = MasterTransfers()
}


case class SlaveTransfers(probe:      TransferSupport = TransferSupport.none,
                          arithmetic: TransferSupport = TransferSupport.none,
                          logical:    TransferSupport = TransferSupport.none,
                          get:        TransferSupport = TransferSupport.none,
                          putFull:    TransferSupport = TransferSupport.none,
                          putPartial: TransferSupport = TransferSupport.none,
                          hint:       TransferSupport = TransferSupport.none
)  {
  def withBCE = probe.some
  def withDataB = putFull.some || putPartial.some

  def intersect(rhs: SlaveTransfers) = SlaveTransfers(
    probe      = probe     .intersect(rhs.probe),
    arithmetic = arithmetic.intersect(rhs.arithmetic),
    logical    = logical   .intersect(rhs.logical),
    get        = get       .intersect(rhs.get),
    putFull    = putFull   .intersect(rhs.putFull),
    putPartial = putPartial.intersect(rhs.putPartial),
    hint       = hint      .intersect(rhs.hint)
  )
  def mincover(rhs: SlaveTransfers) = SlaveTransfers(
    probe      = probe     .mincover(rhs.probe),
    arithmetic = arithmetic.mincover(rhs.arithmetic),
    logical    = logical   .mincover(rhs.logical),
    get        = get       .mincover(rhs.get),
    putFull    = putFull   .mincover(rhs.putFull),
    putPartial = putPartial.mincover(rhs.putPartial),
    hint       = hint      .mincover(rhs.hint)
  )
  // Reduce rendering to a simple yes/no per field
  override def toString = {
    def str(x: TransferSupport, flag: String) = if (x.none) "" else flag
    def flags = Vector(
      str(probe,      "P"),
      str(arithmetic, "A"),
      str(logical,    "L"),
      str(get,        "G"),
      str(putFull,    "F"),
      str(putPartial, "P"),
      str(hint,       "H"))
    flags.mkString
  }
  // Prints out the actual information in a user readable way
  def infoString = {
    s"""probe = ${probe}
       |arithmetic = ${arithmetic}
       |logical = ${logical}
       |get = ${get}
       |putFull = ${putFull}
       |putPartial = ${putPartial}
       |hint = ${hint}
       |
       |""".stripMargin
  }

  val sizeBytes = List(
    probe.max,
    arithmetic.max,
    logical.max,
    get.max,
    putFull.max,
    putPartial.max,
    hint.max
  ).max
}

object SlaveTransfers {
  def unknownEmits = SlaveTransfers(
    arithmetic = TransferSupport(1, 4096),
    logical    = TransferSupport(1, 4096),
    get        = TransferSupport(1, 4096),
    putFull    = TransferSupport(1, 4096),
    putPartial = TransferSupport(1, 4096),
    hint       = TransferSupport(1, 4096),
    probe      = TransferSupport(1, 4096))
  def unknownSupports = SlaveTransfers()
}


case class MasterParameters (name         : Nameable,
                             emits        : MasterTransfers,
//                             supports     : SlaveTransfers,
                             sourceId     : AddressMapping,
                             addressRange : Seq[AddressMapping]) extends OverridedEqualsHashCode {
  val addressWidth = log2Up(addressRange.map(_.highestBound).max+1)
  def withSourceOffset(offset : Int): MasterParameters ={
    copy(sourceId = sourceId.withOffset(offset))
  }
}

case class SlaveParameters (name         : Nameable,
                            sinkId       : AddressMapping,
                            emits        : SlaveTransfers/*,
                            supports     : MasterTransfers*/) extends OverridedEqualsHashCode {
  val sinkWidth = sinkId.width
  def withSinkOffset(offset : Int): SlaveParameters ={
    copy(sinkId = sinkId.withOffset(offset))
  }
}



case class MastersParameters(masters   : Seq[MasterParameters]) extends OverridedEqualsHashCode {
  val addressWidth = masters.map(_.addressWidth).max
  val sizeBytes = masters.map(_.emits.sizeBytes).max
  val sourceWidth = masters.map(_.sourceId.width).max
  val withBCE = masters.map(_.emits.withBCE).reduce(_ || _)
  def withDataA = masters.map(_.emits.withDataA).reduce(_ || _)
  def withDataD = masters.map(_.emits.withDataD).reduce(_ || _)
}

case class SlavesParameters(slaves    : Seq[SlaveParameters]) extends OverridedEqualsHashCode {
  def defaulted[T](default : T)(body : => T) : T = if(slaves.isEmpty) default else body
  val sizeBytes = defaulted(0)(slaves.map(_.emits.sizeBytes).max)
  val sinkWidth = defaulted(0)(slaves.map(_.sinkWidth).max)
  val withBCE   = defaulted(false)(slaves.map(_.emits.withBCE).reduce(_ || _))
  def withDataB = defaulted(false)(slaves.map(_.emits.withDataB).reduce(_ || _))
}

case class NodeParameters(m : MastersParameters,
                          s : SlavesParameters,
                          dataBytes : Int){
  val sizeBytes = s.sizeBytes max m.sizeBytes
  val withBCE = s.withBCE || m.withBCE
  def toBusParameter() = BusParameter(
    addressWidth  = m.addressWidth,
    dataBytes     = dataBytes,
    sizeBytes     = sizeBytes,
    sourceWidth   = m.sourceWidth,
    sinkWidth     = s.sinkWidth,
    withBCE       = withBCE,
    withDataA     = m.withDataA,
    withDataB     = s.withDataB,
    withDataD     = m.withDataD
  )
}

object NodeParameters{
  def mergeMasters(nodes : Seq[NodeParameters]): NodeParameters ={
    val sourcePreWidth = log2Up(nodes.map(_.m.sourceWidth).max)
    NodeParameters(
      m = MastersParameters(
        masters = nodes.zipWithIndex.flatMap{
          case (node, i) => node.m.masters.map(_.withSourceOffset(i << sourcePreWidth))
        }
      ),
      s = nodes.head.s,
      dataBytes = nodes.map(_.dataBytes).max
    )
  }
}