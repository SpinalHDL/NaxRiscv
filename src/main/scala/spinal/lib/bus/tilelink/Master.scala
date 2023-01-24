package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping


case class MasterTransfers(acquireT     : SizeRange = SizeRange.none,
                           acquireB     : SizeRange = SizeRange.none,
                           arithmetic   : SizeRange = SizeRange.none,
                           logical      : SizeRange = SizeRange.none,
                           get          : SizeRange = SizeRange.none,
                           putFull      : SizeRange = SizeRange.none,
                           putPartial   : SizeRange = SizeRange.none,
                           hint         : SizeRange = SizeRange.none,
                           probeAckData : SizeRange = SizeRange.none){

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
    def str(x: SizeRange, flag: String) = if (x.none) "" else flag
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
    acquireT   = SizeRange(1, 4096),
    acquireB   = SizeRange(1, 4096),
    arithmetic = SizeRange(1, 4096),
    logical    = SizeRange(1, 4096),
    get        = SizeRange(1, 4096),
    putFull    = SizeRange(1, 4096),
    putPartial = SizeRange(1, 4096),
    hint       = SizeRange(1, 4096))
  def unknownSupports = MasterTransfers()

  def intersect(values : Seq[MasterTransfers]) : MasterTransfers = values.reduce(_ intersect _)
}



case class MasterSource(id           : AddressMapping,
                        emits        : MasterTransfers,
                        addressRange : Seq[AddressMapping]){
  def withSourceOffset(offset : Int) = copy(id = id.withOffset(offset))
  def bSourceId = id.lowerBound.toInt
}

case class MasterParameters (name    : Nameable,
                             mapping : Seq[MasterSource]) extends OverridedEqualsHashCode {
  val addressWidth = log2Up(mapping.flatMap(_.addressRange.map(_.highestBound)).max+1)
  def withSourceOffset(offset : Int): MasterParameters ={
    copy(mapping = mapping.map(_.withSourceOffset(offset)))
  }
  val emits = MasterTransfers.intersect(mapping.map(_.emits))
  val sourceWidth = mapping.map(_.id.width).max
  def bSourceId = mapping.head.bSourceId
  def sourceHit(source : UInt) = mapping.map(_.id.hit(source)).orR
}



case class MastersParameters(masters   : Seq[MasterParameters]) extends OverridedEqualsHashCode {
  val addressWidth = masters.map(_.addressWidth).max
  val sizeBytes = masters.map(_.emits.sizeBytes).max
  val sourceWidth = masters.map(_.sourceWidth).max
  val withBCE = masters.map(_.emits.withBCE).reduce(_ || _)
  def withDataA = masters.map(_.emits.withDataA).reduce(_ || _)
  def withDataD = masters.map(_.emits.withDataD).reduce(_ || _)
  def sourceHit(source : UInt) = masters.map(_.sourceHit(source)).orR
  def getMasterFromSource(source : Int) = masters.find(_.mapping.exists(_.id.hit(source))).get
}

