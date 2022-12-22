package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping


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
  def intersect(values : Seq[SlaveTransfers]) : SlaveTransfers = values.reduce(_ intersect _)
}

case class SlaveSink(id           : AddressMapping,
                     emits        : SlaveTransfers){
  def withSinkOffset(offset : Int) = copy(id = id.withOffset(offset))
  val sinkWidth = id.width
}


case class SlaveParameters (name         : Nameable,
                            sinks        : Seq[SlaveSink]) extends OverridedEqualsHashCode {
  val sinkWidth = sinks.map(_.sinkWidth).max
  def withSinkOffset(offset : Int): SlaveParameters ={
    copy(sinks = sinks.map(_.withSinkOffset(offset)))
  }
  val emits = SlaveTransfers.intersect(sinks.map(_.emits))
}


case class SlavesParameters(slaves    : Seq[SlaveParameters]) extends OverridedEqualsHashCode {
  def defaulted[T](default : T)(body : => T) : T = if(slaves.isEmpty) default else body
  val sizeBytes = defaulted(0)(slaves.map(_.emits.sizeBytes).max)
  val sinkWidth = defaulted(0)(slaves.map(_.sinkWidth).max)
  val withBCE   = defaulted(false)(slaves.map(_.emits.withBCE).reduce(_ || _))
  def withDataB = defaulted(false)(slaves.map(_.emits.withDataB).reduce(_ || _))
}

