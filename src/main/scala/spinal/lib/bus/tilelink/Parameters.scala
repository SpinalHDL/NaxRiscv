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