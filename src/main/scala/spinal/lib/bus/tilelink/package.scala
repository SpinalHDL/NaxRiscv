package spinal.lib.bus

import spinal.core._
import spinal.lib._

package object tilelink {
  def sizeToBeatMinusOne(p : BusParameter, size : UInt) : UInt = signalCache(p, size){
    (0 until p.sizeMax).map(s => U(1 << s)/p.dataBytes).read(size)
  }
  implicit class BusFragmentPimper(ch : BusFragment){
    def sizeToBeatMinusOne = tilelink.sizeToBeatMinusOne(ch.p, ch.size)
  }

  implicit class TilelinkBusFragmentPimper[T <: BusFragment] (ch : Stream[T]){
    def isLast() : Bool = {
      ch.withData match {
        case false => True
        case true => signalCache(ch -> "isLast"){
          new Composite(ch, "isLast"){
            val beat = Reg(UInt(ch.p.beatWidth bits)) init(0)
            val value = beat === sizeToBeatMinusOne(ch.p, ch.size)
            when(ch.fire){
              beat := beat + 1
              when(value){
                beat := 0
              }
            }
          }.value
        }
      }

    }

    def isFirst() : Bool = ch.withData match {
      case false => True
      case true => signalCache(ch)(RegNext(isLast(), ch.fire) init(True) setCompositeName(ch, "isFirst"))
    }
  }
}
