package naxriscv.utilities

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

object Misc {
  def outsideCondScope[T](that : => T) : T = {
    val body = Component.current.dslBody  // Get the head of the current component symboles tree (AST in other words)
    val ctx = body.push()                 // Now all access to the SpinalHDL API will be append to it (instead of the current context)
    val swapContext = body.swap()         // Empty the symbole tree (but keep a reference to the old content)
    val ret = that                        // Execute the block of code (will be added to the recently empty body)
    ctx.restore()                         // Restore the original context in which this function was called
    swapContext.appendBack()              // append the original symboles tree to the modified body
    ret                                   // return the value returned by that
  }
}

object AddressToMask{
  def apply(address : UInt, size : UInt, width : Int) : Bits ={
    size.muxListDc((0 to log2Up(width)).map(i => U(i) -> B((1 << (1 << i)) -1, width bits))) |<< address(log2Up(width)-1 downto 0)
  }
}

class Reservation{
  class Entry(val priority : Int) extends Area{
    val win = Bool()
    val take = False

    def takeIt() = take := True
  }
  val model = ArrayBuffer[Entry]()
  def create(priority : Int) : Entry = {
    val e = new  Entry( priority)
    model += e
    e
  }

  Component.current.afterElaboration{
    assert(model.map(_.priority).distinct.size == model.size)
    for(e <- model){
      e.win := !model.filter(_.priority < e.priority).map(_.take).orR
    }
  }
}