package spinal.lib.logic

import spinal.core.{Bits, Bool, MaskedLiteral, assert}

object Masked{
  def apply(ml : MaskedLiteral) : Masked = Masked(ml.value, ml.careAbout)
}

case class Masked(value : BigInt,care : BigInt){
  assert((value & ~care) == 0)
  var isPrime = true

  def < (that: Masked) = value < that.value || value == that.value && ~care < ~that.care

  def intersects(x: Masked) = ((value ^ x.value) & care & x.care) == 0

  def covers(x: Masked) = ((value ^ x.value) & care | (~x.care) & care) == 0

  def setPrime(value : Boolean) = {
    isPrime = value
    this
  }

  def mergeOneBitDifSmaller(x: Masked) = {
    val bit = value - x.value
    val ret = new Masked(value &~ bit, care & ~bit)
    //    ret.isPrime = isPrime || x.isPrime
    isPrime = false
    x.isPrime = false
    ret
  }
  def isSimilarOneBitDifSmaller(x: Masked) = {
    val diff = value - x.value
    care == x.care && value > x.value && (diff & diff - 1) == 0
  }


  def === (hard : Bits) : Bool = (hard & care) === (value & care)

  def toString(bitCount : Int) = (0 until bitCount).map(i => if(care.testBit(i)) (if(value.testBit(i)) "1" else "0") else "-").reverseIterator.reduce(_+_)
}
