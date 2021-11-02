package spinal.lib.pipeline

import spinal.core.{Area, Bool, Data, Nameable, OverridedEqualsHashCode, when}


case class ConnectionPoint(valid : Bool, ready : Bool, payload : Seq[Data]) extends Nameable
trait ConnectionLogic extends Nameable with OverridedEqualsHashCode {
  def on(m : ConnectionPoint,
         s : ConnectionPoint,
         flush : Bool, flushNext : Bool, flushNextHit : Bool) : Area // Remove => one element, flush =>

  def latency : Int = ???
  def tokenCapacity : Int = ???
  def alwasContainsSlaveToken : Boolean = false
  def withPayload : Boolean = true
}

object Connection{
  case class DIRECT() extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool, flushNext : Bool, flushNextHit : Bool) = new Area {
      if(m.ready != null) m.ready   := s.ready
      s.valid   := m.valid
      (s.payload, m.payload).zipped.foreach(_ := _)
    }

    override def latency = 0
    override def tokenCapacity = 0
  }

  case class M2S(collapse : Boolean = true,
                 holdPayload : Boolean = false,
                 flushPreserveInput : Boolean = false) extends ConnectionLogic {
    def on(m : ConnectionPoint,
           s : ConnectionPoint,
           flush : Bool, flushNext : Bool, flushNextHit : Bool) = new Area{

      s.valid.setAsReg()
      s.payload.foreach(_.setAsReg())



      m.ready match {
        case null =>
          s.valid := m.valid
          (s.payload, m.payload).zipped.foreach(_ := _)
        case r => {
          if (flush != null && flushPreserveInput) s.valid clearWhen(flush)
          when(r) {
            s.valid := m.valid
          }
          when(if (holdPayload) m.valid && r else r) {
            (s.payload, m.payload).zipped.foreach(_ := _)
          }
        }
      }


      if (flush != null && !flushPreserveInput) s.valid clearWhen(flush)

      if(m.ready != null) {
        m.ready := s.ready
        if (collapse) m.ready setWhen (!s.valid)
      }
    }

    override def latency = 1
    override def tokenCapacity = 1
    override def alwasContainsSlaveToken : Boolean = true
  }

}