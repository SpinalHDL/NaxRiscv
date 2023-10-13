// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.lsu

import spinal.core._
import spinal.lib._

case class PrefetchPrediction(addressWidth : Int) extends Bundle with IMasterSlave {
  val cmd = Stream(UInt(addressWidth bits))
  val rsp = Flow(Bool())

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

case class PrefetchLearnCmd(addressWidth : Int) extends Bundle {
  val physical = UInt(addressWidth bits)
  val allocate = Bool()
}

class PrefetchPredictor(lineSize          : Int,
                        addressWidth      : Int,
                        slotCount         : Int = 0,
                        prefetchMax       : Int = 2,
                        confidenceMin     : Int = 3,
                        confidenceStartAt : Int = 2,
                        confidenceMax     : Int = 4) extends Component{
  val io = new Bundle {
    val learn      = slave(Flow(PrefetchLearnCmd(addressWidth)))
    val prediction = master(PrefetchPrediction(addressWidth))
  }

  val lineWidth = log2Up(lineSize)
  val tagWidth = addressWidth - lineWidth
  val counterWidth = log2Up(prefetchMax+1)

  val confidenceWidth = log2Up(confidenceMax+1)
  val tagRange = tagWidth + lineWidth - 1 downto lineWidth

  val dummy = (slotCount == 0) generate new Area{
    io.prediction.cmd.setIdle()
  }

  val impl = (slotCount != 0) generate new Area{
    val slots = for(_id <- 0 until slotCount) yield new Area {
      val id = _id
      val tag = Reg(UInt(tagWidth bits))
      val confidence = Reg(UInt(confidenceWidth bits)) init(0)
      val prefetched = Reg(UInt(counterWidth bits))
      val inflight = RegInit(False)

      val prefetchedInc = False
      val prefetchedDec = False
      val allocate = False
      val inflightSet = False

      inflight setWhen(inflightSet) clearWhen(allocate)
      prefetched := (prefetched + U(prefetchedInc) - U(prefetchedDec && !(prefetched === 0 && !prefetchedInc))).andMask(!allocate)

      val isFree = confidence === 0
      val isFull = confidence === confidenceMax
    }


    val learn = new Area{
      val cmdHits0 = B(slots.map(s => s.tag === io.learn.physical(tagRange)))
      val cmdHits1 = B(slots.map(s => s.tag+1 === io.learn.physical(tagRange)))

      val cmdHit0 = cmdHits0.orR
      val cmdHit1 = cmdHits1.orR

      val cmdHits = cmdHits0 | cmdHits1
      val cmdHit = cmdHits.orR

      val freeHits = B(slots.map(_.isFree))
      val freeHit = freeHits.orR
      val freeOh = OHMasking.firstV2(freeHits)

      val perSlot = for(slot <- slots) yield new Area {
        val incrBy = U(1, confidenceWidth bits).andMask(cmdHits1(slot.id) && !slot.isFull)
        val decrBy = U((1 << confidenceWidth) - 1, confidenceWidth bits).andMask(!cmdHit)

        when(io.learn.valid) {
          when(!slot.isFree) {
            slot.confidence := slot.confidence + (incrBy | decrBy)
            when(cmdHits(slot.id)) {
              slot.tag := io.learn.physical(tagRange)
            }
            when(cmdHits1(slot.id)) {
              slot.tag := io.learn.physical(tagRange)
              slot.prefetchedDec := True
            }
          }
          when(!cmdHit && io.learn.allocate && freeHit && freeOh(slot.id)){
            slot.allocate := True
            slot.confidence := confidenceStartAt
            slot.tag := io.learn.physical(tagRange)
          }
        }
      }
    }

    val prediction = new Area{
      val requestHits = B(slots.map(s => s.prefetched < prefetchMax && s.confidence >= confidenceMin))
      val requestHit = requestHits.orR
      val requestOh = OHMasking.firstV2(requestHits)

      val base   = OhMux(requestOh, slots.map(_.tag)) << lineWidth
      val offset = OhMux(requestOh, slots.map(_.prefetched)) << lineWidth

      val pending = RegInit(False) setWhen(io.prediction.cmd.fire) clearWhen(io.prediction.rsp.fire)

      io.prediction.cmd.valid := requestHit && !pending
      io.prediction.cmd.payload := base + offset + lineSize
      for(slot <- slots){
        slot.inflightSet setWhen(io.prediction.cmd.fire && requestOh(slot.id))
        slot.prefetchedInc setWhen(io.prediction.rsp.fire && slot.inflight && io.prediction.rsp.payload)
        slot.inflight clearWhen(io.prediction.rsp.valid)
      }
    }
  }
}
