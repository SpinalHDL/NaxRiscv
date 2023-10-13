// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.misc

import naxriscv.Frontend._
import naxriscv._
import naxriscv.frontend.FrontendPlugin
import naxriscv.interfaces.{RobCompletion, RobLineMask, RobService}
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core.fiber.Lock
import spinal.core.{log2Up, _}
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RobPlugin(var robSize : Int,
                var completionWithReg : Boolean = false) extends Plugin with RobService{

  create config {
    ROB.SIZE.set(robSize)
  }


  case class Completion(bus : Flow[RobCompletion])
  val completions = ArrayBuffer[Completion]()

  val robLineMaskPort = ArrayBuffer[RobLineMask]()

  override def newRobLineValids(bypass : Boolean) = { val e = RobLineMask(bypass); robLineMaskPort += e; e }
  override def newRobCompletion() = { val c = Completion(Flow(RobCompletion())); completions += c; c.bus }

  case class Write(key: Stageable[Data], size : Int, value: Seq[Data], robId: UInt, enable: Bool)
  case class ReadAsync(key: Stageable[Data], size : Int, robId: UInt, skipFactor: Int, skipOffset: Int, rsp : Vec[Data])

  val writes = ArrayBuffer[Write]()
  val readAsyncs = ArrayBuffer[ReadAsync]()

  override def write[T <: Data](key: Stageable[T], size : Int, value: Seq[T], robId: UInt, enable: Bool) = {
    writes += Write(key.asInstanceOf[Stageable[Data]], size, value, robId, enable)
  }
  override def readAsync[T <: Data](key: Stageable[T], size : Int, robId: UInt, skipFactor: Int = 1, skipOffset: Int = 0) : Vec[T] = {
    val r = ReadAsync(key.asInstanceOf[Stageable[Data]], size, robId, skipFactor, skipOffset, Vec.fill(size)(key()))
    readAsyncs += r
    r.rsp.asInstanceOf[Vec[T]]
  }


  val lock = Lock()
  override def retain() = lock.retain()
  override def release() = lock.release()

  val setup = create early new Area{
    getService[FrontendPlugin].retain()
  }


  val logic = create late new Area{
    lock.await()
    val frontend = getService[FrontendPlugin]

    val completionReg = completionWithReg generate new Area {
      val valids = Reg(Bits(ROB.SIZE bits)) init (0)
      for (p <- robLineMaskPort) {
        p.mask := valids.subdivideIn(ROB.COLS bits).read(p.line(ROB.lineRange))
      }
      for (c <- completions) {
        when(c.bus.valid) {
          valids(c.bus.id) := True
        }
      }

      when(frontend.pipeline.allocated.isFireing) {
        valids.subdivideIn(ROB.COLS bits).write(frontend.pipeline.allocated(ROB.ID) >> log2Up(ROB.COLS), B(0, ROB.COLS bits))
      }
    }

    val completionMem = !completionWithReg generate new Area {
      assert(ROB.COLS == DISPATCH_COUNT)
      val target = Mem.fill(ROB.LINES)(Bits(ROB.COLS bits))
      val hits   = List.fill(completions.size)(Mem.fill(ROB.SIZE)(Bool()))
      if(GenerationFlags.simulation){
        hits.foreach(mem => mem.initBigInt(List.fill(mem.wordCount)(BigInt(0))))
      }

      val targetWrite = target.writePort
      targetWrite.valid := frontend.pipeline.allocated.isFireing
      targetWrite.address := (frontend.pipeline.allocated(ROB.ID) >> log2Up(ROB.COLS))
      val init = for(slotId <- 0 until DISPATCH_COUNT) yield new Area{
        val robId = (frontend.pipeline.allocated(ROB.ID) >> log2Up(ROB.COLS)) @@ U(slotId, log2Up(ROB.COLS)  bits)
        targetWrite.data(slotId) := hits.map(_.readAsync(robId)).xorR
      }

      for ((c, id) <- completions.zipWithIndex) {
        when(c.bus.valid) {
          hits(id).write(c.bus.id, !hits(id).readAsync(c.bus.id))
        }
      }

      val reads = for (p <- robLineMaskPort) yield new Area{
        val targetRead = target.readAsyncPort
        targetRead.address := p.line >> log2Up(ROB.COLS)
        for(slotId <- 0 until ROB.COLS) yield new Area {
          val robId = (p.line >> log2Up(ROB.COLS)) @@ U(slotId, log2Up(ROB.COLS) bits)
          p.mask(slotId) := hits.map(_.readAsync(robId)).xorR =/= targetRead.data(slotId)
        }
      }
    }

    val bypasses = new Area {
      for (p <- robLineMaskPort if p.bypass) {
        for (slotId <- 0 until ROB.COLS) yield new Area {
          val robId = (p.line >> log2Up(ROB.COLS)) @@ U(slotId, log2Up(ROB.COLS) bits)
          for ((c, id) <- completions.zipWithIndex) {
            when(c.bus.valid && c.bus.id === robId) {
              p.mask(slotId) := True
            }
          }
        }
        when(frontend.pipeline.allocated.isFireing && frontend.pipeline.allocated(ROB.ID) === p.line) {
          p.mask.clearAll()
        }
      }
    }

    val storage = new Area{
      val keys = mutable.LinkedHashSet[Stageable[Data]]()
      keys ++= readAsyncs.map(_.key)
      val e = for(key <- keys; if widthOf(key) != 0) yield new Area{
        val k = key
        val wl = writes.filter(_.key == key)
        val ral = readAsyncs.filter(_.key == key)
        if(wl.isEmpty) SpinalError(s"RobPlugin has not writes for ${key}")
        val writeSizeMin = wl.map(_.size).min
        val writeSizeMax = wl.map(_.size).max
        val readSizeMin = ral.map(_.size).min
        val sizeMin = readSizeMin min writeSizeMin
        val bankCount = writeSizeMax/sizeMin
        val elementPerBank = sizeMin
        val banks = Seq.fill(bankCount)(Mem.fill(ROB.SIZE/bankCount/sizeMin)(Vec.fill(elementPerBank)(key())))

        for(e <- wl){
          assert(isPow2(e.size))
          val ratio = e.size / sizeMin
          val bankRange = log2Up(writeSizeMax) - 1 downto log2Up(e.size)
          val address =  e.robId >> log2Up(bankCount*sizeMin)
          val groupedData = e.value.grouped(sizeMin).toSeq
          for((bank, bankId) <- banks.zipWithIndex) {
            bank.write(
              enable = e.enable && e.robId(bankRange) === bankId/ratio,
              address = address,
              data = Vec(groupedData(bankId % groupedData.size))
            )
          }
        }

        for(e <- ral){
          assert(isPow2(e.size))
          if(e.size > writeSizeMax){
            assert(e.skipFactor == 1)
            val ratio = e.size/elementPerBank
            val address =  e.robId >> log2Up(ratio*elementPerBank*bankCount)
            val reads = for(slice <- 0 until ratio) yield banks(slice % bankCount).readAsync(address @@ U(slice / bankCount, log2Up(ratio*elementPerBank*bankCount) bits))
            e.rsp.assignFromBits(B(reads))
          } else if(e.skipFactor == 1){
            val address =  e.robId >> log2Up(bankCount*sizeMin)
            val reads = banks.map(_.readAsync(
              address = address
            ))
            val cat = Cat(reads)
            val sliceRange = log2Up(writeSizeMax) - 1 downto log2Up(e.size)
            e.rsp.assignFromBits(cat.subdivideIn(widthOf(e.rsp) bits).read(e.robId(sliceRange)))
          } else {
            assert(e.size == 1)
            val address =  e.robId >> log2Up(bankCount*sizeMin)
            val banksIds = (0 until bankCount).filter(v => v % e.skipFactor == e.skipOffset)
            val reads = banksIds.map(banks(_)).map(_.readAsync(
              address = address
            ))
            val cat = Cat(reads)
            val sliceRange = log2Up(writeSizeMax) - 1 downto log2Up(e.size*e.skipFactor)
            e.rsp.assignFromBits(cat.subdivideIn(widthOf(e.rsp) bits).read(e.robId(sliceRange)))
          }
        }
      }

      for(x <- e) {
        if(x.k.isNamed) {
          x.setPartialName(x.k.getName())
        }
      }
    }

    val whitebox = new Area{
      val completionsPorts = Verilator.public(Vec(completions.map(_.bus.combStage())))
    }
    getService[DocPlugin].property("ROB_SIZE", ROB.SIZE.get)
    getService[DocPlugin].property("ROB_COMPLETIONS_PORTS", whitebox.completionsPorts.size)
    frontend.release()
  }

}
