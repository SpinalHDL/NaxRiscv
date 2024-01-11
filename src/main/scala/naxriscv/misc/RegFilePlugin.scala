// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.misc

import naxriscv.compatibility.MultiPortWritesSymplifier
import naxriscv.interfaces._
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class RegFileReadParameter(withReady : Boolean, forceNoBypass : Boolean)
case class RegFileWriteParameter(withReady : Boolean)


case class RegFileIo( addressWidth: Int,
                      dataWidth: Int,
                      readsParameter: Seq[RegFileReadParameter],
                      writesParameter: Seq[RegFileWriteParameter]
                    ) extends Bundle{
  val writes = Vec(writesParameter.map(p => slave(RegFileWrite(addressWidth, dataWidth, p.withReady))))
  val reads = Vec(readsParameter.map(p => slave(RegFileRead(addressWidth, dataWidth, p.withReady, 0, p.forceNoBypass))))
}

//The bankCount is currently useless, but maybe uesefull in the future with execution units which can stall
class RegFileAsync(addressWidth    : Int,
                   dataWidth       : Int,
                   bankCount       : Int,
                   readsParameter  : Seq[RegFileReadParameter],
                   writesParameter : Seq[RegFileWriteParameter],
                   preferedWritePortForInit : Int,
                   headZero        : Boolean,
                   allOne         : Boolean,
                   asyncReadBySyncReadRevertedClk : Boolean = false) extends Component {
  assert(!(allOne && headZero))

  val io = RegFileIo(addressWidth, dataWidth, readsParameter, writesParameter)

  val bankShift = log2Up(bankCount)
  val writePortCount = io.writes.count(!_.withReady)
  val readPortCount  = io.reads.count(!_.withReady)
  val banks = for(bankId <- 0 until bankCount) yield new Area{
    val ram = Mem.fill((1 << addressWidth)/bankCount)(Bits(dataWidth bits))
    Verilator.public(ram)
    def asyncRead = if(asyncReadBySyncReadRevertedClk) ram.readAsyncPortBySyncReadRevertedClk else ram.readAsyncPort
    val writePort = Seq.fill(writePortCount)(ram.writePort)
    val readPort = Seq.fill(readPortCount)(asyncRead)
  }
  io.reads.foreach(e => assert(!e.withReady))
  io.writes.foreach(e => assert(!e.withReady))

  for((w, i) <- io.writes.filter(!_.withReady).zipWithIndex){
    for((bank, bankId) <- banks.zipWithIndex){
      val port = bank.writePort(i)
      port.valid := w.valid && w.address(bankShift-1 downto 0) === bankId
      port.address := w.address >> bankShift
      port.data := w.data
    }
  }

  for((r, i) <- io.reads.filter(!_.withReady).zipWithIndex){
    for((bank, bankId) <- banks.zipWithIndex){
      val port = bank.readPort(i)
      port.address := r.address >> bankShift
      r.data := port.data
    }
  }

  val initPort = banks.head.writePort(preferedWritePortForInit)
  val initHead = headZero generate new Area{
    val booted = RegNext(True) init(False)
    when(!booted){
      initPort.valid := True
      initPort.address := 0
      initPort.data := 0
    }
  }

  val initAll = allOne generate new Area{
    assert(bankCount == 1)
    val counter = Reg(UInt(addressWidth+1 bits)) init(0)
    val done = counter.msb
    when(!done){
      initPort.valid := True
      initPort.address := counter.resized
      initPort.data.setAll()
      counter := counter + 1
    }
  }
}

object RegFileAsyncSynth extends App{
  LutInputs.set(6)

  val spinalConfig = SpinalConfig()
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(spinalConfig.generateVerilog(Rtl.ffIo(new RegFileAsync(
    addressWidth    = 6,
    dataWidth       = 32,
    bankCount       = 1,
    readsParameter  = Seq.fill(4)(RegFileReadParameter(withReady = false, forceNoBypass = false)),
    writesParameter = Seq.fill(1)(RegFileWriteParameter(withReady = false)),
    preferedWritePortForInit = 0,
    headZero        = true,
    allOne         = false
  ))).printPruned())
  val targets = XilinxStdTargets().take(2)


  Bench(rtls, targets)
  //  {
  //    val x2 = ClockDomain.external("x2", withReset = false).setSynchronousWith(ClockDomain.current)
  //    ClockDomain.current.addTag(ClockDomainFasterTag(2, x2))
  //  }
}
/*
4r 2w
Artix 7 -> 162 Mhz 630 LUT 702 FF
Artix 7 -> 367 Mhz 632 LUT 702 FF
4r 1w
Artix 7 -> 375 Mhz 176 LUT 585 FF
Artix 7 -> 538 Mhz 176 LUT 585 FF

 */
class sky130_fd_sc_hd__dlxtp_1 extends BlackBox {
  val Q = out Bool()
  val D = in Bool()
  val GATE = in Bool()
}

class RegFileLatch(addressWidth    : Int,
                   dataWidth       : Int,
                   readsParameter  : Seq[RegFileReadParameter],
                   writesParameter : Seq[RegFileWriteParameter],
                   headZero        : Boolean) extends Component {
  val io = RegFileIo(addressWidth, dataWidth, readsParameter, writesParameter)

  io.reads.foreach(e => assert(!e.withReady))
  io.writes.foreach(e => assert(!e.withReady))


  val writeFrontend = new Area {
    @dontName val clock = ClockDomain.current.readClockWire
    val buffers = for (port <- io.writes) yield LatchWhen(port.data, clock)
//    val buffers = for (port <- io.writes) yield RegNext(port.data)
  }

  val latches = for (i <- headZero.toInt until 1 << addressWidth) yield new Area {
    val write = new Area {
      val mask = B(io.writes.map(port => port.valid && port.address === i))
      val maskReg = LatchWhen(mask, writeFrontend.clock)
      val validReg = LatchWhen(mask.orR, writeFrontend.clock)
//      val maskReg = RegNext(mask)
//      val validReg = RegNext(mask.orR)
      val data = OhMux.or(maskReg, writeFrontend.buffers)
    }

//    val storages = Array.fill(dataWidth)(new sky130_fd_sc_hd__dlxtp_1)
//    val GATE = !writeFrontend.clock && write.validReg
//    for((s, i) <- storages.zipWithIndex){
//      s.D := write.data(i)
//      s.GATE := GATE
//    }
//    val storage = storages.map(_.Q).toSeq.asBits

    val storage = LatchWhen(write.data, !writeFrontend.clock && write.validReg)
  }


  val readLogic = for ((r, i) <- io.reads.zipWithIndex) yield new Area {
    var mem = latches.map(_.storage).toList
    if(headZero) mem = B(0, dataWidth bits) :: mem

    val oh = UIntToOh(r.address)
    val tri = Analog(Bits(dataWidth bits))
    mem.onMask(oh){ value =>
      tri := value
    }
    r.data := tri


//    r.data := mem.read(r.address)
  }
}

class RegFilePlugin(var spec : RegfileSpec,
                    var physicalDepth : Int,
                    var bankCount : Int,
                    var preferedWritePortForInit : String,
                    var asyncReadBySyncReadRevertedClk : Boolean = false,
                    var allOne : Boolean = false,
                    var latchBased : Boolean = false) extends Plugin with RegfileService with InitCycles {
  withPrefix(spec.getName())

  override def getPhysicalDepth = physicalDepth
  override def rfSpec = spec
  override def initCycles = if(allOne) physicalDepth else 0

  case class WriteSpec(port : RegFileWrite,
                       withReady : Boolean,
                       latency : Int,
                       sharingKey : Any,
                       priority : Int) //Higher first

  val lock = Lock()
  def addressWidth = log2Up(physicalDepth)
  def dataWidth = spec.width
  val reads = ArrayBuffer[RegFileRead]()
  val writes = ArrayBuffer[WriteSpec]()
  val bypasses = ArrayBuffer[RegFileBypass]()

  override def newRead(withReady : Boolean, forceNoBypass : Boolean) = reads.addRet(RegFileRead(addressWidth, dataWidth, withReady, 0, forceNoBypass))
  override def newWrite(withReady : Boolean, latency : Int, sharingKey : Any = new{}, priority : Int = 0) = writes.addRet(
    WriteSpec(
      port       = RegFileWrite(addressWidth, dataWidth, withReady),
      withReady  = withReady,
      latency    = latency,
      sharingKey = sharingKey,
      priority   = priority
    )
  ).port
  override def newBypass() : RegFileBypass = bypasses.addRet(RegFileBypass(addressWidth, dataWidth))
  override def getWrites() = writes.map(_.port)

  override def retain() = lock.retain()
  override def release() = lock.release()

  val logic = create late new Area{
    lock.await()
    assert(isPow2(bankCount))

    val writeGroups = writes.groupByLinked(_.sharingKey)
    val writeMerges = for((key, elements) <- writeGroups) yield new Area{
      val bus = RegFileWrite(addressWidth, dataWidth, false)
      bus.valid   := elements.map(_.port.valid).orR

      val one = (elements.size == 1) generate new Area{
        val h = elements.head
        bus.address := h.port.address
        bus.data    := h.port.data
        bus.robId   := h.port.robId
        if(h.withReady) h.port.ready := True
      }

      val multiple = (elements.size > 1) generate new Area {
        assert(elements.count(!_.withReady) <= 1)
        val sorted = elements.sortWith((a, b) => if(!a.withReady && b.withReady) true else a.priority > b.priority)
        assert(sorted.map(_.priority).indices.size == sorted.size, "Conflicting priorities for regfile writes")

        val mask = sorted.map(_.port.valid)
        val oh = OHMasking.firstV2(Vec(mask))
        bus.address := OhMux.or(oh, sorted.map(_.port.address))
        bus.data    := OhMux.or(oh, sorted.map(_.port.data))
        bus.robId   := OhMux.or(oh, sorted.map(_.port.robId))
        for((element, enable) <- (sorted, oh).zipped){
          if(element.withReady) element.port.ready := enable
        }
      }
      val bypass = if(elements.exists(_.latency == 0)) new Area{
        val port = newBypass()
        elements.count(_.latency == 0)  match {
          case 1 => {
            val src = elements.find(_.latency == 0).get
            port.valid   := src.port.valid
            port.address := src.port.address
            port.data    := src.port.data
          }
          case _ => {
            port.valid   := bus.valid
            port.address := bus.address
            port.data    := bus.data
          }
        }
      }
    }

    val regfile = new Area{
      val fpga = !latchBased generate new RegFileAsync(
        addressWidth = addressWidth,
        dataWidth = dataWidth,
        bankCount = bankCount,
        readsParameter = reads.map(e => RegFileReadParameter(withReady = e.withReady, e.forceNoBypass)),
        writesParameter = writeMerges.map(e => RegFileWriteParameter(withReady = false)).toList,
        headZero = spec.x0AlwaysZero,
        preferedWritePortForInit = writeGroups.zipWithIndex.find(_._1._2.exists(_.port.getName().contains(preferedWritePortForInit))).get._2,
        allOne = allOne,
        asyncReadBySyncReadRevertedClk = asyncReadBySyncReadRevertedClk
      )

      val latches = latchBased generate new RegFileLatch(
        addressWidth = addressWidth,
        dataWidth = dataWidth,
        readsParameter = reads.map(e => RegFileReadParameter(withReady = e.withReady, e.forceNoBypass)),
        writesParameter = writeMerges.map(e => RegFileWriteParameter(withReady = false)).toList,
        headZero = spec.x0AlwaysZero
      )


      val io = latchBased match {
        case false => fpga.io
        case true => latches.io
      }
    }


    (regfile.io.reads, reads).zipped.foreach(_ <> _)
    (regfile.io.writes, writeMerges.map(_.bus)).zipped.foreach(_ <> _)
    val bypass = for(i <- 0 until regfile.io.readsParameter.size) {
      val r = reads(i)
      if(!r.forceNoBypass && bypasses.size != 0){
        val hits = bypasses.map(b => b.valid && b.address === r.address)
        val hitsValue = MuxOH.mux(hits, bypasses.map(_.data))
        when(hits.orR) {
          r.data := hitsValue
        }
      }
    }

    //Used for tracing in verilator sim
    val writeEvents = Vec(writeMerges.map(e => CombInit(e.bus)))
    writeEvents.setName(spec.getName()+"_write").addAttribute(Verilator.public)
    val doc = getService[DocPlugin]
    doc.property(writeEvents.getName() +"_count", writeEvents.size)
    doc.property(spec.getName() +"_PHYSICAL_DEPTH", physicalDepth)
  }
}


object LatchRegFilePlacer extends App{

//  latches_17_storages_24 585.12 848.64
//  latches_17_storages_25 585.58 848.64
}