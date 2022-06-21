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


//The bankCount is currently useless, but maybe uesefull in the future with execution units which can stall
class RegFileAsync(addressWidth    : Int,
                   dataWidth       : Int,
                   bankCount       : Int,
                   readsParameter  : Seq[RegFileReadParameter],
                   writesParameter : Seq[RegFileWriteParameter],
                   bypasseCount    : Int,
                   headZero        : Boolean,
                   allZero         : Boolean,
                   asyncReadBySyncReadRevertedClk : Boolean = false) extends Component {
  assert(!(allZero && headZero))

  val io = new Bundle {
    val writes = Vec(writesParameter.map(p => slave(RegFileWrite(addressWidth, dataWidth, p.withReady))))
    val reads = Vec(readsParameter.map(p => slave(RegFileRead(addressWidth, dataWidth, p.withReady, 0, p.forceNoBypass))))
    val bypasses = Vec.fill(bypasseCount)(slave(RegFileBypass(addressWidth, dataWidth)))
  }

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
//      port.valid := r.valid && r.address(bankShift-1 downto 0) === bankId
      port.address := r.address >> bankShift
      r.data := port.data
    }

    val bypass = (!r.forceNoBypass && bypasseCount != 0) generate new Area{
      val hits = io.bypasses.map(b => b.valid && b.address === r.address)
      val hitsValue = MuxOH.mux(hits, io.bypasses.map(_.data))
      when(hits.orR){
        r.data := hitsValue
      }
    }
  }

  val initHead = headZero generate new Area{
    val booted = RegNext(True) init(False)
    when(!booted){
      val bank = banks.head
      val port = bank.writePort.head
      port.valid := True
      port.address := 0
      port.data := 0
    }
  }

  val initAll = allZero generate new Area{
    assert(bankCount == 1)
    val counter = Reg(UInt(addressWidth+1 bits)) init(0)
    val done = counter.msb
    when(!done){
      val bank = banks.head
      val port = bank.writePort.head
      port.valid := True
      port.address := counter.resized
      port.data := 0
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
    bypasseCount    = 0,
    headZero        = true,
    allZero         = false
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



class RegFilePlugin(var spec : RegfileSpec,
                    var physicalDepth : Int,
                    var bankCount : Int,
                    var asyncReadBySyncReadRevertedClk : Boolean = false,
                    var allZero : Boolean = false) extends Plugin with RegfileService with InitCycles {
  withPrefix(spec.getName())

  override def getPhysicalDepth = physicalDepth
  override def rfSpec = spec
  override def initCycles = if(allZero) physicalDepth else 0

  val lock = Lock()
  def addressWidth = log2Up(physicalDepth)
  def dataWidth = spec.width
  val reads = ArrayBuffer[RegFileRead]()
  val writes = ArrayBuffer[RegFileWrite]()
  val bypasses = ArrayBuffer[RegFileBypass]()

  override def newRead(withReady : Boolean, forceNoBypass : Boolean) = reads.addRet(RegFileRead(addressWidth, dataWidth, withReady, 0, forceNoBypass))
  override def newWrite(withReady : Boolean, latency : Int) = writes.addRet(RegFileWrite(addressWidth, dataWidth, withReady, latency))
  override def newBypass() : RegFileBypass = bypasses.addRet(RegFileBypass(addressWidth, dataWidth))


  override def getWrites() = writes

  override def retain() = lock.retain()
  override def release() = lock.release()

  val logic = create late new Area{
    lock.await()
    assert(isPow2(bankCount))

    val writeBypasses = for(write <- writes; if write.latency == 0) yield new Area{
      val port = newBypass()
      port.valid := write.valid
      port.address := write.address
      port.data := write.data
    }

    val regfile = new RegFileAsync(
      addressWidth    = addressWidth,
      dataWidth       = dataWidth,
      bankCount       = bankCount,
      readsParameter  = reads.map(e => RegFileReadParameter(withReady = e.withReady, e.forceNoBypass)),
      writesParameter = writes.map(e => RegFileWriteParameter(withReady = e.withReady)),
      bypasseCount    = bypasses.size,
      headZero        = spec.x0AlwaysZero,
      allZero         = allZero,
      asyncReadBySyncReadRevertedClk = asyncReadBySyncReadRevertedClk
    )

    (regfile.io.reads, reads).zipped.foreach(_ <> _)
    (regfile.io.writes, writes).zipped.foreach(_ <> _)
    (regfile.io.bypasses, bypasses).zipped.foreach(_ <> _)

    //Used for tracing in verilator sim
    val writeEvents = Vec(writes.map(e => e.asWithoutReady()))
    writeEvents.setName(spec.getName()+"_write").addAttribute(Verilator.public)
    val doc = getService[DocPlugin]
    doc.property(writeEvents.getName() +"_count", writeEvents.size)
    doc.property(spec.getName() +"_PHYSICAL_DEPTH", physicalDepth)
  }
}


