package naxriscv.backend

import naxriscv.compatibility.MultiPortWritesSymplifier
import spinal.core._
import spinal.lib._
import naxriscv.interfaces._
import naxriscv.utilities.Plugin
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class RegFileReadParameter(withReady : Boolean)
case class RegFileWriteParameter(withReady : Boolean)


//The bankCount is currently useless, but maybe uesefull in the future with execution units which can stall
class RegFileAsync(addressWidth    : Int,
                   dataWidth       : Int,
                   bankCount       : Int,
                   readsParameter  : Seq[RegFileReadParameter],
                   writesParameter : Seq[RegFileWriteParameter],
                   bypasseCount    : Int) extends Component {
  val io = new Bundle {
    val writes = Vec(writesParameter.map(p => slave(RegFileWrite(addressWidth, dataWidth, p.withReady))))
    val reads = Vec(readsParameter.map(p => slave(RegFileRead(addressWidth, dataWidth, p.withReady, 0))))
    val bypasses = Vec.fill(bypasseCount)(slave(RegFileBypass(addressWidth, dataWidth)))
  }

  assert(bypasseCount == 0)

  val bankShift = log2Up(bankCount)
  val writePortCount = io.writes.count(!_.withReady)
  val readPortCount  = io.reads.count(!_.withReady)
  val banks = for(bankId <- 0 until bankCount) yield new Area{
    val ram = Mem.fill((1 << addressWidth)/bankCount)(Bits(dataWidth bits))
    val writePort = Seq.fill(writePortCount)(ram.writePort)
    val readPort = Seq.fill(readPortCount)(ram.readAsyncPort)
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
    readsParameter  = Seq.fill(4)(RegFileReadParameter(withReady = false)),
    writesParameter = Seq.fill(1)(RegFileWriteParameter(withReady = false)),
    bypasseCount    = 0
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
 */



class RegFilePlugin(spec : RegfileSpec,
                    physicalDepth : Int,
                    bankCount : Int) extends Plugin with RegfileService{
  override def uniqueIds = List(spec)
  override def getPhysicalDepth = physicalDepth

  assert(isPow2(bankCount))

  val addressWidth = log2Up(physicalDepth)
  val dataWidth = spec.width
  val reads = ArrayBuffer[RegFileRead]()
  val writes = ArrayBuffer[RegFileWrite]()
  val bypasses = ArrayBuffer[RegFileBypass]()

  override def newRead(withReady : Boolean) = reads.addRet(RegFileRead(addressWidth, dataWidth, withReady, 0))
  override def newWrite(withReady : Boolean) = writes.addRet(RegFileWrite(addressWidth, dataWidth, withReady))
  override def newBypass() = bypasses.addRet(RegFileBypass(addressWidth, dataWidth))

  val logic = create late new Area{
    val regfile = new RegFileAsync(
      addressWidth    = addressWidth,
      dataWidth       = dataWidth,
      bankCount       = bankCount,
      readsParameter  = reads.map(e => RegFileReadParameter(withReady = e.withReady)),
      writesParameter = writes.map(e => RegFileWriteParameter(withReady = e.withReady)),
      bypasseCount    = bypasses.size
    )

    (regfile.io.reads, reads).zipped.foreach(_ <> _)
    (regfile.io.writes, writes).zipped.foreach(_ <> _)
    (regfile.io.bypasses, bypasses).zipped.foreach(_ <> _)
  }
}


