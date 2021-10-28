package naxriscv.utilities

import naxriscv.Global
import naxriscv.frontend.Frontend
import naxriscv.interfaces.{RegfileSpec, RfAllocationService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable.ArrayBuffer

case class AllocatorMultiPortPop[T <: Data](dataType : HardType[T], popCount : Int) extends Bundle with IMasterSlave {
  val mask = Bits(popCount bits)
  val ready = Bool()
  val fire = Bool()
  val values = Vec.fill(popCount)(dataType())

  override def asMaster() = {
    in(mask, fire)
    out(ready, values)
  }
}

case class AllocatorMultiPortMem[T <: Data](dataType : HardType[T],
                                            depth : Int,
                                            pushCount : Int,
                                            popCount : Int) extends Component{
  assert(isPow2(depth))
  assert(depth >= pushCount)
  assert(depth >= popCount)

  val io = new Bundle{
    val push = Vec.fill(pushCount)(slave(Flow(dataType())))
    val pop = master(AllocatorMultiPortPop(dataType, popCount))
  }
  val waysCount = 1 << log2Up(pushCount max popCount)
  val entriesPerWay = depth / waysCount

  val wayRange = log2Up(waysCount)-1 downto 0
  val addressRange = log2Up(depth)-1 downto log2Up(waysCount)

  val popMaskCount = CountOne(io.pop.mask)

  val ptr = new Area{
    val push, pop = Reg(UInt(log2Up(depth) + 1 bits)) init(0)
    push := push + CountOne(io.push.map(_.valid))
    when(io.pop.fire) {
      pop := pop + popMaskCount
    }
    val occupancy = push-pop
  }

  val pushArbitration = new Area{
    var pushOffset = ptr.push
    val push = for(i <- 0 until pushCount) yield new Area{
      val ptr = CombInit(pushOffset)
      pushOffset \= pushOffset + io.push(i).valid.asUInt
    }
  }


  val ways = for(i <- 0 until waysCount) yield new Area{
    val mem = Mem.fill(entriesPerWay)(dataType)
    val push = new Area{
      val offset = ptr.push + (waysCount-1-i)
      val hits = io.push.map(_.valid).asBits & pushArbitration.push.map(_.ptr(wayRange) === i).asBits
      mem.write(
        address = offset(addressRange),
        data = MuxOH(hits, io.push.map(_.payload)),
        enable = hits.orR
      )
    }

    val pop = new Area{
      val offset = ptr.pop + (waysCount-1-i)
      val data = mem.readAsync(offset(addressRange))
    }
  }


  val popArbitration = new Area{
    var popOffset = ptr.pop
    val pop = for(i <- 0 until popCount) yield new Area{
      val ptr = CombInit(popOffset)
      io.pop.values(i) := ways.map(_.pop.data).read(ptr(wayRange))
      popOffset \= popOffset + io.pop.mask(i).asUInt
    }
  }
  io.pop.ready := ptr.occupancy >= popMaskCount
}

object AllocatorMultiPortSynth extends App{
  LutInputs.set(6)

  val rtls = ArrayBuffer[Rtl]()
  rtls += Rtl(SpinalVerilog(Rtl.ffIo(AllocatorMultiPortMem(
    dataType  = UInt(6 bits),
    depth     = 64,
    pushCount = 2,
    popCount  = 2
  ))))
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)
}
