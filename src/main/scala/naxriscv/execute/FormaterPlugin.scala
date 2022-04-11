package naxriscv.execute

import naxriscv.{DecodeList, Global}
import naxriscv.interfaces.{MicroOp, RD}
import naxriscv.riscv.IntRegFile
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntFormatPlugin(val euId : String) extends Plugin{
  withPrefix(euId)

  case class SignExtend(op : MicroOp, bitId : Int)
  case class Spec(port : Flow[Bits],
                  stageId : Int,
                  writeLatency : Int){
    val signExtends = ArrayBuffer[SignExtend]()
  }
  val portToSpec = mutable.LinkedHashMap[Flow[Bits],Spec]()

  def access(stageId : Int, writeLatency : Int) : Flow[Bits] = {
    val port = Flow(Bits(Global.XLEN bits))
    portToSpec(port) = Spec(port, stageId, writeLatency)
    port
  }

  def signExtend(port : Flow[Bits], microOp: MicroOp, bitId : Int) = {
    portToSpec(port).signExtends += SignExtend(microOp, bitId)
  }

  val setup = create early new Area{
    val eu = findService[ExecutionUnitBase](_.euId == euId)
    eu.retain()
  }

  val logic = create late new Area{
    val s = setup.get
    import s._
    val specs = portToSpec.values
    val grouped = specs.groupByLinked(_.stageId)
    val stages = for(group <- grouped.values) yield new Area{
      val writeLatency = group.map(_.writeLatency).min
      val stageId = group.head.stageId

      val wb = eu.newWriteback(IntRegFile, RD, eu.getExecute(stageId), writeLatency)
      val hits = B(group.map(_.port.valid))
      wb.valid := hits.orR

      val signExtendsGroups = group.flatMap(_.signExtends).groupByLinked(_.bitId).map(_._2)
      val raw = MuxOH.or(hits, group.map(_.port.payload), true)
      wb.payload := raw

      val signExtend = for(seg <- signExtendsGroups) yield new Area{
        val bitId = seg.head.bitId
        val DO_IT = Stageable(Bool())
        eu.setDecodingDefault(DO_IT, False)
        for(e <- seg){
          eu.addDecoding(e.op, DecodeList(DO_IT -> True))
        }
        when(eu.getExecute(stageId)(DO_IT)){
          wb.payload(Global.XLEN.get-1 downto bitId+1) := (default -> raw(bitId))
        }
      }
    }

    eu.release()
  }
}
