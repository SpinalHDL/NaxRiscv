package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{DecoderService, Encoding, EuGroup, ExecuteUnitService, RegfileService, RfResource, Riscv}
import spinal.lib.pipeline.Connection.DIRECT
import spinal.lib.pipeline._
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.logic.{Masked, Symplify}
import spinal.lib.pipeline.Stageable

import scala.collection.mutable._




//case class DecoderParameter(decoderWidth : Int,
//                            instructionWidth : Int)  {
//
//}

//case class DecompressedInstruction() extends Bundle {
//  val valid = Bool()
//  val instruction = InstructionWidth.craft()
//}
//
//
//case class DecoderIo() extends Bundle {
//  val instructions = slave Stream(Vec.fill(DecoderWidth)(DecompressedInstruction()))
//}
//
//case class Decoder() extends Component {
//
//}



class DecoderPlugin() extends Plugin with DecoderService{

//  val defaults = LinkedHashMap[Stageable[_ <: BaseType], BaseType]()
//  val encodings = LinkedHashMap[MaskedLiteral,ArrayBuffer[(Stageable[_ <: BaseType], BaseType)]]()
//
//  override def add(encoding: Seq[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]): Unit = encoding.foreach(e => this.add(e._1,e._2))
//  override def add(key: MaskedLiteral, values: Seq[(Stageable[_ <: BaseType], Any)]): Unit = {
//    val instructionModel = encodings.getOrElseUpdate(key,ArrayBuffer[(Stageable[_ <: BaseType], BaseType)]())
//    values.map{case (a,b) => {
//      assert(!instructionModel.contains(a), s"Over specification of $a")
//      val value = b match {
//        case e: SpinalEnumElement[_] => e()
//        case e: BaseType => e
//      }
//      instructionModel += (a->value)
//    }}
//  }
//
//  override def addDefault(key: Stageable[_  <: BaseType], value: Any): Unit = {
//    assert(!defaults.contains(key))
//    defaults(key) = value match{
//      case e : SpinalEnumElement[_] => e()
//      case e : BaseType => e
//    }
//  }
//  override def add(key: MaskedLiteral, values: Seq[(Stageable[_ <: BaseType], Any)]) = ???
//  override def add(encoding: Seq[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]) = ???
//  override def addDefault(key: Stageable[_ <: BaseType], value: Any) = ???

  val euToEncodings = LinkedHashMap[ExecuteUnitService, ArrayBuffer[Encoding]]()
  override def addFunction(fu: ExecuteUnitService, enc: Encoding) = {
    euToEncodings.getOrElseUpdate(fu, ArrayBuffer[Encoding]()) += enc
  }


  override def euGroups = logic.euGroups
  override def READ_RS(id: Int) = logic.regfiles.READ_RS(id)
  override def PHYSICAL_RS(id: Int) = logic.regfiles.ARCH_RS(id)
  override def WRITE_RD = logic.regfiles.WRITE_RD
  override def PHYSICAL_RD = logic.regfiles.ARCH_RD
  override def rsCount = logic.regfiles.rsCount
  override def rsPhysicalDepthMax = logic.regfiles.physicalMax

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.decompressed, frontend.pipeline.decoded)(new DIRECT)
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]

    val executionUnits = getServicesOf[ExecuteUnitService]
    val euGroups = ArrayBuffer[EuGroup]()
    //TODO merges
    for(eu <- executionUnits){
      euGroups += EuGroup(
        Seq(eu),
        Stageable(Bool()).setName(eu.euName() + "_SEL")
      )
    }

    val regfiles = new Area {
      val plugins = getServicesOf[RegfileService]
      val physicalMax = plugins.map(_.getPhysicalDepth).max
      val rsCount = 2
      val ARCH_RS = List.fill(rsCount)(Stageable(UInt(log2Up(physicalMax) bits)))
      val ARCH_RD = Stageable(UInt(log2Up(physicalMax) bits))
      val READ_RS = List.fill(rsCount)(Stageable(Bool()))
      val WRITE_RD = Stageable(Bool)
    }

    val stage = frontend.pipeline.decoded
    import stage._

//    val spec = LinkedHashMap[Masked, Masked]()
//    for(e <- encodings){
//      val key = Masked(e.key)
//      val value =
//    }

    val encodings = new Area{
      val encs = LinkedHashSet[Encoding]() ++ euToEncodings.flatMap(_._2)
      val all = LinkedHashSet[Masked]()
      val readRs1, readRs2, writeRd = LinkedHashSet[Masked]()
      for(e <- encs){
        val masked = Masked(e.key)
        all += masked
        e.ressources.foreach {
          case r: RfResource => r.enc match {
            case Riscv.RS1 => readRs1 += masked
            case Riscv.RS2 => readRs2 += masked
            case Riscv.RD => writeRd += masked
          }
        }
      }
      val readRs1N = all -- readRs1
      val readRs2N = all -- readRs2
      val writeRdN = all -- writeRd

      val groups, groupsN = LinkedHashMap[EuGroup, LinkedHashSet[Masked]]()
      for(group <- euGroups){
        val addTo = groups.getOrElseUpdate(group, LinkedHashSet[Masked]())
        for(eu <- group.eus){
          for(enc <- euToEncodings(eu)){
            addTo += Masked(enc.key)
          }
        }
        groupsN(group) = all -- addTo
      }
//      for((eu, encs) <- euToEncodings){
//        val e = groups.getOrElseUpdate()
//      }
    }

    for (i <- 0 until Frontend.DECODE_COUNT) {
      implicit val offset = StageableOffset(i)
      regfiles.READ_RS(0) := Symplify(INSTRUCTION_DECOMPRESSED, encodings.readRs1, encodings.readRs1N)
      regfiles.READ_RS(1) := Symplify(INSTRUCTION_DECOMPRESSED, encodings.readRs2, encodings.readRs2N)
      regfiles.WRITE_RD   := Symplify(INSTRUCTION_DECOMPRESSED, encodings.writeRd, encodings.writeRdN)
      for (group <- euGroups) {
        group.sel := Symplify(INSTRUCTION_DECOMPRESSED, encodings.groups(group), encodings.groupsN(group))
      }
      DISPATCH_MASK := MASK_ALIGNED
    }

    frontend.release()
  }
}