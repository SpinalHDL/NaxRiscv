package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.interfaces.{DecoderService, Encoding, EuGroup, ExecuteUnitService, RegfileService, RfResource, Riscv, RobService}
import spinal.lib.pipeline.Connection.DIRECT
import spinal.lib.pipeline._
import spinal.core._
import spinal.lib._
import naxriscv.utilities.Plugin
import spinal.lib.logic.{Masked, Symplify}
import spinal.lib.pipeline.Stageable

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, LinkedHashSet}






class DecoderPlugin() extends Plugin with DecoderService{
  val euToEncodings = LinkedHashMap[ExecuteUnitService, ArrayBuffer[Encoding]]()
  val encodings = LinkedHashSet[Encoding]()
  override def addFunction(fu: ExecuteUnitService, enc: Encoding) = {
    euToEncodings.getOrElseUpdate(fu, ArrayBuffer[Encoding]()) += enc
    encodings += enc
  }


  override def euGroups = logic.euGroups
  override def READ_RS(id: Int) : Stageable[Bool] = logic.regfiles.READ_RS(id)
  override def ARCH_RS(id: Int) : Stageable[UInt] = logic.regfiles.ARCH_RS(id)
  override def PHYS_RS(id: Int) : Stageable[UInt] = logic.regfiles.PHYS_RS(id)
  override def WRITE_RD : Stageable[Bool] = logic.regfiles.WRITE_RD
  override def ARCH_RD : Stageable[UInt] = logic.regfiles.ARCH_RD
  override def PHYS_RD : Stageable[UInt] = logic.regfiles.PHYS_RD
  override def PHYS_RD_FREE : Stageable[UInt] = logic.regfiles.PHYS_RD_FREE
  override def rsCount = 2
  override def rsPhysicalDepthMax = logic.regfiles.physicalMax

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.decompressed, frontend.pipeline.decoded)(new DIRECT)

    getService[RobService].retain()
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val rob = getService[RobService]

    val executionUnits = getServicesOf[ExecuteUnitService]
    val euGroups = ArrayBuffer[EuGroup]()

    def groupBy[T, K](that : Seq[T])(by : T => K) : LinkedHashMap[K, ArrayBuffer[T]] = {
      val ret = LinkedHashMap[K, ArrayBuffer[T]]()
      for(e <- that) {
        val k = by(e)
        ret.getOrElseUpdate(k, ArrayBuffer[T]()) += e
      }
      ret
    }

    //Create groups of similar execution units
    val euSimilar = groupBy(executionUnits)(eu => euToEncodings(eu))
    for((enc, eus) <- euSimilar){
      euGroups += EuGroup(
        eus.toList,
        Stageable(Bool()).setName(eus.map(_.euName()).mkString("_") + "_SEL"),
        encodings = enc
      )
    }

    //Find for each implemented encoding, which are the EuGroup implementing it
    val encToGroups = mutable.LinkedHashMap[Encoding, ArrayBuffer[EuGroup]]()
    for(g <- euGroups; enc <- g.encodings) encToGroups.getOrElseUpdate(enc, ArrayBuffer[EuGroup]()) += g

    //figure out EuGroup implementing common encodings
    val partitions = groupBy(encToGroups.toSeq)(_._2).map(e => e._1 -> e._2.map(_._1))
    for(p <- partitions){
      assert(DISPATCH_COUNT % p._1.map(_.eus.size).sum == 0, "Can't handle execution units partition with dynamic mapping")
    }

    for(groups <- encToGroups.values){
      assert(groups.size == 1, "Multiple groups implementing the same opcode not yet supported")
    }


    val regfiles = new Area {
      val plugins = getServicesOf[RegfileService]
      val physicalMax = plugins.map(_.getPhysicalDepth).max
      val ARCH_RS = List.fill(rsCount)(Stageable(UInt(5 bits)))
      val ARCH_RD = Stageable(UInt(5 bits))
      val PHYS_RS = List.fill(rsCount)(Stageable(UInt(log2Up(physicalMax) bits)))
      val PHYS_RD = Stageable(UInt(log2Up(physicalMax) bits))
      val PHYS_RD_FREE = Stageable(UInt(log2Up(physicalMax) bits))
      val READ_RS = List.fill(rsCount)(Stageable(Bool()))
      val WRITE_RD = Stageable(Bool)
    }

    val stage = frontend.pipeline.decoded
    import stage._


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
    }

    for (i <- 0 until Frontend.DECODE_COUNT) {
      implicit val offset = StageableOffset(i)
      val rdZero = INSTRUCTION_DECOMPRESSED(Riscv.rdRange) === 0
      regfiles.READ_RS(0) := Symplify(INSTRUCTION_DECOMPRESSED, encodings.readRs1, encodings.readRs1N)
      regfiles.READ_RS(1) := Symplify(INSTRUCTION_DECOMPRESSED, encodings.readRs2, encodings.readRs2N)
      regfiles.WRITE_RD   := Symplify(INSTRUCTION_DECOMPRESSED, encodings.writeRd, encodings.writeRdN) && !rdZero
      for (group <- euGroups) {
        group.sel := Symplify(INSTRUCTION_DECOMPRESSED, encodings.groups(group), encodings.groupsN(group))
      }
      DISPATCH_MASK := MASK_ALIGNED
    }

    val robCtx = new Area{
      val stage = frontend.pipeline.allocated

      def remapped[T <: Data](key : Stageable[T]) : Seq[T] = (0 until Frontend.DISPATCH_COUNT).map(stage(key, _))
      def writeLine[T <: Data](key : Stageable[T]) : Unit = writeLine(key, remapped(key))
      def writeLine[T <: Data](key : Stageable[T], value : Seq[T]) : Unit  = {
        rob.write(
          key = key,
          size = DISPATCH_COUNT,
          value = value,
          robId = stage(ROB_ID),
          enable = stage.isFireing
        )
      }

      writeLine(PC)
      writeLine(regfiles.WRITE_RD)
      writeLine(regfiles.PHYS_RD)
      writeLine(regfiles.PHYS_RD_FREE)
      writeLine(INSTRUCTION_DECOMPRESSED)
      writeLine(regfiles.ARCH_RD, remapped(INSTRUCTION_DECOMPRESSED).map(e => U(e(Riscv.rdRange))))

      for(i <- 0 until 2) {
        writeLine(regfiles.READ_RS(i))
        writeLine(regfiles.PHYS_RS(i))
        writeLine(regfiles.ARCH_RS(i), remapped(INSTRUCTION_DECOMPRESSED).map(e => U(e(Riscv.rsRange(i)))))
      }

      writeLine(DISPATCH_MASK)
    }

    rob.release()
    frontend.release()
  }
}