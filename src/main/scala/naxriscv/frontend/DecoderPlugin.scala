package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.Fetch._
import naxriscv.interfaces.{AddressTranslationService, CommitService, DecoderException, DecoderService, EuGroup, ExecuteUnitService, INSTRUCTION_SIZE, LockedImpl, MicroOp, PC_READ, RD, RS1, RS2, RS3, RegfileService, Resource, RfRead, RfResource, RobService, SingleDecoding}
import naxriscv.prediction.DecoderPrediction
import naxriscv.riscv.{CSR, Const}
import spinal.lib.pipeline.Connection.{DIRECT, M2S}
import spinal.lib.pipeline._
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{DocPlugin, Plugin}
import spinal.lib.logic.{DecodingSpec, Masked, Symplify, SymplifyBit}
import spinal.lib.pipeline.Stageable

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, LinkedHashSet}



object DecoderPlugin extends AreaRoot{
  val OP_ID = Stageable(UInt(12 bits))
}


class DecoderPlugin() extends Plugin with DecoderService with LockedImpl{
  import DecoderPlugin.OP_ID

  val euToMicroOps = LinkedHashMap[ExecuteUnitService, ArrayBuffer[MicroOp]]()
  val microOps = LinkedHashSet[MicroOp]()
  val singleDecodings = LinkedHashSet[SingleDecoding]()
  val resourceToStageable = LinkedHashMap[Resource, Stageable[Bool]]()

  override def addEuOp(fu: ExecuteUnitService, microOp: MicroOp) = {
    euToMicroOps.getOrElseUpdate(fu, ArrayBuffer[MicroOp]()) += microOp
    microOps += microOp
    microOp match {
      case sd : SingleDecoding => singleDecodings += sd
      case _ =>
    }
  }


  override def addResourceDecoding(resource: Resource, stageable: Stageable[Bool]) = resourceToStageable(resource) = stageable
  override def covers() = logic.encodings.all.toList


  val decodingSpecs = mutable.LinkedHashMap[Stageable[_ <: BaseType], DecodingSpec[_ <: BaseType]]()
  def getDecodingSpec(key : Stageable[_ <: BaseType]) = decodingSpecs.getOrElseUpdate(key, new DecodingSpec(key))
  def setDecodingDefault(key : Stageable[_ <: BaseType], value : BaseType) : Unit = {
    getDecodingSpec(key).setDefault(Masked(value))
  }
  override def addMicroOpDecoding(microOp: MicroOp, decoding: DecodeListType) = {
    val op = Masked(microOp.key)
    for((key, value) <- decoding) {
      getDecodingSpec(key).addNeeds(op, Masked(value))
    }
  }

  override def addMicroOpDecodingDefault(key: Stageable[_ <: BaseType], value: BaseType) = {
    getDecodingSpec(key).setDefault(Masked(value))
  }

  def rsToId(id : RfRead) = id match {
    case RS1 => 0
    case RS2 => 1
    case RS3 => 2
  }

  override def euGroups = logic.euGroups
  override def READ_RS(id: Int) : Stageable[Bool] = setup.keys.READ_RS(id)
  override def ARCH_RS(id: Int) : Stageable[UInt] = setup.keys.ARCH_RS(id)
  override def PHYS_RS(id: Int) : Stageable[UInt] = setup.keys.PHYS_RS(id)
  override def READ_RS(id: RfRead) : Stageable[Bool] = READ_RS(rsToId(id))
  override def ARCH_RS(id: RfRead) : Stageable[UInt] = ARCH_RS(rsToId(id))
  override def PHYS_RS(id: RfRead) : Stageable[UInt] = PHYS_RS(rsToId(id))
  override def WRITE_RD : Stageable[Bool] = setup.keys.WRITE_RD
  override def ARCH_RD : Stageable[UInt] = setup.keys.ARCH_RD
  override def PHYS_RD : Stageable[UInt] = setup.keys.PHYS_RD
  override def PHYS_RD_FREE : Stageable[UInt] = setup.keys.PHYS_RD_FREE
  override def rsCount = 2
  override def rsPhysicalDepthMax = setup.keys.physicalMax
  override def getException() = setup.exceptionPort

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.decompressed, frontend.pipeline.decoded)(DIRECT())
    frontend.pipeline.connect(frontend.pipeline.decoded, frontend.pipeline.serialized)(M2S())

    getService[RobService].retain()

    val exceptionPort = Flow(DecoderException())
    val keys = new Area {
      setName("")
      val plugins = getServicesOf[RegfileService]
      val physicalMax = plugins.map(_.getPhysicalDepth).max
      val ARCH_RS = List.fill(rsCount)(Stageable(UInt(5 bits)))
      val ARCH_RD = Stageable(UInt(5 bits))
      val PHYS_RS = List.fill(rsCount)(Stageable(UInt(log2Up(physicalMax) bits)))
      val PHYS_RD = Stageable(UInt(log2Up(physicalMax) bits))
      val PHYS_RD_FREE = Stageable(UInt(log2Up(physicalMax) bits))
      val READ_RS = List.fill(rsCount)(Stageable(Bool()))
      val WRITE_RD = Stageable(Bool())
      val LEGAL = Stageable(Bool())
      val EXCEPTION = Stageable(Bool())
    }
  }


  val logic = create late new Area{
    lock.await()

    val frontend = getService[FrontendPlugin]
    val rob = getService[RobService]
    val doc = getService[DocPlugin]
    val commit = getService[CommitService]

    val executionUnits = getServicesOf[ExecuteUnitService]
    val euGroups = ArrayBuffer[EuGroup]()



    //Create groups of similar execution units
    val euSimilar = executionUnits.groupByLinked(eu => euToMicroOps(eu))
    for((microOps, eus) <- euSimilar){
      euGroups += EuGroup(
        eus.toList,
        Stageable(Bool()).setName(eus.map(_.euName()).mkString("_") + "_SEL"),
        microOps = microOps
      )
    }

    //Find for each implemented microop, which are the EuGroup implementing it
    val microOpToGroups = mutable.LinkedHashMap[MicroOp, ArrayBuffer[EuGroup]]()
    for(g <- euGroups; microOps <- g.microOps) microOpToGroups.getOrElseUpdate(microOps, ArrayBuffer[EuGroup]()) += g

    //figure out EuGroup implementing common microOps
    val partitions = microOpToGroups.toSeq.groupByLinked(_._2).map(e => e._1 -> e._2.map(_._1))
    for(p <- partitions){
      assert(DISPATCH_COUNT % p._1.map(_.eus.size).sum == 0, "Can't handle execution units partition with dynamic mapping")
    }

    for(groups <- microOpToGroups.values){
      assert(groups.size == 1, "Multiple groups implementing the same opcode not yet supported")
    }


    val stage = frontend.pipeline.decoded
    import stage._


    val encodings = new Area{
      val all = LinkedHashSet[Masked]()
      val one = Masked(1,1)
      val zero = Masked(0,1)
      val readRs1, readRs2, writeRd = new DecodingSpec(Bool()).setDefault(zero)
      val resourceToSpec = resourceToStageable.keys.map(_ -> new DecodingSpec(Bool()).setDefault(zero)).toMap
      for(e <- singleDecodings){
        val key = Masked(e.key)
        all += key
        e.resources.foreach {
          case r: RfResource => r.access match {
            case RS1 => readRs1.addNeeds(key, one)
            case RS2 => readRs2.addNeeds(key, one)
            case RD =>  writeRd.addNeeds(key, one)
          }
          case PC_READ =>
          case INSTRUCTION_SIZE =>
          case r if resourceToStageable.contains(r) => resourceToSpec(r).addNeeds(key, one)
        }
      }

      val groups = LinkedHashMap[EuGroup, DecodingSpec[Bool]]()
      for(group <- euGroups){
        val addTo = groups.getOrElseUpdate(group, new DecodingSpec(Bool()).setDefault(zero))
        for(eu <- group.eus){
          for(enc <- euToMicroOps(eu)) enc match {
            case sd : SingleDecoding => addTo.addNeeds(Masked(sd.key), one)
          }
        }
      }
    }

    for (i <- 0 until Frontend.DECODE_COUNT) {
      implicit val offset = StageableOffset(i)
      val rdZero = INSTRUCTION_DECOMPRESSED(Const.rdRange) === 0
      setup.keys.LEGAL := Symplify.logicOf(INSTRUCTION_DECOMPRESSED, SymplifyBit.getPrimeImplicantsByTrueAndDontCare(encodings.all.toSeq, Nil, INSTRUCTION_WIDTH))
      setup.keys.READ_RS(0) := encodings.readRs1.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      setup.keys.READ_RS(1) := encodings.readRs2.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      setup.keys.WRITE_RD   := encodings.writeRd.build(INSTRUCTION_DECOMPRESSED, encodings.all) && !rdZero
      for (group <- euGroups) {
        group.sel := encodings.groups(group).build(INSTRUCTION_DECOMPRESSED, encodings.all)
      }
      for((r, s) <- resourceToStageable){
        s := encodings.resourceToSpec(r).build(INSTRUCTION_DECOMPRESSED, encodings.all)
      }
      setup.keys.EXCEPTION := MASK_ALIGNED && !setup.keys.LEGAL
      DECODED_MASK := MASK_ALIGNED && !stage(0 to i)(setup.keys.EXCEPTION).orR
      if(!isServiceAvailable[DecoderPrediction]) DISPATCH_MASK := DECODED_MASK

      MICRO_OP := INSTRUCTION_DECOMPRESSED
      terminal(INSTRUCTION_DECOMPRESSED, i)

      setup.keys.ARCH_RD := U(INSTRUCTION_DECOMPRESSED(Const.rdRange))
      for(i <- 0 until rsCount) {
        setup.keys.ARCH_RS(i) := U(INSTRUCTION_DECOMPRESSED(Const.rsRange(i)))
      }
    }

    val exception = new Area{
      val set = isFireing && getAll(setup.keys.EXCEPTION).orR
      val clear = CombInit(isFlushed)
      val trigged = RegInit(False) setWhen(set) clearWhen(clear)
      def getAll[T <: Data](that : Stageable[T]) = Vec(stage(0 until  DECODE_COUNT)(that))
      val exceptionReg = RegNextWhen(getAll(setup.keys.EXCEPTION), !trigged)
      val epcReg   = RegNextWhen(getAll(PC), !trigged)
      val instReg = RegNextWhen(getAll(INSTRUCTION_ALIGNED), !trigged)
      val oh = OHMasking.first(exceptionReg)

      val doIt = trigged && !frontend.isBusyAfterDecode() && commit.isRobEmpty
      flushIt(doIt)
      haltIt(trigged)

      setup.exceptionPort.valid := doIt
      setup.exceptionPort.cause := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
      setup.exceptionPort.epc   := OHMux.or(oh, epcReg)
      setup.exceptionPort.tval  := OHMux.or(oh, instReg)
    }

    val microOpDecoding = new Area{
      val stage = frontend.pipeline.serialized
      import stage._
      for(slotId <- 0 until DISPATCH_COUNT) {
        implicit val _ = StageableOffset(slotId)
        val coverAll = microOps.map(e => Masked(e.key))
        for ((key, spec) <- decodingSpecs) {
          key.assignFromBits(spec.build(Frontend.MICRO_OP, coverAll).asBits)
        }
      }
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
          robId = stage(ROB.ID),
          enable = stage.isFireing
        )
      }

      writeLine(PC)
      writeLine(MICRO_OP)
      writeLine(INSTRUCTION_SLICE_COUNT)
      writeLine(setup.keys.WRITE_RD)
      writeLine(setup.keys.PHYS_RD)
      writeLine(setup.keys.PHYS_RD_FREE)
      writeLine(setup.keys.ARCH_RD)

      for(i <- 0 until rsCount) {
        writeLine(setup.keys.READ_RS(i))
        writeLine(setup.keys.PHYS_RS(i))
        writeLine(setup.keys.ARCH_RS(i))
      }

      writeLine(DISPATCH_MASK)
    }

    val whitebox = new Area{
      Verilator.public(stage.isFireing)

      Verilator.public(OP_ID.setAsReg().init(0))
      OP_ID := OP_ID + (U(isFireing) << log2Up(DECODE_COUNT))
      assert(isPow2(DECODE_COUNT.get))

      Verilator.public(frontend.pipeline.allocated(OP_ID))
      for(slotId <- 0 until DECODE_COUNT) {
        Verilator.public(stage(INSTRUCTION_DECOMPRESSED, slotId))
        Verilator.public(stage(PC, slotId))
      }

      doc.property("DECODE_COUNT", DECODE_COUNT.get)
      doc.property("XLEN", XLEN.get)
    }

    rob.release()
    frontend.release()
  }
}