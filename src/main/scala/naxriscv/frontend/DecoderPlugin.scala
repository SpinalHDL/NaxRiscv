// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.frontend

import naxriscv._
import naxriscv.Global._
import naxriscv.Frontend._
import naxriscv.Fetch._
import naxriscv.execute.fpu.FpuWriteback
import naxriscv.interfaces.{AddressTranslationService, CommitService, DecoderService, DecoderTrap, EuGroup, ExecuteUnitService, FPU, INSTRUCTION_SIZE, LockedImpl, MicroOp, PC_READ, PrivilegedService, RD, RM, RS1, RS2, RS3, RegFileSel, RegfileService, RegfileSpec, Resource, RfRead, RfResource, RobService, SingleDecoding}
import naxriscv.prediction.DecoderPrediction
import naxriscv.riscv.{CSR, Const, FloatRegFile, IntRegFile, Rvfd}
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


class DecoderPlugin(val xlen : Int) extends Plugin with DecoderService with LockedImpl{
  import DecoderPlugin.OP_ID

  create config{
    XLEN.set(xlen)
  }

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

  val keysToPushInRob = mutable.LinkedHashSet[Stageable[_ <: BaseType]]()
  override def addDecodingToRob(key: Stageable[_ <: BaseType]) = {
    keysToPushInRob += key
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
  override def rsCount(rf : RegfileSpec) = rf match {
    case IntRegFile => 2
    case FloatRegFile => 3
  }
  override def rsCountMax() = 2+RVF.get.toInt
  override def rsPhysicalDepthMax = setup.keys.physicalMax
  override def getTrap() = setup.exceptionPort
  override def trapHalt() = setup.trapHalt := True
  override def trapRaise() = setup.trapRaise := True
  override def trapReady() = setup.trapReady

  override def REGFILE_RD = setup.keys.REGFILE_RD
  override def REGFILE_RS(id: Int) : RegFileSel = setup.keys.REGFILE_RS(id)
  override def REGFILE_RS(id: RfRead) : RegFileSel = REGFILE_RS(rsToId(id))

  override def debugEnter(slotId: Int) = {
    setup.debugEnter(slotId) := True
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    frontend.retain()
    frontend.pipeline.connect(frontend.pipeline.decompressed, frontend.pipeline.decoded)(DIRECT())
    frontend.pipeline.connect(frontend.pipeline.decoded, frontend.pipeline.serialized)(M2S())

    getService[RobService].retain()

    val exceptionPort = Flow(DecoderTrap())
    val trapHalt = False
    val trapRaise = False
    val trapReady = Bool()
    val debugEnter = Vec.fill(DECODE_COUNT)(False)
    
    val keys = new Area {
      setName("")
      val plugins = getServicesOf[RegfileService]
      val physicalMax = plugins.map(_.getPhysicalDepth).max
      val ARCH_RS = List.fill(rsCountMax())(Stageable(UInt(5 bits)))
      val ARCH_RD = Stageable(UInt(5 bits))
      val PHYS_RS = List.fill(rsCountMax())(Stageable(UInt(log2Up(physicalMax) bits)))
      val PHYS_RD = Stageable(UInt(log2Up(physicalMax) bits))
      val PHYS_RD_FREE = Stageable(UInt(log2Up(physicalMax) bits))
      val READ_RS = List.fill(rsCountMax())(Stageable(Bool()))
      val WRITE_RD = Stageable(Bool())
      val regfileSelWidth = log2Up(getServicesOf[RegfileService].size)
      def regFileSelType() = RegFileSel(getServicesOf[RegfileService].zipWithIndex.map(e => e._2 -> e._1.rfSpec).toMapLinked(), regfileSelWidth)
      val REGFILE_RD = regFileSelType()
      val REGFILE_RS =  List.fill(rsCountMax())(regFileSelType())
      val LEGAL = Stageable(Bool())
      val TRAP = Stageable(Bool())
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
      val readRs1, readRs2, readRs3, writeRd, fpSpec, rmSpec = new DecodingSpec(Bool()).setDefault(zero)
      val regfileRs1, regfileRs2, regfileRs3, regfileRd = new DecodingSpec(REGFILE_RD())
      val resourceToSpec = resourceToStageable.keys.map(_ -> new DecodingSpec(Bool()).setDefault(zero)).toMap
      val regfileSelMask = (1 << setup.keys.regfileSelWidth)-1
      var withRs3 = false
      for(e <- singleDecodings){
        val key = Masked(e.key)
        all += key
        e.resources.foreach {
          case r: RfResource => r.access match {
            case RS1 => {
              readRs1.addNeeds(key, one)
              regfileRs1.addNeeds(key, Masked(REGFILE_RS(0).rfToId(r.rf), regfileSelMask))
            }
            case RS2 => {
              readRs2.addNeeds(key, one)
              regfileRs2.addNeeds(key, Masked(REGFILE_RS(1).rfToId(r.rf), regfileSelMask))
            }
            case RS3 => {
              readRs3.addNeeds(key, one)
              regfileRs3.addNeeds(key, Masked(REGFILE_RS(2).rfToId(r.rf), regfileSelMask))
              withRs3 = true
            }
            case RD =>  {
              writeRd.addNeeds(key, one)
              regfileRd.addNeeds(key, Masked(REGFILE_RD.rfToId(r.rf), regfileSelMask))
            }
          }
          case PC_READ =>
          case INSTRUCTION_SIZE =>
          case FPU => fpSpec.addNeeds(key, one)
          case RM => rmSpec.addNeeds(key, one)
          case r if resourceToStageable.contains(r) => resourceToSpec(r).addNeeds(key, one)
          case naxriscv.interfaces.SQ =>
          case naxriscv.interfaces.LQ =>
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

    val slots = for (i <- 0 until Frontend.DECODE_COUNT)  yield new Area {
      implicit val offset = StageableOffset(i)
      val rdZero = INSTRUCTION_DECOMPRESSED(Const.rdRange) === 0
      setup.keys.LEGAL := Symplify(INSTRUCTION_DECOMPRESSED, encodings.all) && !INSTRUCTION_ILLEGAL

      val fp = RVF.get generate new Area{
        val useFpu = encodings.fpSpec.build(INSTRUCTION_DECOMPRESSED, encodings.all)
        useFpu.setWhen(INSTRUCTION_DECOMPRESSED === M"0000000000---------------1110011" && INSTRUCTION_DECOMPRESSED(12, 2 bits) =/= 0 && INSTRUCTION_DECOMPRESSED(20, 2 bits) =/= 0)
        val useRm = encodings.rmSpec.build(INSTRUCTION_DECOMPRESSED, encodings.all)
        val csrRm = getService[FpuWriteback].getRoundingMode()
        val instRm = INSTRUCTION_DECOMPRESSED(Const.funct3Range)
        val rm = U((instRm === 7) ? csrRm | instRm)
        val enabled = getService[PrivilegedService].isFpuEnabled()
        val triggered = useFpu && !enabled || useRm && rm >= 5
        when(triggered){
          setup.keys.LEGAL := False
        }
      }


      setup.keys.REGFILE_RS(0) := encodings.regfileRs1.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      setup.keys.REGFILE_RS(1) := encodings.regfileRs2.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      if(encodings.withRs3) setup.keys.REGFILE_RS(2) := encodings.regfileRs3.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      setup.keys.REGFILE_RD := encodings.regfileRd.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      setup.keys.READ_RS(0) := encodings.readRs1.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      setup.keys.READ_RS(1) := encodings.readRs2.build(INSTRUCTION_DECOMPRESSED, encodings.all)
      if(encodings.withRs3) setup.keys.READ_RS(2) := encodings.readRs3.build(INSTRUCTION_DECOMPRESSED, encodings.all)

      val x0AlwaysZero = setup.keys.REGFILE_RD.muxListDc(REGFILE_RD.idToRf.toSeq.map(e => e._1 -> Bool(e._2.x0AlwaysZero)))
      setup.keys.WRITE_RD   := encodings.writeRd.build(INSTRUCTION_DECOMPRESSED, encodings.all) && !(rdZero && x0AlwaysZero)
      for (group <- euGroups) {
        group.sel := encodings.groups(group).build(INSTRUCTION_DECOMPRESSED, encodings.all)
      }
      for((r, s) <- resourceToStageable){
        s := encodings.resourceToSpec(r).build(INSTRUCTION_DECOMPRESSED, encodings.all)
      }
      setup.keys.TRAP := MASK_ALIGNED && (!setup.keys.LEGAL || FETCH_FAULT || setup.trapRaise || setup.debugEnter(i))
      DECODED_MASK := MASK_ALIGNED && !stage(0 to i)(setup.keys.TRAP).orR
      if(!isServiceAvailable[DecoderPrediction]) DISPATCH_MASK := DECODED_MASK

      MICRO_OP := INSTRUCTION_DECOMPRESSED
      terminal(INSTRUCTION_DECOMPRESSED, i)

      setup.keys.ARCH_RD := U(INSTRUCTION_DECOMPRESSED(Const.rdRange))
      for(i <- 0 until rsCountMax()) {
        setup.keys.ARCH_RS(i) := U(INSTRUCTION_DECOMPRESSED(Const.rsRange(i)))
      }
    }

    val exception = new Area{
      val set = isFireing && getAll(setup.keys.TRAP).orR
      val clear = CombInit(isFlushed)
      val trigged = RegInit(False) setWhen(set) clearWhen(clear)
      def getAll[T <: Data](that : Stageable[T]) = Vec(stage(0 until  DECODE_COUNT)(that))
      val exceptionReg = RegNextWhen(getAll(setup.keys.TRAP), !trigged)
      val fetchFaultReg = RegNextWhen(getAll(FETCH_FAULT), !trigged)
      val fetchFaultPageReg = RegNextWhen(getAll(FETCH_FAULT_PAGE), !trigged)
      val fetchFaultSliceReg = RegNextWhen(getAll(FETCH_FAULT_SLICE), !trigged)
      val debugEnterReg = RegNextWhen(setup.debugEnter, !trigged)
      val epcReg   = RegNextWhen(getAll(PC), !trigged)
      val instReg = RegNextWhen(getAll(INSTRUCTION_ALIGNED), !trigged)
      val oh = OHMasking.first(exceptionReg)

      val fetchFault      = OHMux.or(oh, fetchFaultReg)
      val fetchFaultPage  = OHMux.or(oh, fetchFaultPageReg)
      val fetchFaultSlice = OHMux.or(oh, fetchFaultSliceReg)
      val debugEnter      = OHMux.or(oh, debugEnterReg)
      val pc              = OHMux.or(oh, epcReg)

      val pipelineEmpty = !frontend.isBusyAfterDecode() && commit.isRobEmpty
      val doIt = trigged && pipelineEmpty
      val doItAgain = RegNext(doIt) init(False) //As the fetch stage isn't halted durring the first doit, we clean up a eventualy fetch stage(1)

      flushIt(doIt || doItAgain)
      haltIt(trigged)
      haltIt(setup.trapHalt && !setup.trapRaise)

      setup.trapReady := isValid && pipelineEmpty
      setup.exceptionPort.valid := doIt
      setup.exceptionPort.epc   := pc
      if(RV_DEBUG) setup.exceptionPort.debugEnter := debugEnter
      when(fetchFault){
        when(fetchFaultPage){
          setup.exceptionPort.cause := CSR.MCAUSE_ENUM.INSTRUCTION_PAGE_FAULT
        } otherwise {
          setup.exceptionPort.cause := CSR.MCAUSE_ENUM.INSTRUCTION_ACCESS_FAULT
        }
        setup.exceptionPort.tval  := B(pc + (fetchFaultSlice << SLICE_RANGE_LOW)).resized //TODO PC sign extends ?
      } otherwise {
        setup.exceptionPort.cause := CSR.MCAUSE_ENUM.ILLEGAL_INSTRUCTION
        setup.exceptionPort.tval  := OHMux.or(oh, instReg).resized
      }
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
      writeLine(setup.keys.REGFILE_RD)

      for(i <- 0 until rsCountMax()) {
        writeLine(setup.keys.READ_RS(i))
        writeLine(setup.keys.PHYS_RS(i))
        writeLine(setup.keys.ARCH_RS(i))
        writeLine(setup.keys.REGFILE_RS(i))
      }

      writeLine(DISPATCH_MASK)

      for(e <- keysToPushInRob) writeLine(e)
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
        Verilator.public(stage(DECODED_MASK, slotId))
      }

      doc.property("DECODE_COUNT", DECODE_COUNT.get)
      doc.property("XLEN", XLEN.get)
    }

    rob.release()
    frontend.release()
  }
}