package naxriscv.frontend

import naxriscv.Fetch._
import naxriscv.Frontend.{DISPATCH_COUNT, DISPATCH_MASK}
import naxriscv.backend.BranchContextPlugin
import naxriscv.fetch.{AlignerPlugin, FetchPlugin, PcPlugin}
import naxriscv.{Frontend, Global, ROB}
import naxriscv.interfaces.{AddressTranslationService, CommitService, JumpService, RobService}
import naxriscv.riscv.{IMM, Rvi}
import spinal.core._
import spinal.lib._
import naxriscv.utilities.{NaxParameter, Plugin}
import spinal.lib.logic.{DecodingSpec, DecodingSpecExample, Masked}
import spinal.lib.pipeline.{Stageable, StageableOffset}

class PredictorPlugin() extends Plugin{
  val btbJumpAt = 1
  val branchHistoryFetchAt = 1

  val keys = create early new AreaRoot{
    val BRANCH_CONDITIONAL = Stageable(Bool())
    val BRANCH_HISTORY_WIDTH = 16 //TODO
    val BRANCH_HISTORY = Stageable(Bits(BRANCH_HISTORY_WIDTH bits))

    val GSHARE_TAKE_IT = Stageable(Bits(SLICE_COUNT bits))
    val GSHARE_STRONG  = Stageable(Bits(SLICE_COUNT bits))
  }

  val setup = create early new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val jump = getService[JumpService]
    val rob = getService[RobService]
    val aligner = getService[AlignerPlugin]
    val PC = getService[AddressTranslationService].PC
    val decodeJump = jump.createJumpInterface(JumpService.Priorities.DECODE_PREDICTION)
    val btbJump = jump.createJumpInterface(JumpService.Priorities.FETCH_WORD(btbJumpAt, true))
    frontend.retain()
    fetch.retain()
    rob.retain()

    aligner.addWordContext(
      keys.GSHARE_TAKE_IT,
      keys.GSHARE_STRONG,
      keys.BRANCH_HISTORY
    )
  }

  val logic = create late new Area{
    val frontend = getService[FrontendPlugin]
    val fetch = getService[FetchPlugin]
    val decoder = getService[DecoderPlugin]
    val rob = getService[RobService]
    val commit = getService[CommitService]
    val branchContext = getService[BranchContextPlugin]
    val PC = getService[AddressTranslationService].PC
    val ak = getService[AlignerPlugin].keys.get
    import ak._

    val FETCH_PC = fetch.keys.FETCH_PC_PRE_TRANSLATION

    val sliceShift = if(RVC) 1 else 2

    val learnContext = new Area{
      val history = Mem.fill(branchContext.branchCount)(keys.BRANCH_HISTORY)
      val conditionalBranch = Mem.fill(branchContext.branchCount)(Bool())
    }

    val branchHistory = new Area{
      val onCommit = new Area {
        val value = Reg(keys.BRANCH_HISTORY) init (0)

        val event = commit.onCommit()
        val isConditionalBranch = rob.readAsync(keys.BRANCH_CONDITIONAL, Global.COMMIT_COUNT, event.robId)
        val isTaken = rob.readAsync(branchContext.keys.BRANCH_TAKEN, Global.COMMIT_COUNT, event.robId)
        var valueNext = CombInit(value)
        for (slotId <- 0 until Global.COMMIT_COUNT) {
          when(event.mask(slotId) && isConditionalBranch(slotId)) {
            valueNext \= valueNext.dropHigh(1) ## isTaken(slotId)
          }
        }
        value := valueNext.resized
      }

      val onDecode = new Area{
        val value = Reg(keys.BRANCH_HISTORY) init (0)
      }

      val onFetch = new Area{
        val value = Reg(keys.BRANCH_HISTORY) init (0)
      }
    }

    val ras = new Area{
      val rasDepth = 32
      val mem = new Area{
        val stack = Mem.fill(rasDepth)(PC)
      }
      val ptr = new Area{
        val push = Reg(UInt(log2Up(rasDepth) bits)) init(0)
        val pop = Reg(UInt(log2Up(rasDepth) bits)) init(rasDepth-1)
        val pushIt, popIt = False

        push := push + U(pushIt) - U(popIt)
        pop  := pop + U(pushIt) - U(popIt)
      }
      val read = mem.stack.readAsync(ptr.pop)
      val write = mem.stack.writePort
      write.valid := ptr.pushIt
      write.address := ptr.push
      write.data.assignDontCare()
    }

    val gshare = new Area{
      val gshareWords = 4096
      def gshareHash(address : UInt, history : Bits) = address(SLICE_RANGE.high + 1, log2Up(gshareWords) bits) ^ U(history).resized

      class Entry extends Bundle{
//        val
      }

      val mem = new Area{ //TODO bypass read durring write ?
        val takeIt = Mem.fill(gshareWords)(keys.GSHARE_TAKE_IT)
        val strong = Mem.fill(gshareWords)(keys.GSHARE_STRONG)
        val takeItReaded = Mem.fill(branchContext.branchCount)(keys.GSHARE_TAKE_IT)
        val strongReaded = Mem.fill(branchContext.branchCount)(keys.GSHARE_STRONG)
      }
      val readCmd = new Area{
        val stage = fetch.getStage(branchHistoryFetchAt)
        import stage._

        val address = gshareHash(fetch.keys.FETCH_PC_PRE_TRANSLATION, keys.BRANCH_HISTORY)
      }

      val readRsp = new Area{
        val stage = fetch.getStage(branchHistoryFetchAt+1)
        import stage._

        stage(keys.GSHARE_TAKE_IT) := mem.takeIt.readSync(readCmd.address, readCmd.stage.isReady)
        stage(keys.GSHARE_STRONG) := mem.strong.readSync(readCmd.address, readCmd.stage.isReady)
      }

      val onLearn = new Area{
        val event = branchContext.logic.free.learn
        val history = learnContext.history.readAsync(event.id)
        val isConditionalBranch = learnContext.conditionalBranch.readAsync(event.id)
        val takeItReaded = mem.takeItReaded.readAsync(event.id)
        val hash = gshareHash(event.finalContext.pcOnLastSlice, history)

        val takeItPort = mem.takeIt.writePort
        takeItPort.valid := event.valid
        takeItPort.address := hash
        takeItPort.data := takeItReaded
        takeItPort.data(event.finalContext.pcOnLastSlice(SLICE_RANGE)) := event.finalContext.taken
      }
    }

    //TODO learn conditional bias
    val btb = new Area{
      val btbDepth = 8096
      val hashWidth = 16
      val wordBytesWidth = log2Up(FETCH_DATA_WIDTH/8)

      def getHash(value : UInt) = value(wordBytesWidth, hashWidth bits) //TODO better hash
      case class BtbEntry() extends Bundle {
        val hash = UInt(hashWidth bits)
        val slice  = UInt(log2Up(SLICE_COUNT) bits)
        val pcNext = PC()
      }

      val mem = Mem.fill(btbDepth)(BtbEntry())

      val onLearn = new Area{
        val event = branchContext.logic.free.learn
        val hash = getHash(event.finalContext.pcOnLastSlice)

        val port = mem.writePort
        port.valid := event.valid// && False //TODO REMOVE FALSE ! (debug)
        port.address := (event.finalContext.pcOnLastSlice >> wordBytesWidth).resized
        port.data.hash := hash
        port.data.slice := (event.finalContext.pcOnLastSlice >> sliceShift).resized
        port.data.pcNext := event.finalContext.pcNext
      }

      val read = new Area{
        val stage = fetch.getStage(btbJumpAt-1)
        import stage._
        val hash = getHash(FETCH_PC)
        val entryAddress = (FETCH_PC >> wordBytesWidth).resize(mem.addressWidth)
      }
      val applyIt = new Area{
        val stage = fetch.getStage(btbJumpAt)
        import stage._
        val entry = mem.readSync(read.entryAddress, read.stage.isReady)
        val hit = isValid && entry.hash === getHash(FETCH_PC)// && FETCH_PC(SLICE_RANGE) =/= entry.pcNext(SLICE_RANGE)
        flushNext(hit)
        setup.btbJump.valid := hit
        setup.btbJump.pc := entry.pcNext

        WORD_BRANCH_VALID := hit
        WORD_BRANCH_SLICE := entry.slice
        WORD_BRANCH_PC_NEXT := entry.pcNext
      }
    }

    val decodePatch = new Area {
      val stage = frontend.pipeline.decoded
      val stagePrevious = frontend.pipeline.aligned
      val branchKeys = List(Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU).map(e => Masked(e.key))
      val branchDecoder, jalDecoder, jalrDecoder, anyDecoder = new DecodingSpec(Bool()).setDefault(Masked.zero)
      branchDecoder.addNeeds(branchKeys, Masked.one)
      jalDecoder.addNeeds(Masked(Rvi.JAL.key), Masked.one)
      jalrDecoder.addNeeds(Masked(Rvi.JALR.key), Masked.one)
      anyDecoder.addNeeds(branchKeys ++ List(Rvi.JAL, Rvi.JALR).map(e => Masked(e.key)), Masked.one)

      import stage._

      var rasPushUsed = False
      var rasPopUsed = False
      val slots = for (slotId <- 0 until Frontend.DECODE_COUNT) yield new Area {
        implicit val _ = StageableOffset(slotId)

        def inst = Frontend.INSTRUCTION_DECOMPRESSED

        val isJal = jalDecoder.build(inst, decoder.covers())
        val isJalR = jalrDecoder.build(inst, decoder.covers())
        val isBranch = branchDecoder.build(inst, decoder.covers())
        val isAny = anyDecoder.build(inst, decoder.covers())
        val rdLink  = List(1,5).map(decoder.ARCH_RD === _).orR
        val rs1Link = List(1,5).map(decoder.ARCH_RS(0) === _).orR
        val rdEquRs1 = decoder.ARCH_RD === decoder.ARCH_RS(0)
        val rasPush = (isJal || isJalR) && rdLink
        val rasPop  = isJalR && (!rdLink && rs1Link || rdLink && rs1Link && !rdEquRs1)

        val imm = IMM(inst)
        val offset = Frontend.INSTRUCTION_DECOMPRESSED(2).mux(
          False -> imm.b_sext,
          True -> imm.j_sext
        )

        val lastSlice = PC(SLICE_RANGE) + INSTRUCTION_SLICE_COUNT
        val conditionalPrediction =  keys.GSHARE_TAKE_IT(lastSlice)//CombInit(offset.msb)

        val slices = INSTRUCTION_SLICE_COUNT +^ 1
        val pcInc = S(PC + (slices << sliceShift))
        val pcTarget = S(PC) + offset
        when(isJalR){ pcTarget := S(ras.read) }
        val canImprove = !isJalR || rasPop
        val branchedPrediction = isBranch && conditionalPrediction || isJal || isJalR
        val pcPrediction = branchedPrediction ? pcTarget otherwise pcInc
        val pcNext = canImprove ?  U(pcPrediction) otherwise ALIGNED_BRANCH_PC_NEXT
        val missmatch = !ALIGNED_BRANCH_VALID && branchedPrediction || ALIGNED_BRANCH_VALID && ALIGNED_BRANCH_PC_NEXT =/= U(pcPrediction)
        val needCorrection = Frontend.DISPATCH_MASK && canImprove && missmatch

        branchContext.keys.BRANCH_SEL := isAny
        branchContext.keys.BRANCH_EARLY.pcNext := pcNext
        keys.BRANCH_CONDITIONAL := isBranch

        when(stage.resulting(DISPATCH_MASK, slotId) && rasPush) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
          when(!rasPushUsed){
            ras.write.data := U(pcInc)
          }
          rasPushUsed \= True
        }
        when(stage.resulting(DISPATCH_MASK, slotId) && rasPop) { //WARNING use resulting DISPATCH_MASK ! (if one day things are moved around)
          rasPopUsed \= True
        }
      }
      when(isFireing){
        ras.ptr.pushIt    setWhen(rasPushUsed)
        ras.ptr.popIt     setWhen(rasPopUsed)
      }

      val hit = slots.map(_.needCorrection).orR
      val selOh = OHMasking.first(slots.map(_.needCorrection))
      setup.decodeJump.valid := isFireing && hit
      setup.decodeJump.pc := U(MuxOH(selOh, slots.map(_.pcPrediction)))

      flushIt(setup.decodeJump.valid, root = false)

      //WARNING, overloaded(Frontend.DISPATCH_MASK) may not be reconized by some downstream plugins if you move this futher the decoding stage
      for (slotId <- 1 until Frontend.DECODE_COUNT) {
        stage.overloaded(Frontend.DISPATCH_MASK, slotId) := stage(Frontend.DISPATCH_MASK, slotId)&& !(0 until slotId).map(i => slots(i).needCorrection).orR
      }

      var branchHistoryNext  = CombInit(branchHistory.onDecode.value)
      for (slotId <- 0 until Frontend.DECODE_COUNT) {
        val slot = slots(slotId)
        when(slot.isBranch && (Frontend.DISPATCH_MASK, slotId) && B(slots.take(slotId).map(s => s.needCorrection)) === 0){
          branchHistoryNext \= branchHistoryNext.dropHigh(1) ## slot.branchedPrediction
        }
      }
      when(isFireing){
        branchHistory.onDecode.value := branchHistoryNext
        branchHistory.onFetch.value := branchHistoryNext //TODO REMOVE ME (DEBUG), instead the update of the onFetch history should be done by gshare itself, in the fetch stages, to provide a more responsive branch history
      }
    }

    val update = new Area{
      val stage = frontend.pipeline.dispatch
      import stage._
      rob.write(keys.BRANCH_CONDITIONAL, DISPATCH_COUNT, (0 until DISPATCH_COUNT).map(stage(keys.BRANCH_CONDITIONAL, _)),  ROB.ID, isFireing)


      for (slotId <- 0 until Frontend.DECODE_COUNT) {
        implicit val _ = StageableOffset(slotId)
        val enable = isFireing && branchContext.keys.BRANCH_SEL && DISPATCH_MASK
        val bid = branchContext.keys.BRANCH_ID

        learnContext.history.write(bid, keys.BRANCH_HISTORY, enable)
        learnContext.conditionalBranch.write(bid, keys.BRANCH_CONDITIONAL, enable)
        gshare.mem.takeItReaded.write(bid, keys.GSHARE_TAKE_IT, enable)
        gshare.mem.strongReaded.write(bid, keys.GSHARE_STRONG, enable)
      }
    }

    val branchHistoryUpdates = new Area{
      import branchHistory._

      assert(branchHistoryFetchAt >= 1, "Would require some bypass of the stage(0) value, maybe later it could be implemented")
      fetch.getStage(branchHistoryFetchAt)(keys.BRANCH_HISTORY) := onFetch.value

      val fetchJumps = getService[PcPlugin].getFetchJumps()
      val grouped = fetchJumps.groupBy(_._1)
      val ordered = grouped.toSeq.sortBy(_._1).map(_._2)
      val group = for(group <- grouped) yield new Area{ //TODO manage case where the group is < than branchHistoryFetchAt
        val valid = group._2.map(_._2).orR
        when(valid){
          onFetch.value := fetch.getStage(group._1).resulting(keys.BRANCH_HISTORY)
        }
      }
      when(setup.decodeJump.valid){
        onFetch.value := decodePatch.branchHistoryNext
      }
      when(commit.reschedulingPort().valid){
        onFetch.value := onCommit.valueNext
        onDecode.value := onCommit.valueNext
      }
    }
    rob.release()
    frontend.release()
    fetch.release()
  }
}