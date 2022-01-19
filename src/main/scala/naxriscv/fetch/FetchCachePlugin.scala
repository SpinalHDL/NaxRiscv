package naxriscv.fetch

import naxriscv.interfaces.{AddressTranslationService, JumpService, PerformanceCounterService}
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
import naxriscv.Frontend._
import naxriscv.Fetch._
import naxriscv.prediction.HistoryPlugin

case class FetchL1Cmd(p : FetchCachePlugin, physicalWidth : Int) extends Bundle{
  val address = UInt(physicalWidth bits)
}

case class FetchL1Rsp(p : FetchCachePlugin) extends Bundle{
  val data = Bits(p.memDataWidth bit)
  val error = Bool()
}

case class FetchL1Bus(p : FetchCachePlugin, physicalWidth : Int) extends Bundle with IMasterSlave {
  val cmd = Stream(FetchL1Cmd(p, physicalWidth))
  val rsp = Flow(FetchL1Rsp(p))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

class FetchCachePlugin(val cacheSize : Int,
                       val wayCount : Int,
                       val memDataWidth : Int,
                       val lineSize : Int = 64,
                       val readAt : Int = 0,
                       val hitsAt : Int = 1,
                       val hitAt : Int = 1,
                       val bankMuxesAt : Int = 1,
                       val bankMuxAt : Int = 2,
                       val controlAt : Int = 2,
                       val injectionAt : Int = 2,
                       val reducedBankWidth : Boolean = false,
                       val refillEventId : Int = PerformanceCounterService.ICACHE_REFILL
                      ) extends Plugin with FetchPipelineRequirements {
  override def stagesCountMin = injectionAt + 1

  val mem = create early master(FetchL1Bus(this, getService[AddressTranslationService].postWidth))
  def flushPort = setup.flushPort

  val setup = create early new Area{
    val pipeline = getService[FetchPlugin]
    pipeline.lock.retain()

    val withHistory = isServiceAvailable[HistoryPlugin]
    val priority = JumpService.Priorities.FETCH_WORD(controlAt, false)
    val redoJump = getService[PcPlugin].createJumpInterface(priority)
    val historyJump = withHistory generate getService[HistoryPlugin].createJumpPort(priority)
    val refillEvent = getServiceOption[PerformanceCounterService].map(_.createEventPort(refillEventId))
    val flushPort = False

    mem.flatten.filter(_.isOutput).foreach(_.assignDontCare())

    val doc = getService[DocPlugin]
    doc.property("FETCH_MEM_DATA_BITS", memDataWidth)
    doc.property("FETCH_LINE_BYTES", lineSize)
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val preTranslationWidth = getService[AddressTranslationService].postWidth
    val cpuWordWidth = FETCH_DATA_WIDTH.get
    val bytePerMemWord = memDataWidth/8
    val bytePerFetchWord = cpuWordWidth/8
    val waySize = cacheSize/wayCount
    val linePerWay = waySize/lineSize
    val memDataPerWay = waySize/bytePerMemWord
    val memData = HardType(Bits(memDataWidth bits))
    val memWordPerLine = lineSize/bytePerMemWord
    val tagWidth = preTranslationWidth-log2Up(waySize)


    val tagRange = preTranslationWidth-1 downto log2Up(linePerWay*lineSize)
    val lineRange = tagRange.low-1 downto log2Up(lineSize)

    val bankCount = wayCount
    val bankWidth =  if(!reducedBankWidth) memDataWidth else Math.max(cpuWordWidth, memDataWidth/wayCount)
    val bankByteSize = cacheSize/bankCount
    val bankWordCount = bankByteSize*8/bankWidth
    val bankWordToCpuWordRange = log2Up(bankWidth/8)-1 downto log2Up(bytePerFetchWord)
    val memToBankRatio = bankWidth*bankCount / memDataWidth
    val bankWord = HardType(Bits(bankWidth bits))



    val readStage = setup.pipeline.getStage(readAt)
    val hitsStage = setup.pipeline.getStage(hitsAt)
    val hitStage = setup.pipeline.getStage(hitAt)
    val bankMuxesStage = setup.pipeline.getStage(bankMuxesAt)
    val bankMuxStage = setup.pipeline.getStage(bankMuxAt)
    val controlStage = setup.pipeline.getStage(controlAt)
    val injectionStage = setup.pipeline.getStage(injectionAt)

    case class Tag() extends Bundle{
      val loaded = Bool()
      val error = Bool()
      val address = UInt(tagWidth bits)
    }

    val BANKS_WORDS = Stageable(Vec.fill(bankCount)(bankWord()))
    val WAYS_TAGS = Stageable(Vec.fill(wayCount)(Tag()))
    val WAYS_HITS = Stageable(Vec.fill(wayCount)(Bool()))
    val WAYS_HIT = Stageable(Bool())

    val BANKS_MUXES = Stageable(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))



    val banks = for(id <- 0 until bankCount) yield new Area{
      val mem = Mem(Bits(bankWidth bits), bankWordCount)
      val write = mem.writePort
      val read = new Area{
        val cmd = Flow(mem.addressType)
        val rsp = mem.readSync(cmd.payload, cmd.valid)
        setup.pipeline.getStage(readAt+1)(BANKS_WORDS)(id) := rsp
        KeepAttribute(rsp)
      }
    }
    val waysWrite = new Area{
      val mask = Bits(wayCount bits)
      val address = UInt(log2Up(linePerWay) bits)
      val tag = Tag()

      mask := 0
      address.assignDontCare()
      tag.assignDontCare()
    }
    val ways = for(id <- 0 until wayCount) yield new Area {
      val mem = Mem.fill(linePerWay)(Tag())
      mem.write(waysWrite.address, waysWrite.tag, waysWrite.mask(id))
      val read = new Area{
        val cmd = Flow(mem.addressType)
        val rsp = mem.readSync(cmd.payload, cmd.valid)
        setup.pipeline.getStage(readAt+1)(WAYS_TAGS)(id) := rsp
        KeepAttribute(rsp)
      }
    }

    val flush = new Area{
      val requested = RegInit(False) setWhen(setup.flushPort)
      val canStart = True
      val counter = Reg(UInt(log2Up(linePerWay)+1 bits)) init(0)
      val done = counter.msb
      when(!done){
        counter := counter + 1
      }

      when(!done) {
        waysWrite.mask := (default -> true)
        waysWrite.address := counter.resized
        waysWrite.tag.loaded := False
      }

      readStage.haltIt(!done || requested)

      when(requested && canStart){
        counter := 0
        requested := False
      }

      when(fetch.pipeline.stages.drop(readAt+1).map(_.isValid).toList.orR){
        canStart := False
      }
    }


    val refill = new Area {
      val fire = False
      val valid = RegInit(False) clearWhen (fire)
      val address = KeepAttribute(Reg(UInt(preTranslationWidth bits)))
      val hadError = RegInit(False)

      flush.canStart clearWhen(valid)

      val cmdSent = RegInit(False) setWhen (mem.cmd.fire) clearWhen (fire)
      mem.cmd.valid := valid && !cmdSent
      mem.cmd.address := address(tagRange.high downto lineRange.low) @@ U(0, lineRange.low bit)

      val wayToAllocate = Counter(wayCount, !valid)
      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      when(flush.done) {
        waysWrite.mask(wayToAllocate) setWhen(fire)
        waysWrite.address := address(lineRange)
        waysWrite.tag.loaded := True
        waysWrite.tag.error := hadError || mem.rsp.error
        waysWrite.tag.address := address(tagRange)
      }


      for((bank, bankId) <- banks.zipWithIndex){
        if(!reducedBankWidth) {
          bank.write.valid := mem.rsp.valid && wayToAllocate === bankId
          bank.write.address := address(lineRange) @@ wordIndex
          bank.write.data := mem.rsp.data
        } else {
          val sel = U(bankId) - wayToAllocate.value
          val groupSel = wayToAllocate(log2Up(bankCount)-1 downto log2Up(bankCount/memToBankRatio))
          val subSel = sel(log2Up(bankCount/memToBankRatio) -1 downto 0)
          bank.write.valid := mem.rsp.valid && groupSel === (bankId >> log2Up(bankCount/memToBankRatio))
          bank.write.address := address(lineRange) @@ wordIndex @@ (subSel)
          bank.write.data := mem.rsp.data.subdivideIn(bankCount/memToBankRatio slices)(subSel)
        }
      }


      when(mem.rsp.valid) {
        wordIndex := (wordIndex + 1).resized
        hadError.setWhen(mem.rsp.error)
        when(wordIndex === wordIndex.maxValue) {
          fire := True
        }
      }

      hadError clearWhen (fire)
      setup.pipeline.getStage(0).haltIt(valid)

      setup.refillEvent.map(_ := RegNext(fire) init(False))
    }

    val read = new Area{
      for((bank, bankId) <- banks.zipWithIndex) yield new Area{
        {
          import readStage._
          bank.read.cmd.valid := !isStuck
          bank.read.cmd.payload := FETCH_PC(lineRange.high downto log2Up(bankWidth / 8))
        }
        {import bankMuxesStage._; BANKS_MUXES(bankId) := BANKS_WORDS(bankId).subdivideIn(cpuWordWidth bits).read(FETCH_PC(bankWordToCpuWordRange)) }
      }

      val bankMux = new Area {
        import bankMuxStage._
        val wayId = OHToUInt(WAYS_HITS)
        val bankId = if(!reducedBankWidth) wayId else (wayId >> log2Up(bankCount/memToBankRatio)) @@ ((wayId + (FETCH_PC(log2Up(bankWidth/8), log2Up(bankCount) bits))).resize(log2Up(bankCount/memToBankRatio)))
        WORD := BANKS_MUXES.read(bankId) //MuxOH(WAYS_HITS, BANKS_MUXES)
      }


      for((way, wayId) <- ways.zipWithIndex) yield new Area{
        {
          import readStage._
          way.read.cmd.valid := !isStuck
          way.read.cmd.payload := FETCH_PC(lineRange)
        }

        {import hitsStage._ ; WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === FETCH_PC_TRANSLATED(tagRange) }
      }

      {import hitStage._;   WAYS_HIT := B(WAYS_HITS).orR}

      val ctrl = new Area{
        import controlStage._

        setup.redoJump.valid := False
        setup.redoJump.pc    := FETCH_PC

        WORD_FAULT := (B(WAYS_HITS) & B(WAYS_TAGS.map(_.error))).orR
        WORD_FAULT_PAGE := False //TODO

        when(isValid) {
          when(!WAYS_HIT) {
            refill.valid := True
            refill.address := FETCH_PC_TRANSLATED

            setup.redoJump.valid := True
            flushIt()
            setup.pipeline.getStage(0).haltIt() //"optional"
          }
        }

        if(setup.withHistory){
          setup.historyJump.valid := setup.redoJump.valid
          setup.historyJump.history := getService[HistoryPlugin].keys.BRANCH_HISTORY
        }
      }

      val inject = new Area{
      }
    }




    setup.pipeline.lock.release()
  }
}
