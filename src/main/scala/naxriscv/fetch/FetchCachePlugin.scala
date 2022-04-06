package naxriscv.fetch

import naxriscv.{Fetch, Global}
import naxriscv.Global._
import naxriscv.interfaces.{AddressTranslationPortUsage, AddressTranslationService, JumpService, PerformanceCounterService, PulseHandshake}
import naxriscv.utilities._
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable
import naxriscv.Frontend._
import naxriscv.Fetch._
import naxriscv.prediction.HistoryPlugin
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly}
import spinal.lib.bus.amba4.axilite.{AxiLite4Config, AxiLite4ReadOnly}

import scala.collection.mutable.ArrayBuffer

case class FetchL1Cmd(physicalWidth : Int) extends Bundle{
  val address = UInt(physicalWidth bits)
  val io      = Bool()
}

case class FetchL1Rsp(dataWidth : Int) extends Bundle{
  val data = Bits(dataWidth bits)
  val error = Bool()
}

case class FetchL1Bus(physicalWidth : Int,
                      dataWidth : Int,
                      lineSize : Int,
                      withBackPresure : Boolean) extends Bundle with IMasterSlave {
  val cmd = Stream(FetchL1Cmd(physicalWidth))
  val rsp = Stream(FetchL1Rsp(dataWidth))

  def beatCount = lineSize*8/dataWidth
  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }

  def ioSplit() : (FetchL1Bus, FetchL1Bus) = new Composite(this, "ioSplit"){
    val io, ram = cloneOf(self)
    val selIo = RegNextWhen(cmd.io, cmd.valid)

    io.cmd.valid := cmd.valid && cmd.io
    ram.cmd.valid := cmd.valid && !cmd.io

    io.cmd.payload  := cmd.payload
    ram.cmd.payload := cmd.payload

    cmd.ready := cmd.io ? io.cmd.ready | ram.cmd.ready

    rsp.valid := io.rsp.valid || ram.rsp.valid
    rsp.payload := selIo ? io.rsp.payload | ram.rsp.payload

    val ret = (io, ram)
  }.ret


  def resizer(newDataWidth : Int) : FetchL1Bus = new Composite(this, "resizer"){
    val ret = FetchL1Bus(
      physicalWidth   = physicalWidth,
      dataWidth       = newDataWidth,
      lineSize        = lineSize,
      withBackPresure = withBackPresure || newDataWidth > dataWidth
    )

    ret.cmd << self.cmd

    val rspOutputStream = Stream(Bits(dataWidth bits))
    StreamWidthAdapter(ret.rsp.translateWith(ret.rsp.data), rspOutputStream)

    rsp.valid := rspOutputStream.valid
    rsp.data  := rspOutputStream.payload
    rsp.error := ret.rsp.error
    rspOutputStream.ready :=  (if(withBackPresure) rspOutputStream.ready else True)
  }.ret

  def toAxi4(): Axi4ReadOnly = new Composite(this, "toAxi4"){
    val axiConfig = Axi4Config(
      addressWidth = physicalWidth,
      dataWidth    = dataWidth,
      idWidth      = 0,
      useId        = true,
      useRegion    = false,
      useBurst     = true,
      useLock      = false,
      useCache     = false,
      useSize      = true,
      useQos       = false,
      useLen       = true,
      useLast      = true,
      useResp      = true,
      useProt      = true,
      useStrb      = false
    )

    val axi = Axi4ReadOnly(axiConfig)
    axi.ar.valid := cmd.valid
    axi.ar.addr  := cmd.address
    axi.ar.id    := 0
    axi.ar.prot  := B"110"
    axi.ar.len   := lineSize*8/dataWidth-1
    axi.ar.size  := log2Up(dataWidth/8)
    axi.ar.setBurstINCR()
    cmd.ready := axi.ar.ready

    rsp.valid := axi.r.valid
    rsp.data  := axi.r.data
    rsp.error := !axi.r.isOKAY()
    axi.r.ready := (if(withBackPresure) rsp.ready else True)
  }.axi

  def toAxiLite4(): AxiLite4ReadOnly = new Composite(this, "toAxi4"){
    val axiConfig = AxiLite4Config(
      addressWidth = physicalWidth,
      dataWidth    = dataWidth
    )

    val counter = Reg(UInt(log2Up(lineSize*8/dataWidth) bits)) init(0)
    val last = counter.andR

    val axi = AxiLite4ReadOnly(axiConfig)
    axi.ar.valid := cmd.valid
    axi.ar.addr  := cmd.address | (counter << log2Up(dataWidth/8)).resized
    axi.ar.setUnprivileged
    cmd.ready := axi.ar.ready && last
    when(axi.ar.fire){
      counter := counter + 1
    }

    rsp.valid := axi.r.valid
    rsp.data  := axi.r.data
    rsp.error := !axi.r.isOKAY()
    axi.r.ready := True
  }.axi

}

case class FetchBypassSpec(stageId : Int) extends Area{
  val valid = Stageable(Bool())
  val data =  Stageable(WORD)
}

class FetchCachePlugin(var cacheSize : Int,
                       var wayCount : Int,
                       var memDataWidth : Int,
                       var fetchDataWidth : Int,
                       var translationStorageParameter : Any,
                       var translationPortParameter : Any,
                       var lineSize : Int = 64,
                       var readAt : Int = 0,
                       var hitsAt : Int = 1,
                       var hitAt : Int = 1,
                       var bankMuxesAt : Int = 1,
                       var bankMuxAt : Int = 2,
                       var controlAt : Int = 2,
                       var injectionAt : Int = 2,
                       var hitsWithTranslationWays : Boolean = false,
                       var reducedBankWidth : Boolean = false,
                       var tagsReadAsync : Boolean = true,
                       var refillEventId : Int = PerformanceCounterService.ICACHE_REFILL) extends Plugin with FetchPipelineRequirements {


  create config {
    Fetch.FETCH_DATA_WIDTH.set(fetchDataWidth)
  }

  override def stagesCountMin = injectionAt + 1

  val mem = create early master(FetchL1Bus(
    physicalWidth = PHYSICAL_WIDTH,
    dataWidth     = memDataWidth,
    lineSize      = lineSize,
    withBackPresure = false
  ))
  def invalidatePort = setup.invalidatePort

  val bypassesSpec = ArrayBuffer[FetchBypassSpec]()
  def createBypass(stageId : Int = controlAt) = bypassesSpec.addRet(new FetchBypassSpec(stageId))

  val setup = create early new Area{
    val pipeline = getService[FetchPlugin]
    val translation = getService[AddressTranslationService]
    pipeline.lock.retain()
    translation.retain()

    val withHistory = isServiceAvailable[HistoryPlugin]
    val priority = JumpService.Priorities.FETCH_WORD(controlAt, false)
    val redoJump = getService[PcPlugin].createJumpInterface(priority)
    val historyJump = withHistory generate getService[HistoryPlugin].createJumpPort(priority)
    val refillEvent = getServiceOption[PerformanceCounterService].map(_.createEventPort(refillEventId))
    val invalidatePort = PulseHandshake().setIdleAll


    val translationStorage = translation.newStorage(translationStorageParameter)

    mem.flatten.filter(_.isOutput).foreach(_.assignDontCare())

    val doc = getService[DocPlugin]
    doc.property("FETCH_MEM_DATA_BITS", memDataWidth)
    doc.property("FETCH_LINE_BYTES", lineSize)
  }

  val logic = create late new Area{
    val fetch = getService[FetchPlugin]
    val translation = getService[AddressTranslationService]
    val cpuWordWidth = FETCH_DATA_WIDTH.get
    val bytePerMemWord = memDataWidth/8
    val bytePerFetchWord = cpuWordWidth/8
    val waySize = cacheSize/wayCount
    val linePerWay = waySize/lineSize
    val memDataPerWay = waySize/bytePerMemWord
    val memData = HardType(Bits(memDataWidth bits))
    val memWordPerLine = lineSize/bytePerMemWord
    val tagWidth = PHYSICAL_WIDTH-log2Up(waySize)


    val tagRange = PHYSICAL_WIDTH-1 downto log2Up(linePerWay*lineSize)
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

    val translationPort = translation.newTranslationPort(
      stages = fetch.pipeline.stages,
      preAddress = FETCH_PC,
      usage = AddressTranslationPortUsage.FETCH,
      portSpec = translationPortParameter,
      storageSpec = setup.translationStorage
    )
    val tpk = translationPort.keys


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
        val rsp = if(tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
        setup.pipeline.getStage(readAt+(!tagsReadAsync).toInt)(WAYS_TAGS)(id) := rsp
        KeepAttribute(rsp)
      }
    }

    val invalidate = new Area{
      val requested = RegInit(False) setWhen(setup.invalidatePort.request)
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

      setup.invalidatePort.served setWhen(done.rise(False))
    }


    val refill = new Area {
      val fire = False
      val valid = RegInit(False) clearWhen (fire)
      val address = KeepAttribute(Reg(UInt(PHYSICAL_WIDTH bits)))
      val isIo = Reg(Bool())
      val hadError = RegInit(False)

      invalidate.canStart clearWhen(valid)

      val cmdSent = RegInit(False) setWhen (mem.cmd.fire) clearWhen (fire)
      mem.cmd.valid := valid && !cmdSent
      mem.cmd.address := address(tagRange.high downto lineRange.low) @@ U(0, lineRange.low bit)
      mem.cmd.io := isIo

      val wayToAllocate = Counter(wayCount, !valid)
      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      when(invalidate.done) {
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

      val bankMuxStd = !reducedBankWidth generate new Area {
        import bankMuxStage._
        WORD := OhMux.or(WAYS_HITS, BANKS_MUXES)
      }

      val bankMux = reducedBankWidth generate new Area {
        import bankMuxStage._
        val wayId = OHToUInt(WAYS_HITS)
        val bankId = (wayId >> log2Up(bankCount/memToBankRatio)) @@ ((wayId + (FETCH_PC(log2Up(bankWidth/8), log2Up(bankCount) bits))).resize(log2Up(bankCount/memToBankRatio)))
        WORD := BANKS_MUXES.read(bankId) //MuxOH(WAYS_HITS, BANKS_MUXES)
      }


      val onWays = for((way, wayId) <- ways.zipWithIndex) yield new Area{
        {
          import readStage._
          way.read.cmd.valid := !isStuck
          way.read.cmd.payload := FETCH_PC(lineRange)
        }

        (!hitsWithTranslationWays || translationPort.wayCount == 0) generate {import hitsStage._ ; WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === tpk.TRANSLATED(tagRange) }

        val hits = (hitsWithTranslationWays && translationPort.wayCount > 0) generate new Area{
          import hitsStage._
          val wayTlbHits = (0 until translationPort.wayCount) map(tlbWayId => WAYS_TAGS(wayId).address === tpk.WAYS_PHYSICAL(tlbWayId)(tagRange) && tpk.WAYS_OH(tlbWayId))
          val translatedHits = wayTlbHits.orR
          val bypassHits     = WAYS_TAGS(wayId).address === FETCH_PC >> tagRange.low
          WAYS_HITS(wayId) := (tpk.BYPASS_TRANSLATION ? bypassHits | translatedHits) & WAYS_TAGS(wayId).loaded
        }
      }

      {import hitStage._;   WAYS_HIT := B(WAYS_HITS).orR}

      val ctrl = new Area{
        import controlStage._

        setup.redoJump.valid := False
        setup.redoJump.pc    := FETCH_PC

        WORD_FAULT := (B(WAYS_HITS) & B(WAYS_TAGS.map(_.error))).orR || WORD_FAULT_PAGE || tpk.ACCESS_FAULT
        WORD_FAULT_PAGE := tpk.PAGE_FAULT || !tpk.ALLOW_EXECUTE




        val redoIt = False
        when(redoIt){
          setup.redoJump.valid := True
          flushIt()
          setup.pipeline.getStage(0).haltIt() //"optional"
        }

        when(isValid) {
          when(tpk.REDO) {
            redoIt := True
          } elsewhen (!WORD_FAULT_PAGE && !tpk.ACCESS_FAULT && !WAYS_HIT) {
            redoIt := True
            refill.valid := True
            refill.address := tpk.TRANSLATED
            refill.isIo := tpk.IO
          }
        }

        val bypass = if(bypassesSpec.nonEmpty)  new Area {
          val hits = B(bypassesSpec.map(e => controlStage(e.valid)))
          val hit = hits.orR
          val value = OhMux.or(hits, bypassesSpec.map(e => controlStage(e.data)),bypassIfSingle = true)
          when(hit) {
            WORD_FAULT := False
            WORD_FAULT_PAGE := False
            WORD := value
            redoIt := False
            refill.valid := False
          }
        }

        setup.pipeline.getStage(0).haltIt(!translationPort.wake)

        if(setup.withHistory){
          setup.historyJump.valid := setup.redoJump.valid
          setup.historyJump.history := getService[HistoryPlugin].keys.BRANCH_HISTORY
        }
      }
    }




    translation.release()
    setup.pipeline.lock.release()
  }
}
