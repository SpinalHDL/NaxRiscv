package naxriscv.units.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stageable, StageableOffsetNone}

import scala.collection.mutable.ArrayBuffer

case class DataLoadPort(preTranslationWidth : Int,
                        postTranslationWidth : Int,
                        dataWidth : Int,
                        refillCount : Int,
                        rspAt : Int,
                        translatedAt : Int) extends Bundle with IMasterSlave {
  val cmd = Flow(DataLoadCmd(preTranslationWidth, dataWidth))
  val translated = DataLoadTranslated(postTranslationWidth)
  val cancels = Bits(rspAt+1 bits)
  val rsp = Flow(DataLoadRsp(dataWidth, refillCount)) //The rsp.valid is fondamentaly necessary, as it has a fixed latency

  override def asMaster() = {
    master(cmd)
    out(translated)
    out(cancels)
    slave(rsp)
  }
}

case class DataLoadCmd(preTranslationWidth : Int, dataWidth : Int) extends Bundle {
  val virtual = UInt(preTranslationWidth bits)
  val size = UInt(log2Up(log2Up(dataWidth/8)+1) bits)
}

case class DataLoadTranslated(physicalWidth : Int) extends Bundle {
  val physical   = UInt(physicalWidth bits)
  val peripheral = Bool()
}

case class DataLoadRsp(dataWidth : Int, refillCount : Int) extends Bundle {
  val data = Bits(dataWidth bits)
  val fault = Bool()
  val miss = Bool()
  val refillSlot = Bits(refillCount bits) //Zero when refillSlotFull
  val refillSlotFull = Bool() //Not valid if !miss
}

case class DataStorePort(physicalWidth: Int,
                         dataWidth: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataStoreCmd(physicalWidth, dataWidth))
  val rsp = Flow(NoData)

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}
case class DataStoreCmd(postTranslationWidth: Int,
                        dataWidth: Int) extends Bundle {
  val address = UInt(postTranslationWidth bits)
  val data = Bits(dataWidth bits)
  val mask = Bits(dataWidth/8 bits)
}

case class DataMemCmd(addressWidth: Int,
                      dataWidth: Int) extends Bundle {
  val address = UInt(addressWidth bits)
//  val data = Bits(dataWidth bits)
//  val mask = Bits(dataWidth/8 bits)
}

case class DataMemRsp(dataWidth: Int) extends Bundle {
  val data = Bits(dataWidth bits)
  val error = Bool()
}

case class DataMemBus(addressWidth: Int,
                      dataWidth: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataMemCmd(addressWidth, dataWidth))
  val rsp = Flow(DataMemRsp(dataWidth))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}


class DataCache(val cacheSize: Int,
                val wayCount: Int,
                val refillCount : Int,
                val memDataWidth: Int,
                val cpuDataWidth: Int,
                val preTranslationWidth: Int,
                val postTranslationWidth: Int,
                val lineSize: Int = 64,
                val loadReadAt: Int = 0,
                val loadHitsAt: Int = 1,
                val loadHitAt: Int = 1,
                val loadBankMuxesAt: Int = 1,
                val loadBankMuxAt: Int = 2,
                val loadControlAt: Int = 2,
                val loadRspAt: Int = 2) extends Component {
  assert(refillCount == 1, "refillCounts should be 1 for now")

  val io = new Bundle {
    val load = slave(DataLoadPort(
      preTranslationWidth  = preTranslationWidth,
      postTranslationWidth = postTranslationWidth,
      dataWidth     = cpuDataWidth,
      refillCount   = refillCount,
      rspAt         = loadRspAt,
      translatedAt  = loadHitsAt
    ))
    val store = slave(Stream(DataStoreCmd(
      postTranslationWidth = postTranslationWidth,
      dataWidth     = cpuDataWidth
    )))
    val mem = master(DataMemBus(postTranslationWidth, memDataWidth))
    val refillCompletions = out Bits(refillCount bits)
  }

  val pipeline = new Pipeline{
    val stages = Array.fill(loadRspAt+1)(newStage())
    connect(stages)(List(M2S()))

    for((stage, stageId) <- stages.zipWithIndex){
      stage.throwIt(io.load.cancels(stageId))
    }
  }



  val cpuWordWidth = cpuDataWidth
  val bytePerMemWord = memDataWidth/8
  val bytePerFetchWord = memDataWidth/8
  val waySize = cacheSize/wayCount
  val linePerWay = waySize/lineSize
  val memDataPerWay = waySize/bytePerMemWord
  val memData = HardType(Bits(memDataWidth bits))
  val memWordPerLine = lineSize/bytePerMemWord
  val tagWidth = preTranslationWidth-log2Up(waySize)


  val tagRange = preTranslationWidth-1 downto log2Up(linePerWay*lineSize)
  val lineRange = tagRange.low-1 downto log2Up(lineSize)

  val bankCount = wayCount
  val bankWidth = Math.max(cpuWordWidth, memDataWidth/wayCount)
  val bankByteSize = cacheSize/bankCount
  val bankWordCount = bankByteSize*8/bankWidth
  val bankWordToCpuWordRange = log2Up(bankWidth/8)-1 downto log2Up(bytePerFetchWord)
  val memToBankRatio = bankWidth*bankCount / memDataWidth
  val bankWord = HardType(Bits(bankWidth bits))


  val ADDRESS_PRE_TRANSLATION = Stageable(UInt(preTranslationWidth bits))
  val ADDRESS_POST_TRANSLATION = ADDRESS_PRE_TRANSLATION //Stageable(UInt(postTranslationWidth bits))  //TODO for now
  val CPU_WORD = Stageable(Bits(cpuWordWidth bits))

  val readStage      = pipeline.stages(loadReadAt)
  val hitsStage      = pipeline.stages(loadHitsAt)
  val hitStage       = pipeline.stages(loadHitAt)
  val bankMuxesStage = pipeline.stages(loadBankMuxesAt)
  val bankMuxStage   = pipeline.stages(loadBankMuxAt)
  val controlStage   = pipeline.stages(loadControlAt)
  val rspStage = pipeline.stages(loadRspAt)

  case class Tag() extends Bundle{
    val loaded = Bool()
    val error = Bool()
    val address = UInt(tagWidth bits)
  }

  val BANKS_WORDS = Stageable(Vec.fill(bankCount)(bankWord()))
  val WAYS_TAGS = Stageable(Vec.fill(wayCount)(Tag()))
  val WAYS_HITS = Stageable(Vec.fill(wayCount)(Bool()))
  val WAYS_HIT = Stageable(Bool())
  val MISS = Stageable(Bool())
  val SIZE = Stageable(UInt(log2Up(log2Up(cpuDataWidth/8)+1) bits))
  val REFILL_SLOT = Stageable(Bits(refillCount bits))
  val REFILL_SLOT_FULL = Stageable(Bool())

  val BANKS_MUXES = Stageable(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))


  val banks = for(id <- 0 until bankCount) yield new Area{
    val mem = Mem(Bits(bankWidth bits), bankWordCount)
    val write = mem.writePort
    val read = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)
      pipeline.stages(loadReadAt+1)(BANKS_WORDS)(id) := rsp
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
      pipeline.stages(loadReadAt+1)(WAYS_TAGS)(id) := rsp
    }
  }

  val flush = new Area{
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

//    readStage.haltIt(!done)
  }


  val refill = for(refillId <- 0 until refillCount) yield new Area {
    val id = refillId
    val fire = False
    val valid = RegInit(False) clearWhen (fire)
    val address = KeepAttribute(Reg(UInt(postTranslationWidth bits)))
    val hadError = RegInit(False) clearWhen (fire)

    val cmdSent = RegInit(False) setWhen (io.mem.cmd.fire) clearWhen (fire)
    io.mem.cmd.valid := valid && !cmdSent
    io.mem.cmd.address := address(tagRange.high downto lineRange.low) @@ U(0, lineRange.low bit)

    val wayToAllocate = Counter(wayCount, !valid)
    val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

    io.refillCompletions(id) := fire //Maybe pipelined

    when(flush.done) {
      waysWrite.mask(wayToAllocate) setWhen(fire)
      waysWrite.address := address(lineRange)
      waysWrite.tag.loaded := True
      waysWrite.tag.error := hadError || io.mem.rsp.error
      waysWrite.tag.address := address(tagRange)
    }


    for(bankId <- 0 until bankCount){
      val sel = U(bankId) - wayToAllocate.value
      val groupSel = wayToAllocate(log2Up(bankCount)-1 downto log2Up(bankCount/memToBankRatio))
      val subSel = sel(log2Up(bankCount/memToBankRatio) -1 downto 0)
      banks(bankId).write.valid := io.mem.rsp.valid && groupSel === (bankId >> log2Up(bankCount/memToBankRatio))
      banks(bankId).write.address := address(lineRange) @@ wordIndex @@ (subSel)
      banks(bankId).write.data := io.mem.rsp.data.subdivideIn(bankCount/memToBankRatio slices)(subSel)
    }


    when(io.mem.rsp.valid) {
      wordIndex := (wordIndex + 1).resized
      hadError.setWhen(io.mem.rsp.error)
      when(wordIndex === wordIndex.maxValue) {
        fire := True
      }
    }

//    pipeline.stages(0).haltIt(valid)
  }
  val refillFree = B(OHMasking.first(refill.map(!_.valid)))
  val refillFull = refill.map(_.valid).andR

  val start = new Area{
    val stage = pipeline.stages.head
    import stage._

    isValid := io.load.cmd.valid
    ADDRESS_PRE_TRANSLATION := io.load.cmd.virtual
    SIZE := io.load.cmd.size
  }

  val fetch = new Area{
    for((bank, bankId) <- banks.zipWithIndex) yield new Area{
      {
        import readStage._
        bank.read.cmd.valid := !isStuck
        bank.read.cmd.payload := ADDRESS_PRE_TRANSLATION(lineRange.high downto log2Up(bankWidth / 8))
      }
      {import bankMuxesStage._; BANKS_MUXES(bankId) := BANKS_WORDS(bankId).subdivideIn(cpuWordWidth bits).read(ADDRESS_PRE_TRANSLATION(bankWordToCpuWordRange)) }
    }
    {import bankMuxStage._;   CPU_WORD := MuxOH(WAYS_HITS, BANKS_MUXES) }


    for((way, wayId) <- ways.zipWithIndex) yield new Area{
      {
        import readStage._
        way.read.cmd.valid := !isStuck
        way.read.cmd.payload := ADDRESS_PRE_TRANSLATION(lineRange)
      }

      {import hitsStage._ ; WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === ADDRESS_POST_TRANSLATION(tagRange) }
    }

    {import hitStage._;   WAYS_HIT := B(WAYS_HITS).orR}

    val ctrl = new Area{
      import controlStage._

      val refillHits = B(refill.map(r => r.valid && r.address === ADDRESS_POST_TRANSLATION))
      val refillHit = refillHits.orR

      MISS := !WAYS_HIT
      REFILL_SLOT_FULL := !refillHit && refillFull
      REFILL_SLOT := refillHit ? refillHits | refillFree

      when(isValid && MISS && !refillHit && !refillFull) {
        for(r <- refill) when(refillFree(r.id)){
          r.valid := True
          r.address := ADDRESS_POST_TRANSLATION
        }
      }
    }

    val inject = new Area{
      import rspStage._

      io.load.rsp.valid := isValid
      io.load.rsp.data := CPU_WORD
      io.load.rsp.fault := False //TODO
      io.load.rsp.miss := MISS
      io.load.rsp.refillSlotFull := REFILL_SLOT_FULL
      io.load.rsp.refillSlot := REFILL_SLOT
    }
  }
  
  pipeline.build()

//  io.flatten.filter(_.isOutput).map(_.assignDontCare())
}