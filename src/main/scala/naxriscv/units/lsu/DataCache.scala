package naxriscv.units.lsu

import naxriscv.utilities.AddressToMask
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.M2S
import spinal.lib.pipeline.{Pipeline, Stage, Stageable, StageableOffsetNone}

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
  val redo = Bool()
  val refillSlot = Bits(refillCount bits) //Zero when refillSlotAny
  val refillSlotAny = Bool() //Not valid if !miss
}

case class DataStorePort(postTranslationWidth: Int,
                         dataWidth: Int,
                         refillCount : Int) extends Bundle with IMasterSlave {
  val cmd = Flow(DataStoreCmd(postTranslationWidth, dataWidth))
  val rsp = Flow(DataStoreRsp(postTranslationWidth, refillCount))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}
case class DataStoreCmd(postTranslationWidth: Int,
                        dataWidth: Int) extends Bundle {
  val address = UInt(postTranslationWidth bits)
  val data = Bits(dataWidth bits)
  val size = UInt(log2Up(log2Up(dataWidth/8)+1) bits)
  val generation  = Bool()
}

case class DataStoreRsp(dataWidth : Int, refillCount : Int) extends Bundle {
  val fault = Bool()
  val redo = Bool()
  val refillSlot = Bits(refillCount bits) //Zero when refillSlotAny
  val refillSlotAny = Bool() //Not valid if !miss
  val generationKo = Bool() //Ignore everything else if this is set
}

case class DataMemReadCmd(addressWidth: Int,
                      dataWidth: Int) extends Bundle {
  val address = UInt(addressWidth bits)
}

case class DataMemReadRsp(dataWidth: Int) extends Bundle {
  val data = Bits(dataWidth bits)
  val error = Bool()
}

case class DataMemReadBus(addressWidth: Int,
                          dataWidth: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataMemReadCmd(addressWidth, dataWidth))
  val rsp = Flow(DataMemReadRsp(dataWidth))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

case class DataMemWriteCmd(addressWidth: Int,
                           dataWidth: Int) extends Bundle {
  val address = UInt(addressWidth bits)
  val data    = Bits(dataWidth bits)
  val mask    = Bits(dataWidth/8 bits)

}

case class DataMemWriteRsp(dataWidth: Int) extends Bundle {
  val error = Bool()
}

case class DataMemWriteBus(addressWidth: Int,
                           dataWidth: Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DataMemWriteCmd(addressWidth, dataWidth))
  val rsp = Flow(DataMemWriteRsp(dataWidth))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}


case class DataMemBus(addressWidth: Int,
                           dataWidth: Int) extends Bundle with IMasterSlave {
  val read = master(DataMemReadBus(addressWidth, dataWidth))
  val write = master(DataMemWriteBus(addressWidth, dataWidth))

  override def asMaster() = {
    master(read, write)
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
                val loadRspAt: Int = 2,
                val reducedBankWidth : Boolean = false
               ) extends Component {
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
    val store = slave(DataStorePort(
      postTranslationWidth = postTranslationWidth,
      dataWidth     = cpuDataWidth,
      refillCount =  refillCount
    ))
    val mem = master(DataMemBus(postTranslationWidth, memDataWidth))
    val refillCompletions = out Bits(refillCount bits)
  }

  io.mem.write.flatten.filter(_.isOutput).foreach(_.assignDontCare())





  val cpuWordWidth = cpuDataWidth
  val bytePerMemWord = memDataWidth/8
  val bytePerFetchWord = cpuDataWidth/8
  val waySize = cacheSize/wayCount
  val linePerWay = waySize/lineSize
  val memDataPerWay = waySize/bytePerMemWord
  val memData = HardType(Bits(memDataWidth bits))
  val memWordPerLine = lineSize/bytePerMemWord
  val tagWidth = preTranslationWidth-log2Up(waySize)


  val tagRange = preTranslationWidth-1 downto log2Up(linePerWay*lineSize)
  val lineRange = tagRange.low-1 downto log2Up(lineSize)
  val refillRange = tagRange.high downto lineRange.low

  val bankCount = wayCount
  val bankWidth =  if(!reducedBankWidth) memDataWidth else Math.max(cpuWordWidth, memDataWidth/wayCount)
  val bankByteSize = cacheSize/bankCount
  val bankWordCount = bankByteSize*8/bankWidth
  val bankWordToCpuWordRange = log2Up(bankWidth/8)-1 downto log2Up(bytePerFetchWord)
  val memToBankRatio = bankWidth*bankCount / memDataWidth
  val bankWord = HardType(Bits(bankWidth bits))


  val ADDRESS_PRE_TRANSLATION = Stageable(UInt(preTranslationWidth bits))
  val ADDRESS_POST_TRANSLATION = ADDRESS_PRE_TRANSLATION //Stageable(UInt(postTranslationWidth bits))  //TODO for now
  val CPU_WORD = Stageable(Bits(cpuWordWidth bits))
  val CPU_MASK = Stageable(Bits(cpuWordWidth/8 bits))
  val TAGS_CORRUPTED = Stageable(Bool())


  case class Tag() extends Bundle{
    val loaded = Bool()
    val error  = Bool()
    val address = UInt(tagWidth bits)
  }

  val BANKS_WORDS = Stageable(Vec.fill(bankCount)(bankWord()))
  val WAYS_TAGS = Stageable(Vec.fill(wayCount)(Tag()))
  val WAYS_HITS = Stageable(Vec.fill(wayCount)(Bool()))
  val WAYS_HIT = Stageable(Bool())
  val MISS = Stageable(Bool())
  val REDO = Stageable(Bool())
  val SIZE = Stageable(UInt(log2Up(log2Up(cpuDataWidth/8)+1) bits))
  val REFILL_SLOT = Stageable(Bits(refillCount bits))
  val REFILL_SLOT_FULL = Stageable(Bool())
  val GENERATION, GENERATION_OK = Stageable(Bool())

  val BANKS_MUXES = Stageable(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))

  val banks = for(id <- 0 until bankCount) yield new Area{
    val mem = Mem(Bits(bankWidth bits), bankWordCount)
    val write = mem.writePortWithMask
    write.mask.setWidth(mem.getWidth/8)
    val read = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)
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
    val loadRead = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)
    }
    val storeRead = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)
    }
  }

  val flush = new Area{
    val counter = Reg(UInt(log2Up(linePerWay)+1 bits)) init(0)
    val done = counter.msb
    when(!done){
      counter := counter + 1
    }

    when(!done) {
      waysWrite.mask.setAll()
      waysWrite.address := counter.resized
      waysWrite.tag.loaded := False
    }

//    readStage.haltIt(!done)
  }


  val refill = new Area {
    val slots = for (refillId <- 0 until refillCount) yield new Area {
      val id = refillId
      val fire = False
      val valid = RegInit(False) clearWhen (fire)
      val address = KeepAttribute(Reg(UInt(postTranslationWidth bits)))
      val hadError = RegInit(False) clearWhen (fire)


      val cmdSent = RegInit(False) setWhen (io.mem.read.cmd.fire) clearWhen (fire)
      io.mem.read.cmd.valid := valid && !cmdSent
      io.mem.read.cmd.address := address(tagRange.high downto lineRange.low) @@ U(0, lineRange.low bit)

      val wayToAllocate = Counter(wayCount, !valid)
      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      io.refillCompletions(id) := fire //Maybe pipelined

      when(flush.done) {
        waysWrite.mask(wayToAllocate) setWhen (fire)
        waysWrite.address := address(lineRange)
        waysWrite.tag.loaded := True
        waysWrite.tag.error := hadError || io.mem.read.rsp.error
        waysWrite.tag.address := address(tagRange)
      }


      for ((bank, bankId) <- banks.zipWithIndex) {
        if (!reducedBankWidth) {
          bank.write.valid := io.mem.read.rsp.valid && wayToAllocate === bankId
          bank.write.address := address(lineRange) @@ wordIndex
          bank.write.data := io.mem.read.rsp.data
        } else {
          val sel = U(bankId) - wayToAllocate.value
          val groupSel = wayToAllocate(log2Up(bankCount) - 1 downto log2Up(bankCount / memToBankRatio))
          val subSel = sel(log2Up(bankCount / memToBankRatio) - 1 downto 0)
          bank.write.valid := io.mem.read.rsp.valid && groupSel === (bankId >> log2Up(bankCount / memToBankRatio))
          bank.write.address := address(lineRange) @@ wordIndex @@ (subSel)
          bank.write.data := io.mem.read.rsp.data.subdivideIn(bankCount / memToBankRatio slices)(subSel)
        }
        banks(bankId).write.mask := (default -> true)
      }


      when(io.mem.read.rsp.valid) {
        wordIndex := (wordIndex + 1).resized
        hadError.setWhen(io.mem.read.rsp.error)
        when(wordIndex === wordIndex.maxValue) {
          fire := True
        }
      }

      //    pipeline.stages(0).haltIt(valid)
    }

    val free = B(OHMasking.first(slots.map(!_.valid)))
    val full = slots.map(_.valid).andR

    val banksWrite = CombInit(io.mem.read.rsp.valid)
    val push = Flow(new Bundle{
      val address = UInt(postTranslationWidth bits)
    }).setIdle()

    for (slot <- slots) when(push.valid && free(slot.id)) {
      slot.valid := True
      slot.address := push.address
    }
  }

  def tagsCorrupted(stages : Seq[Stage], address : Stageable[UInt]): Unit ={
    for(s <- stages){
      s.overloaded(TAGS_CORRUPTED) := s(TAGS_CORRUPTED) || waysWrite.mask =/= 0 && waysWrite.address === s(address)(lineRange)
    }
  }

  val load = new Area {
    val pipeline = new Pipeline{
      val stages = Array.fill(loadRspAt+1)(newStage())
      connect(stages)(List(M2S()))

      for((stage, stageId) <- stages.zipWithIndex){
        stage.throwIt(io.load.cancels(stageId))
      }
    }

    val readStage      = pipeline.stages(loadReadAt)
    val hitsStage      = pipeline.stages(loadHitsAt)
    val hitStage       = pipeline.stages(loadHitAt)
    val bankMuxesStage = pipeline.stages(loadBankMuxesAt)
    val bankMuxStage   = pipeline.stages(loadBankMuxAt)
    val controlStage   = pipeline.stages(loadControlAt)
    val rspStage = pipeline.stages(loadRspAt)


    tagsCorrupted(pipeline.stages.takeWhile(_ != controlStage), ADDRESS_PRE_TRANSLATION)
    val start = new Area {
      val stage = pipeline.stages.head

      import stage._

      isValid := io.load.cmd.valid
      ADDRESS_PRE_TRANSLATION := io.load.cmd.virtual
      SIZE := io.load.cmd.size
      TAGS_CORRUPTED := False
    }

    val fetch = new Area {
      for ((bank, bankId) <- banks.zipWithIndex) yield new Area {
        {
          import readStage._
          bank.read.cmd.valid := !isStuck
          bank.read.cmd.payload := ADDRESS_PRE_TRANSLATION(lineRange.high downto log2Up(bankWidth / 8))
        }
        pipeline.stages(loadReadAt + 1)(BANKS_WORDS)(bankId) := banks(bankId).read.rsp;
        {
          import bankMuxesStage._;
          BANKS_MUXES(bankId) := BANKS_WORDS(bankId).subdivideIn(cpuWordWidth bits).read(ADDRESS_PRE_TRANSLATION(bankWordToCpuWordRange))
        }

      }

      val bankMux = new Area {
        import bankMuxStage._
        val wayId = OHToUInt(WAYS_HITS)
        val bankId = if(!reducedBankWidth) wayId else (wayId >> log2Up(bankCount/memToBankRatio)) @@ ((wayId + (ADDRESS_PRE_TRANSLATION(log2Up(bankWidth/8), log2Up(bankCount) bits))).resize(log2Up(bankCount/memToBankRatio)))
        CPU_WORD := BANKS_MUXES.read(bankId) //MuxOH(WAYS_HITS, BANKS_MUXES)
      }

      for ((way, wayId) <- ways.zipWithIndex) yield new Area {
        {
          import readStage._
          way.loadRead.cmd.valid := !isStuck
          way.loadRead.cmd.payload := ADDRESS_PRE_TRANSLATION(lineRange)
        }
        pipeline.stages(loadReadAt + 1)(WAYS_TAGS)(wayId) := ways(wayId).loadRead.rsp;
        {
          import hitsStage._;
          WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === ADDRESS_POST_TRANSLATION(tagRange)
        }
      }

      {
        import hitStage._;
        WAYS_HIT := B(WAYS_HITS).orR
      }
    }

    val ctrl = new Area {

      import controlStage._

      val refillHits = B(refill.slots.map(r => r.valid && r.address(refillRange) === ADDRESS_POST_TRANSLATION(refillRange)))
      val refillHit = refillHits.orR

      REDO := !WAYS_HIT || TAGS_CORRUPTED
      MISS := !WAYS_HIT && !TAGS_CORRUPTED
      REFILL_SLOT_FULL := !refillHit && refill.full
      REFILL_SLOT := refillHit mux (
        True  -> refillHits,
        False -> (TAGS_CORRUPTED ? REFILL_SLOT.getZero | refill.free)
      )

      val refillStarted = isValid && MISS && !refillHit && !refill.full
      when(refillStarted){
        refill.push.valid := True
        refill.push.address := ADDRESS_POST_TRANSLATION
      }
    }

    val inject = new Area {

      import rspStage._
      assert(rspStage == controlStage, "Need to implement refillSlot bypass otherwise")
      io.load.rsp.valid := isValid
      io.load.rsp.data := CPU_WORD
      io.load.rsp.fault := False //TODO
      io.load.rsp.redo := REDO
      io.load.rsp.refillSlotAny := REFILL_SLOT_FULL
      io.load.rsp.refillSlot := REFILL_SLOT
    }

    pipeline.build()
  }

  val store = new Area{
    val pipeline = new Pipeline{
      val stages = Array.fill(loadRspAt+1)(newStage())
      connect(stages)(List(M2S()))

      val discardAll = False
      for((stage, stageId) <- stages.zipWithIndex){
        stage.throwIt(discardAll)
      }
    }

    val readStage      = pipeline.stages(loadReadAt)
    val hitsStage      = pipeline.stages(loadHitsAt)
    val hitStage       = pipeline.stages(loadHitAt)
    val bankMuxesStage = pipeline.stages(loadBankMuxesAt)
    val bankMuxStage   = pipeline.stages(loadBankMuxAt)
    val controlStage   = pipeline.stages(loadControlAt)
    val rspStage = pipeline.stages(loadRspAt)

    val target = RegInit(False)

    tagsCorrupted(pipeline.stages.takeWhile(_ != controlStage), ADDRESS_PRE_TRANSLATION)
    val start = new Area {
      val stage = pipeline.stages.head

      import stage._

      isValid := io.store.cmd.valid
      ADDRESS_POST_TRANSLATION := io.store.cmd.address
      CPU_WORD.assignDontCare()
      switch(io.store.cmd.size){
        for(s <- 0 to log2Up(cpuDataWidth/8)) is(s){
          val w = (1 << s)*8
          CPU_WORD.subdivideIn(w bits).foreach(_ := io.store.cmd.data(0, w bits))
        }
      }

      SIZE := io.store.cmd.size
      CPU_MASK := AddressToMask(io.store.cmd.address, io.store.cmd.size, widthOf(CPU_MASK))
      GENERATION := io.store.cmd.generation
      TAGS_CORRUPTED := False
    }

    val fetch = new Area {
      for ((way, wayId) <- ways.zipWithIndex) yield new Area {
        {
          import readStage._
          way.storeRead.cmd.valid := !isStuck
          way.storeRead.cmd.payload := ADDRESS_POST_TRANSLATION(lineRange)
        }
        pipeline.stages(loadReadAt + 1)(WAYS_TAGS)(wayId) := ways(wayId).storeRead.rsp;
        {
          import hitsStage._;
          WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === ADDRESS_POST_TRANSLATION(tagRange)
        }
      }

      {
        import hitStage._;
        WAYS_HIT := B(WAYS_HITS).orR
      }
    }

    val ctrl = new Area {
      import controlStage._

      GENERATION_OK := GENERATION === target

      val refillHits = B(refill.slots.map(r => r.valid && r.address(refillRange) === ADDRESS_POST_TRANSLATION(refillRange)))
      val refillHit = refillHits.orR

      REDO := MISS || TAGS_CORRUPTED || refill.banksWrite
      MISS := !WAYS_HIT && !TAGS_CORRUPTED
      REFILL_SLOT_FULL := !refillHit && refill.full
      REFILL_SLOT :=  refillHit mux (
        True  -> refillHits,
        False -> (TAGS_CORRUPTED ? REFILL_SLOT.getZero | refill.free)
      )

      val refillStarted = isValid && GENERATION_OK && MISS && !refillHit && !refill.full && !load.ctrl.refillStarted
      when(refillStarted){
        refill.push.valid := True
        refill.push.address := ADDRESS_POST_TRANSLATION
      }

      val writeCache = isValid && GENERATION_OK && !MISS && !TAGS_CORRUPTED && !refill.banksWrite
      val wayId = OHToUInt(WAYS_HITS)
      val bankHitId = if(!reducedBankWidth) wayId else (wayId >> log2Up(bankCount/memToBankRatio)) @@ ((wayId + (ADDRESS_PRE_TRANSLATION(log2Up(bankWidth/8), log2Up(bankCount) bits))).resize(log2Up(bankCount/memToBankRatio)))

      when(writeCache){
        for((bank, bankId) <- banks.zipWithIndex){
          bank.write.valid := bankId === bankHitId
          bank.write.address := ADDRESS_POST_TRANSLATION(lineRange.high downto log2Up(bankWidth / 8))
          bank.write.data.subdivideIn(cpuWordWidth bits).foreach(_ := CPU_WORD)
          bank.write.mask := 0
          bank.write.mask.subdivideIn(cpuWordWidth/8 bits).write(ADDRESS_POST_TRANSLATION(bankWordToCpuWordRange), CPU_MASK)
        }
      }


      when(isValid && REDO && GENERATION_OK){
        target := !target
      }
    }

    val inject = new Area {
      import rspStage._

      assert(rspStage == controlStage, "Need to implement refillSlot bypass otherwise")
      io.store.rsp.valid := isValid
      io.store.rsp.generationKo := !GENERATION_OK
      io.store.rsp.fault := False //TODO
      io.store.rsp.redo := REDO
      io.store.rsp.refillSlotAny := REFILL_SLOT_FULL
      io.store.rsp.refillSlot := REFILL_SLOT
    }
    pipeline.build()
  }

//  io.flatten.filter(_.isOutput).map(_.assignDontCare())
}