package naxriscv.units.lsu

import naxriscv.utilities.{AddressToMask, Reservation}
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


case class DataMemBusParameter( addressWidth: Int,
                                dataWidth: Int,
                                readIdWidth: Int,
                                writeIdWidth: Int)

case class DataMemReadCmd(p : DataMemBusParameter) extends Bundle {
  val id = UInt(p.readIdWidth bits)
  val address = UInt(p.addressWidth bits)
}

case class DataMemReadRsp(p : DataMemBusParameter) extends Bundle {
  val id = UInt(p.readIdWidth bits)
  val data = Bits(p.dataWidth bits)
  val error = Bool()
}

case class DataMemReadBus(p : DataMemBusParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(DataMemReadCmd(p))
  val rsp = Flow(DataMemReadRsp(p))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

case class DataMemWriteCmd(p : DataMemBusParameter) extends Bundle {
  val address = UInt(p.addressWidth bits)
  val data    = Bits(p.dataWidth bits)
  val mask    = Bits(p.dataWidth/8 bits)
  val id = UInt(p.writeIdWidth bits)
}

case class DataMemWriteRsp(p : DataMemBusParameter) extends Bundle {
  val error = Bool()
  val id = UInt(p.writeIdWidth bits)
}

case class DataMemWriteBus(p : DataMemBusParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(DataMemWriteCmd(p))
  val rsp = Flow(DataMemWriteRsp(p))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}


case class DataMemBus(p : DataMemBusParameter) extends Bundle with IMasterSlave {
  val read = master(DataMemReadBus(p))
  val write = master(DataMemWriteBus(p))

  override def asMaster() = {
    master(read, write)
  }
}


class DataCache(val cacheSize: Int,
                val wayCount: Int,
                val refillCount : Int,
                val writebackCount : Int,
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
                val storeReadAt: Int = 0,
                val storeHitsAt: Int = 1,
                val storeHitAt: Int = 1,
                val storeControlAt: Int = 2,
                val storeRspAt: Int = 2,
                val reducedBankWidth : Boolean = false
               ) extends Component {

  val memParameter = DataMemBusParameter(
    addressWidth  = postTranslationWidth,
    dataWidth     = memDataWidth,
    readIdWidth   = log2Up(refillCount),
    writeIdWidth  = log2Up(writebackCount)
  )

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
    val mem = master(DataMemBus(memParameter))
    val refillCompletions = out Bits(refillCount bits)
  }

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
  val bankWordPerLine = lineSize*8/bankWidth

  assert(bankWidth <= memDataWidth)


  val ADDRESS_PRE_TRANSLATION = Stageable(UInt(preTranslationWidth bits))
  val ADDRESS_POST_TRANSLATION = ADDRESS_PRE_TRANSLATION //Stageable(UInt(postTranslationWidth bits))  //TODO for now
  val CPU_WORD = Stageable(Bits(cpuWordWidth bits))
  val CPU_MASK = Stageable(Bits(cpuWordWidth/8 bits))
//  val TAGS_CORRUPTED = Stageable(Bool())
  val WAYS_HAZARD = Stageable(Bits(wayCount bits))
  val BANK_BUSY = Stageable(Bits(bankCount bits))


  case class Tag() extends Bundle{
    val loaded = Bool()
    val address = UInt(tagWidth bits)
  }

  case class Status() extends Bundle{
    val dirty = Bool()
  }

  val STATUS = Stageable(Vec.fill(wayCount)(Status()))
  val BANKS_WORDS = Stageable(Vec.fill(bankCount)(bankWord()))
  val WAYS_TAGS = Stageable(Vec.fill(wayCount)(Tag()))
  val WAYS_HITS = Stageable(Bits(wayCount bits))
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
      val usedByWriteBack = False
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)

      cmd.setIdle() //TODO revert it !
    }
  }

  val tagsOrStatusWriteArbitration = new Reservation()
  val waysWrite = new Area{
    val mask = Bits(wayCount bits)
    val address = UInt(log2Up(linePerWay) bits)
    val tag = Tag()

    mask := 0
    address.assignDontCare()
    tag.assignDontCare()

    //Used for hazard tracking in a pipelined way
    val maskLast = RegNext(mask)
    val addressLast = RegNext(address)
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


  val status = new Area{
    val mem = Mem.fill(linePerWay)(Vec.fill(wayCount)(Status()))
    val write = mem.writePort.setIdle()
    val loadRead = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)
    }
    val storeRead = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = mem.readSync(cmd.payload, cmd.valid)
    }
    val writeLast = write.stage()
//    def bypass(status : Vec[Status], address : UInt) : Vec[Status] = {
//      val ret = CombInit(status)
//      when(write.valid && write.address === address(lineRange)){
//        ret := write.data
//      }
//      ret
//    }
    def bypass(stage: Stage): Unit ={
      stage.overloaded(STATUS) := stage(STATUS)
      when(write.valid && write.address === stage(ADDRESS_PRE_TRANSLATION)(lineRange)){
        stage.overloaded(STATUS)  := write.data
      }
    }
  }

  val wayRandom = CounterFreeRun(wayCount)

  val flush = new Area{
    val counter = Reg(UInt(log2Up(linePerWay)+1 bits)) init(0)
    val done = counter.msb
    val reservation = tagsOrStatusWriteArbitration.create(0)
    when(!done && reservation.win){
      reservation.takeIt()
      counter := counter + 1
      waysWrite.mask.setAll()
      waysWrite.address := counter.resized
      waysWrite.tag.loaded := False
    }
  }

  class PriorityArea(slots : Seq[(Bool, Bits)]) extends Area{
    val slotsWithId = slots.zipWithIndex.map(e => (e._1._1, e._1._2, e._2))
    val hits = B(slots.map(_._1))
    val hit = hits.orR
    val oh = hits & B(slotsWithId.map(slot => (B(slotsWithId.filter(_ != slot).map(other => hits(other._3))) & slot._2) === 0))
    val sel = OHToUInt(oh)
    val lock = RegNext(oh) init(0)
    when(lock.orR){ oh := lock }
  }

  val refill = new Area {
    val slots = for (refillId <- 0 until refillCount) yield new Area {
      val id = refillId
      val valid = RegInit(False)
      val address = KeepAttribute(Reg(UInt(postTranslationWidth bits)))
      val way = Reg(UInt(log2Up(wayCount) bits))
      val cmdSent = Reg(Bool())
      val priority = Reg(Bits(refillCount-1 bits)) //TODO Check it

      val loaded = Reg(Bool())
      val loadedCounterMax = loadControlAt-1
      val loadedCounter = Reg(UInt(log2Up(loadedCounterMax+1) bits))
      loadedCounter := loadedCounter + U(loaded)
      valid clearWhen (loadedCounter === loadedCounterMax)
    }
    def isLineBusy(address : UInt, way : UInt) = slots.map(s => s.valid && s.way === way && s.address(lineRange) === address(lineRange)).orR

    val free = B(OHMasking.first(slots.map(!_.valid)))
    val full = slots.map(_.valid).andR

    val banksWrite = CombInit(io.mem.read.rsp.valid)
    val push = Flow(new Bundle{
      val address = UInt(postTranslationWidth bits)
      val way = UInt(log2Up(wayCount) bits)
    }).setIdle()

    for (slot <- slots;
         (other, prio) <- (slots.filter(_ != slot), slot.priority.asBools).zipped ) {
      prio.clearWhen(other.valid)
    }

    for (slot <- slots) when(push.valid && free(slot.id)) {
      slot.valid := True
      slot.address := push.address
      slot.way := push.way
      slot.cmdSent := False
      slot.priority.setAll()
      slot.loaded := False
      slot.loadedCounter := 0
    }

    val read = new Area{
      val arbiter = new PriorityArea(slots.map(s => (s.valid && !s.cmdSent, s.priority)))
      when(io.mem.read.cmd.fire){ arbiter.lock := 0 }

      io.mem.read.cmd.valid := arbiter.hit
      io.mem.read.cmd.id := arbiter.sel
      io.mem.read.cmd.address := slots.map(_.address(tagRange.high downto lineRange.low)).read(arbiter.sel) @@ U(0, lineRange.low bit)
      for(s <- slots) s.cmdSent setWhen(arbiter.oh(s.id) && io.mem.read.cmd.ready)

      val address = slots.map(_.address).read(io.mem.read.rsp.id)
      val way = slots.map(_.way).read(io.mem.read.rsp.id)
      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      for ((bank, bankId) <- banks.zipWithIndex) {
        if (!reducedBankWidth) {
          bank.write.valid := io.mem.read.rsp.valid && way=== bankId
          bank.write.address := address(lineRange) @@ wordIndex
          bank.write.data := io.mem.read.rsp.data
        } else {
          val sel = U(bankId) - way
          val groupSel = way(log2Up(bankCount) - 1 downto log2Up(bankCount / memToBankRatio))
          val subSel = sel(log2Up(bankCount / memToBankRatio) - 1 downto 0)
          bank.write.valid := io.mem.read.rsp.valid && groupSel === (bankId >> log2Up(bankCount / memToBankRatio))
          bank.write.address := address(lineRange) @@ wordIndex @@ (subSel)
          bank.write.data := io.mem.read.rsp.data.subdivideIn(bankCount / memToBankRatio slices)(subSel)
        }
        banks(bankId).write.mask := (default -> true)
      }

      val hadError = RegInit(False) setWhen(io.mem.read.rsp.valid && io.mem.read.rsp.error)
      val fire = False
      io.refillCompletions := 0
      when(io.mem.read.rsp.valid) {
        wordIndex := wordIndex + 1
        when(wordIndex === wordIndex.maxValue) {
          hadError := False
          slots.map(_.loaded).write(io.mem.read.rsp.id, True)
          fire := True
          io.refillCompletions(io.mem.read.rsp.id) := True
        }
      }
    }
  }

  val writeback = new Area{
    val slots = for (writebackId <- 0 until writebackCount) yield new Area {
      val id = writebackId
      val fire = False
      val valid = RegInit(False) clearWhen (fire)
      val address = KeepAttribute(Reg(UInt(postTranslationWidth bits)))
      val way = Reg(UInt(log2Up(wayCount) bits))
      val priority = Reg(Bits(writebackCount-1 bits)) //TODO Check it
      val readCmdDone = Reg(Bool())
      val victimBufferReady = Reg(Bool())
      val readRspDone = Reg(Bool())
      val writeCmdDone = Reg(Bool())
    }

    def isLineBusy(address : UInt, way : UInt) = False//slots.map(s => s.valid && s.way === way && s.address(lineRange) === address(lineRange)).orR

    val free = B(OHMasking.first(slots.map(!_.valid)))
    val full = slots.map(_.valid).andR

    val banksWrite = CombInit(io.mem.read.rsp.valid)
    val push = Flow(new Bundle{
      val address = UInt(postTranslationWidth bits)
      val way = UInt(log2Up(wayCount) bits)
    }).setIdle()

    for (slot <- slots; (other, prio) <- (slots.filter(_ != slot), slot.priority.asBools).zipped ) {
      prio.clearWhen(other.valid)
    }

    for (slot <- slots) when(push.valid && free(slot.id)) {
      slot.valid := True
      slot.address := push.address
      slot.way := push.way
      slot.readCmdDone := False
      slot.readRspDone := False
      slot.victimBufferReady := False
      slot.writeCmdDone := False
      slot.priority.setAll()
    }


    val victimBuffer = Mem.fill(writebackCount*memWordPerLine)(Bits(memDataWidth bits))
    val read = new Area{
      val arbiter = new PriorityArea(slots.map(s => (s.valid && !s.readCmdDone, s.priority)))

      val address = slots.map(_.address).read(arbiter.sel)
      val way = slots.map(_.way).read(arbiter.sel)
      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      val slotRead = Flow(new Bundle {
        val id = UInt(log2Up(writebackCount) bits)
        val last = Bool()
        val wordIndex = UInt(log2Up(memWordPerLine) bits)
        val way = UInt(log2Up(wayCount) bits)
      })
      slotRead.valid := arbiter.hit
      slotRead.id := arbiter.sel
      slotRead.wordIndex := wordIndex
      slotRead.way := way
      slotRead.last := wordIndex === wordIndex.maxValue
      wordIndex := wordIndex + U(slotRead.valid)
      when(slotRead.valid && slotRead.last){
        whenMasked(slots, arbiter.oh){ _.readCmdDone := True }
        arbiter.lock := 0
      }

      for ((bank, bankId) <- banks.zipWithIndex) {
        if (!reducedBankWidth) {
          when(slotRead.valid && way === bankId) {
            bank.read.cmd.valid := True
            bank.read.cmd.payload := address(lineRange) @@ wordIndex
            bank.read.usedByWriteBack := True
          }
        } else {
          val sel = U(bankId) - way
          val groupSel = way(log2Up(bankCount) - 1 downto log2Up(bankCount / memToBankRatio))
          val subSel = sel(log2Up(bankCount / memToBankRatio) - 1 downto 0)
          when(arbiter.hit && groupSel === (bankId >> log2Up(bankCount / memToBankRatio))) {
            bank.read.cmd.valid := True
            bank.read.cmd.payload := address(lineRange) @@ wordIndex @@ (subSel)
            bank.read.usedByWriteBack := True
          }
        }
      }

      val slotReadLast = slotRead.stage()
      val readedData = Bits(memDataWidth bits)

      if (!reducedBankWidth) {
        readedData := banks.map(_.read.rsp).read(slotRead.way)
      } else {
        for((slice, sliceId) <- readedData.subdivideIn(bankWidth bits).zipWithIndex) {
          ???
        }
      }


      when(slotReadLast.valid){
        victimBuffer.write(slotReadLast.id @@ slotReadLast.wordIndex, readedData)
        whenIndexed(slots, slotReadLast.id) { _.victimBufferReady := True }
        when(slotReadLast.last) {
          whenIndexed(slots, slotReadLast.id) { _.readRspDone := True }
        }
      }
    }

    val write = new Area{
      val arbiter = new PriorityArea(slots.map(s => (s.valid && s.victimBufferReady && !s.writeCmdDone, s.priority)))
      val wordIndex = KeepAttribute(Reg(UInt(log2Up(memWordPerLine) bits)) init (0))

      val bufferRead = Stream(new Bundle {
        val id = UInt(log2Up(writebackCount) bits)
        val address = UInt(postTranslationWidth bits)
      })
      bufferRead.valid := arbiter.hit
      bufferRead.id := arbiter.sel
      bufferRead.address := slots.map(_.address).read(arbiter.sel)
      wordIndex := wordIndex + U(bufferRead.fire)
      when(bufferRead.fire && wordIndex === wordIndex.maxValue){
        whenMasked(slots, arbiter.oh)(_.writeCmdDone := True)
      }

      val cmd = bufferRead.stage()
      val word = victimBuffer.readSync(arbiter.sel @@ wordIndex, bufferRead.ready)
      io.mem.write.cmd.arbitrationFrom(cmd)
      io.mem.write.cmd.address := cmd.address
      io.mem.write.cmd.data := word
      io.mem.write.cmd.mask.setAll()
      io.mem.write.cmd.id := cmd.id

      when(io.mem.write.rsp.valid){
        whenIndexed(slots, io.mem.write.rsp.id)(_.fire := True)
      }
    }
  }

  def isLineBusy(address : UInt, way : UInt) = refill.isLineBusy(address, way) || writeback.isLineBusy(address, way)



  def waysHazard(stages : Seq[Stage], address : Stageable[UInt]): Unit ={
    for(s <- stages){
      s.overloaded(WAYS_HAZARD) := s(WAYS_HAZARD) | waysWrite.maskLast.andMask(waysWrite.addressLast === s(address)(lineRange))
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


    waysHazard((loadReadAt+1 to loadReadAt+1).map(pipeline.stages(_)), ADDRESS_PRE_TRANSLATION)
    val start = new Area {
      val stage = pipeline.stages.head

      import stage._

      isValid := io.load.cmd.valid
      ADDRESS_PRE_TRANSLATION := io.load.cmd.virtual
      SIZE := io.load.cmd.size
      WAYS_HAZARD := 0
    }

    val fetch = new Area {
      for ((bank, bankId) <- banks.zipWithIndex) yield new Area {
        {
          import readStage._
          BANK_BUSY(bankId) := bank.read.usedByWriteBack
          when(!BANK_BUSY(bankId)) { //Not the best way of muxing it
            bank.read.cmd.valid := !isStuck
            bank.read.cmd.payload := ADDRESS_PRE_TRANSLATION(lineRange.high downto log2Up(bankWidth / 8))
          }
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


      status.loadRead.cmd.valid := !readStage.isStuck
      status.loadRead.cmd.payload := readStage(ADDRESS_PRE_TRANSLATION)(lineRange)
      pipeline.stages(loadReadAt + 1)(STATUS) := status.loadRead.rsp
      (loadReadAt + 1 to loadControlAt).map(pipeline.stages(_)).foreach(stage => status.bypass(stage))
    }

    val ctrl = new Area {
      import controlStage._

      //Cases :
      //-  HIT
      //-  HIT read was busy => HAZARD
      //-  HIT but also in refill => HAZARD
      //- !HIT and in refill => REDO
      //- !HIT and not in refill => REFILL
      //- !HIT and not in refill but refill full => REDO
      val reservation = tagsOrStatusWriteArbitration.create(1)
      val refillWay = CombInit(wayRandom.value)
      val refillWayNeedWriteback = WAYS_TAGS(refillWay).loaded && STATUS(refillWay).dirty
      val refillHits = B(refill.slots.map(r => r.valid && r.address(refillRange) === ADDRESS_POST_TRANSLATION(refillRange))) //TODO a bit heavy, could pipeline ?
      val refillHit = refillHits.orR
      val refillLoaded = (B(refill.slots.map(_.loaded)) & refillHits).orR
      val refillLineBusy = isLineBusy(ADDRESS_PRE_TRANSLATION, refillWay)
      val bankBusy = (BANK_BUSY & WAYS_HITS) =/= 0
      val waysHitHazard = (WAYS_HITS & resulting(WAYS_HAZARD)).orR

      REDO := !WAYS_HIT || waysHitHazard || bankBusy || refillHit
      MISS := !WAYS_HIT && !waysHitHazard && !refillHit
      val canRefill = !refill.full && !refillLineBusy && reservation.win && !(refillWayNeedWriteback && writeback.full)
      val askRefill = MISS && canRefill && !refillHit
      val startRefill = isValid && askRefill

      when(startRefill){
        reservation.takeIt()

        refill.push.valid := True
        refill.push.address := ADDRESS_POST_TRANSLATION
        refill.push.way := refillWay

        waysWrite.mask(refillWay) := True
        waysWrite.address := ADDRESS_PRE_TRANSLATION(lineRange)
        waysWrite.tag.loaded := True
        waysWrite.tag.address := ADDRESS_PRE_TRANSLATION(tagRange)

        status.write.valid := True
        status.write.address := ADDRESS_PRE_TRANSLATION(lineRange)
        status.write.data := STATUS
        status.write.data(refillWay).dirty := False

        writeback.push.valid := refillWayNeedWriteback
        writeback.push.address := (WAYS_TAGS(refillWay).address @@ ADDRESS_PRE_TRANSLATION(lineRange)) << lineRange.low
        writeback.push.way := refillWay
      }

      REFILL_SLOT_FULL := MISS && !refillHit && refill.full
      REFILL_SLOT := refillHits.andMask(!refillLoaded) | refill.free.andMask(askRefill)
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

    val readStage      = pipeline.stages(storeReadAt)
    val hitsStage      = pipeline.stages(storeHitsAt)
    val hitStage       = pipeline.stages(storeHitAt)
    val controlStage   = pipeline.stages(storeControlAt)
    val rspStage       = pipeline.stages(storeRspAt)

    val target = RegInit(False)

    waysHazard((storeReadAt+1 to storeControlAt).map(pipeline.stages(_)), ADDRESS_PRE_TRANSLATION)
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
      WAYS_HAZARD := 0
    }

    val fetch = new Area {
      for ((way, wayId) <- ways.zipWithIndex) yield new Area {
        {
          import readStage._
          way.storeRead.cmd.valid := !isStuck
          way.storeRead.cmd.payload := ADDRESS_POST_TRANSLATION(lineRange)
        }
        pipeline.stages(storeReadAt + 1)(WAYS_TAGS)(wayId) := ways(wayId).storeRead.rsp;
        {
          import hitsStage._;
          WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === ADDRESS_POST_TRANSLATION(tagRange)
        }
      }

      {
        import hitStage._;
        WAYS_HIT := B(WAYS_HITS).orR
      }

      status.storeRead.cmd.valid := !readStage.isStuck
      status.storeRead.cmd.payload := readStage(ADDRESS_PRE_TRANSLATION)(lineRange)
      pipeline.stages(storeReadAt + 1)(STATUS) := status.storeRead.rsp
      (storeReadAt + 1 to storeControlAt).map(pipeline.stages(_)).foreach(stage => status.bypass(stage))
    }

    val ctrl = new Area {
      import controlStage._

      GENERATION_OK := GENERATION === target

//      val refillOverlap = B(refill.slots.map(r => r.valid && (r.way & WAYS_HIT).orR && r.address(lineRange) === ADDRESS_POST_TRANSLATION(lineRange))) //TODO
      val reservation = tagsOrStatusWriteArbitration.create(2)
      val refillWay = CombInit(wayRandom.value)
      val refillWayNeedWriteback = WAYS_TAGS(refillWay).loaded && STATUS(refillWay).dirty
      val refillHits = B(refill.slots.map(r => r.valid && r.address(refillRange) === ADDRESS_POST_TRANSLATION(refillRange)))
      val refillHit = refillHits.orR
      val refillLoaded = (B(refill.slots.map(_.loaded)) & refillHits).orR
      val refillLineBusy = isLineBusy(ADDRESS_PRE_TRANSLATION, refillWay)
      val waysHitHazard = (WAYS_HITS & resulting(WAYS_HAZARD)).orR
      val wasClean = (B(STATUS.map(_.dirty)) & WAYS_HITS).orR

      REDO := MISS || waysHitHazard || refill.banksWrite || refillHit || (wasClean && !reservation.win)// || refillOverlap
      MISS := !WAYS_HIT && !waysHitHazard && !refillHit
      val canRefill = !refill.full && !refillLineBusy && !load.ctrl.startRefill && reservation.win && !(refillWayNeedWriteback && writeback.full)
      val askRefill = MISS && canRefill && !refillHit
      val startRefill = isValid && GENERATION_OK && askRefill

      REFILL_SLOT_FULL := MISS && !refillHit && refill.full
      REFILL_SLOT := refillHits.andMask(!refillLoaded) | refill.free.andMask(askRefill)

      val writeCache = isValid && GENERATION_OK && !REDO// && !refillOverlap
      val wayId = OHToUInt(WAYS_HITS)
      val bankHitId = if(!reducedBankWidth) wayId else (wayId >> log2Up(bankCount/memToBankRatio)) @@ ((wayId + (ADDRESS_PRE_TRANSLATION(log2Up(bankWidth/8), log2Up(bankCount) bits))).resize(log2Up(bankCount/memToBankRatio)))

      when(startRefill || writeCache){
        reservation.takeIt()
        status.write.valid := True
        status.write.address := ADDRESS_PRE_TRANSLATION(lineRange)
        status.write.data := STATUS
      }

      when(startRefill){
        refill.push.valid := True
        refill.push.address := ADDRESS_POST_TRANSLATION
        refill.push.way := refillWay

        waysWrite.mask(refillWay) := True
        waysWrite.address := ADDRESS_PRE_TRANSLATION(lineRange)
        waysWrite.tag.loaded := True
        waysWrite.tag.address := ADDRESS_PRE_TRANSLATION(tagRange)

        writeback.push.valid := refillWayNeedWriteback
        writeback.push.address := (WAYS_TAGS(refillWay).address @@ ADDRESS_PRE_TRANSLATION(lineRange)) << lineRange.low
        writeback.push.way := refillWay
      }

      when(writeCache){
        for((bank, bankId) <- banks.zipWithIndex){
          bank.write.valid := bankId === bankHitId
          bank.write.address := ADDRESS_POST_TRANSLATION(lineRange.high downto log2Up(bankWidth / 8))
          bank.write.data.subdivideIn(cpuWordWidth bits).foreach(_ := CPU_WORD)
          bank.write.mask := 0
          bank.write.mask.subdivideIn(cpuWordWidth/8 bits).write(ADDRESS_POST_TRANSLATION(bankWordToCpuWordRange), CPU_MASK)
        }
        status.write.data(refillWay).dirty := True
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