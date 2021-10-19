package vooxriscv.frontend
import spinal.core._
import spinal.lib._
import vooxriscv.Global
import vooxriscv.frontend.Frontend._
import vooxriscv.pipeline.Stageable
import vooxriscv.utilities._

case class FetchL1Cmd(p : FetchL1Plugin) extends Bundle{
  val address = UInt(Global.PHYSICAL_WIDTH bits)
}

case class FetchL1Rsp(p : FetchL1Plugin) extends Bundle{
  val data = Bits(p.memDataWidth bit)
  val error = Bool()
}

case class FetchL1Bus(p : FetchL1Plugin) extends Bundle with IMasterSlave {
  val cmd = Stream(FetchL1Cmd(p))
  val rsp = Stream(FetchL1Rsp(p))

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}

class FetchL1Plugin(val cacheSize : Int,
                    val wayCount : Int,
                    val lineSize : Int = 64,
                    val readAt : Int = 0,
                    val hitsAt : Int = 1,
                    val hitAt : Int = 1,
                    val bankMuxesAt : Int = 1,
                    val bankMuxAt : Int = 2,
                    val waySelectAt : Int = 2,
                    val injectionAt : Int = 2,
                    val memDataWidth   : Int = 64) extends Plugin with FetchPipelineRequirements {
  override def stagesCountMin = injectionAt + 1

  val mem = create early master(FetchL1Bus(this))

  val setup = create early new Area{
    val pipeline = getService(classOf[FrontendPlugin])
    pipeline.lock.retain()

    val pcPlugin = getService(classOf[PcPlugin])
//    val redoJump = pcPlugin.createJumpInterface()

    mem.flatten.filter(_.isOutput).foreach(_.assignDontCare())
  }

  val logic = create late new Area{
    val cpuWordWidth = FETCH_DATA_WIDTH.get
    val bytePerMemWord = memDataWidth/8
    val bytePerFetchWord = memDataWidth/8
    val waySize = cacheSize/wayCount
    val linePerWay = waySize/lineSize
    val memDataPerWay = waySize/bytePerMemWord
    val memData = HardType(Bits(memDataWidth bits))
    val tagWidth = Global.PHYSICAL_WIDTH-log2Up(waySize)


    val tagRange = Global.PHYSICAL_WIDTH-1 downto log2Up(linePerWay*lineSize)
    val lineRange = tagRange.low-1 downto log2Up(lineSize)

    val bankCount = wayCount
    val bankWidth = Math.max(cpuWordWidth, memDataWidth/wayCount)
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
    val waySelectStage = setup.pipeline.getStage(waySelectAt)
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
    val WAYS_ERROR = Stageable(Bool())

    val BANKS_MUXES = Stageable(Vec.fill(bankCount)(Bits(cpuWordWidth bits)))
    val BANKS_MUX = Stageable(Bits(cpuWordWidth bits))



    val banks = for(id <- 0 until bankCount) yield new Area{
      val mem = Mem(Bits(bankWidth bits), bankWordCount)
      val write = mem.writePort
      val read = new Area{
        val cmd = Flow(mem.addressType)
        val rsp = mem.readSync(cmd.payload, cmd.valid)
        setup.pipeline.getStage(readAt+1)(BANKS_WORDS)(id) := rsp
      }
    }
    val ways = for(id <- 0 until wayCount) yield new Area {
      val mem = Mem.fill(linePerWay)(Tag())
      val write = mem.writePort
//      write.valid := False
//      write.payload.assignDontCare()
      val read = new Area{
        val cmd = Flow(mem.addressType)
        val rsp = mem.readSync(cmd.payload, cmd.valid)
        setup.pipeline.getStage(readAt+1)(WAYS_TAGS)(id) := rsp
      }
    }

//    val read = new Area{
//      val cmd = Flow(UInt(log2Up(memDataPerWay) bits))
//      val data = Vec.fill(wayCount)(memData())
//      val tags = Vec.fill(wayCount)(Tag())
//    }
//    val ways = for(wayId <- 0 until wayCount) yield new Area{
//      val data = Mem.fill(memDataPerWay)(memData)
//      val tags = Mem.fill(linePerWay)(Tag())
//      read.data(wayId) := data.readSync()
//    }

    val flush = new Area{
      val counter = Reg(UInt(log2Up(linePerWay)+1 bits)) init(0)
      val done = counter.msb
      when(!done){
        counter := counter + 1
      }
      ways.foreach{ tag =>
        tag.write.valid := !done
        tag.write.address := counter.resized
        tag.write.data.loaded := False
      }
      readStage.haltIt(!done)
    }


    val fetch = new Area{
      val data = new Area{
        for((bank, bankId) <- banks.zipWithIndex) yield new Area{
          {
            import readStage._
            bank.read.cmd.valid := !isStuck
            bank.read.cmd.payload := FETCH_PC_VIRTUAL(lineRange.high downto log2Up(bankWidth / 8))
          }
          {import bankMuxesStage._; BANKS_MUXES(bankId) := BANKS_WORDS(bankId).subdivideIn(cpuWordWidth bits).read(FETCH_PC_VIRTUAL(bankWordToCpuWordRange)) }
        }
        {import bankMuxStage._;   BANKS_MUX := MuxOH(WAYS_HITS, BANKS_MUXES) }


      }

      val ctrl = new Area{
        for((way, wayId) <- ways.zipWithIndex) yield new Area{
          {
            import readStage._
            way.read.cmd.valid := !isStuck
            way.read.cmd.payload := FETCH_PC_VIRTUAL(lineRange)
          }

          {import hitsStage._ ; WAYS_HITS(wayId) := WAYS_TAGS(wayId).loaded && WAYS_TAGS(wayId).address === FETCH_PC_PHYSICAL(tagRange) }
        }

        {import hitStage._;   WAYS_HIT := B(WAYS_HITS).orR}
      }
    }


    val injectStage = setup.pipeline.getStage(stagesCountMin-1)
    injectStage(WORD) := 0x1111
    injectStage(MASK) := 0x1


    setup.pipeline.lock.release()
  }
}
