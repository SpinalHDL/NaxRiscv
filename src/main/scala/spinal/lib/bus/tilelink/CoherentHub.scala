package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.fsm._
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

case class CoherentHubParameters(nodes : Seq[NodeParameters],
                                 slotCount : Int,
                                 cacheSize: Int,
                                 wayCount: Int,
                                 lineSize : Int,
                                 cBufferCount : Int = 4,
                                 aBufferCount : Int = 4){
  val addressWidth = nodes.map(_.m.addressWidth).max
  val waySize = cacheSize/wayCount
  val linePerWay = waySize/lineSize
  val tagRange = addressWidth-1 downto log2Up(linePerWay*lineSize)
  val lineRange = tagRange.low-1 downto log2Up(lineSize)
  val refillRange = tagRange.high downto lineRange.low
  val blockSize = lineSize
}

/*
access main memory :
- read without ownership
- writes without ownership
- refill
- writeback
- release miss

Read cache :
- read without ownership
- read allocate
- writeback

write cache
- writes without ownership
- refill
- release hit

drive upstream D
- cache read
- upsteam C ?
- downstream D
 */

case class CoherentHubOrdering(p : CoherentHubParameters) extends Bundle{
  val upId = UInt(log2Up(p.nodes.size) bits)
  val upSource = UInt(p.nodes.map(_.m.sourceWidth).max bits)
}

class CoherentHub(p : CoherentHubParameters) extends Component{
  import p._

  val slotDownOffset = 0
  val cDownIdOffset = slotDownOffset + slotCount*2
  val downIdCount = cDownIdOffset + p.cBufferCount

  val bps = p.nodes.map(_.toBusParameter())
  val upParam = NodeParameters.mergeMasters(p.nodes)
  val upBusParam = upParam.toBusParameter()
  val downPutParam = NodeParameters(
    m = MastersParameters(
      List(MasterParameters(
        name = this,
        mapping = List(MasterSource(
          id = SizeMapping(0, downIdCount),
          emits    = MasterTransfers(
            putFull    = SizeRange(p.blockSize),
            putPartial = SizeRange(1, p.blockSize)
          ),
          addressRange = upParam.m.masters.flatMap(_.mapping.flatMap(_.addressRange)).distinctLinked.toList
        ))
      ))
    ),
    s = SlavesParameters(slaves = Nil),
    dataBytes = upParam.dataBytes
  )
  val downGetParam = NodeParameters(
    m = MastersParameters(
      List(MasterParameters(
        name = this,
        mapping = List(MasterSource(
          id = SizeMapping(0, downIdCount),
          emits    = MasterTransfers(
            get        = SizeRange(1, p.blockSize)
          ),
          addressRange = upParam.m.masters.flatMap(_.mapping.flatMap(_.addressRange)).distinctLinked.toList
        ))
      ))
    ),
    s = SlavesParameters(slaves = Nil),
    dataBytes = upParam.dataBytes
  )
  val downPutBusParam = downPutParam.toBusParameter()
  val downGetBusParam = downGetParam.toBusParameter()

//  val writebackParam = NodeParameters(
//    m = MastersParameters(
//      List(MasterParameters(
//        name = this,
//        emits = MasterTransfers(
//          //          get        = TransferSupport(1, p.blockSize)
//          putFull    = TransferSupport(p.blockSize)
//          //          putPartial = TransferSupport(1, p.blockSize)
//        ),
//        sourceId = SizeMapping(0, slotCount),
//        addressRange =
//      ))
//    ),
//    s = SlavesParameters(
//
//    ),
//    dataBytes = upParam.dataBytes
//  )

  val io = new Bundle{
    val ups = Vec(bps.map(e => slave(Bus(e))))
    val downPut = master(Bus(downPutBusParam))
    val downGet = master(Bus(downGetBusParam))

    val ordering = new Bundle{
      val toDownGet = master Flow(CoherentHubOrdering(p))
      val toDownPut = master Flow(CoherentHubOrdering(p))
      def all = List(toDownGet, toDownPut)
    }
  }

//  val withDataA = p.nodes.exists(_.m.withDataA)
//  val upDataBuffers = new Area{
//    val ram = Mem.fill(upDataBufferCount*p.blockSize/upParam.dataBytes)(upBusParam.data)
//    val allocated = Reg(Bits(upDataBufferCount bits)) init(0)
//    val filled = Reg(Bits(upDataBufferCount bits))
//    val askC = False
//    val allowA = CountOne(~allocated) > upDataBufferCountOnlyC && !askC
//    val allowC = ~allocated =/= 0
//    val freeId = OHToUInt(OHMasking.firstV2(~allocated))
//    val busy = RegInit(False)
//    val nodeId = Reg(UInt(log2Up(p.nodes.size) bits))
//    val beats = Reg(UInt(upBusParam.beatWidth bits))
//    val counter = Reg(UInt(upBusParam.beatWidth bits))
//  }

  val nodeToMasterMaskMapping = p.nodes.map(node => node -> node.m.masters.zipWithIndex.filter(_._1.emits.withBCE).toMapLinked()).toMapLinked()
  val slots = for(i <- 0 until slotCount) yield new Area{
    val id = i
    val fire = False
    val valid = RegInit(False) clearWhen(fire)
    val address = Reg(upBusParam.address) //TODO optimize in mem
//    val source  = Reg(upBusParam.source())
    val shared = Reg(Bool())
    val unique = Reg(Bool())
    val tagsReaded = Reg(Bool())
    val readSent, writeSent = Reg(Bool())
    val readMem, writeMem = Reg(Bool())

    val lineConflict = new Area{
      val youngest = Reg(Bool()) //TODO set when conflict is resolved
      val valid = Reg(Bool()) //TODO clear
      val slotId = Reg(UInt(log2Up(slotCount) bits))
    }

    val probe = new Area{
      val filtred = Reg(Bool())
      val ports = for((node, id) <- p.nodes.zipWithIndex) yield new Area{
        val pending =  Reg(Bits(nodeToMasterMaskMapping(node).size bits)) init(0)
        val inflight = Reg(Bits(nodeToMasterMaskMapping(node).size bits)) init(0)
        val empty = (pending ## inflight) === 0
      }
    }

    def spawn(): Unit ={
      readSent := False
      writeSent := False
    }
  }

  val slotsMem = new Area{
    val address = Mem.fill(slotCount)(upBusParam.address)
    val size    = Mem.fill(slotCount)(upBusParam.size)
    val source  = Mem.fill(slotCount)(upBusParam.source)
  }

  def slotAddress(oh : Bits) = slotsMem.address.readAsync(OHToUInt(oh))
  def slotSize(oh : Bits) = slotsMem.size.readAsync(OHToUInt(oh))
  def slotSource(oh : Bits) = slotsMem.source.readAsync(OHToUInt(oh))

  val withDataA = p.nodes.exists(_.m.withDataA)
  val upstream = new Area{
    val buses = io.ups.zipWithIndex.map{case (bus, id) => bus.withSourceOffset(id, upParam.m.sourceWidth)}
    val a = new Area{

      val withoutData = buses.filter(!_.p.withDataA).map(_.a).toList
      val withData = withDataA generate new Area{
        val buffer =  new Area{
          case class Payload() extends Bundle {
            val mask = upBusParam.mask()
            val data = upBusParam.data()
          }
          val ram = Mem.fill(aBufferCount*p.blockSize/upParam.dataBytes)(Payload())
          val allocated = Reg(Bits(aBufferCount bits)) init(0)
          val set, clear = B(0, aBufferCount bits)
          val firstFree = OHMasking.firstV2(~allocated)
          val full = allocated.andR
          val source = Vec.fill(aBufferCount)(Reg(upBusParam.source))
          val write = ram.writePort()
          allocated := (allocated | set) & ~clear
        }

        val filtred = buses.filter(_.p.withDataA).map(_.a)
        val merged = StreamArbiterFactory().roundRobin.lambdaLock[ChannelA](_.isLast()).on(filtred)
        val withBeats = merged.withBeats
        val halted = merged.haltWhen(withBeats && buffer.full)
        buffer.write.valid := withBeats && halted.valid
        buffer.write.address := (halted.address >> log2Up(halted.p.dataBytes)).resized
        buffer.write.data.mask := halted.mask
        buffer.write.data.data := halted.data
        when(halted.fire && halted.isLast() && withBeats){
          buffer.set := buffer.firstFree
          buffer.source.onMask(buffer.firstFree)(_ := halted.source)
        }
        val toSlot = halted.takeWhen(halted.isLast())
        for(i <- 0 until upBusParam.sizeMax) toSlot.address(i) clearWhen(toSlot.size > i)
      }

      val aList = if(withDataA) withData.toSlot :: withoutData else withoutData
      val toSlot = StreamArbiterFactory().noLock.roundRobin.on(aList)
    }
    val c = new Area{
      val toProbeRsp = buses.map(bus => bus.c.toFlow.takeWhen(bus.c.isProbeKind))
      assert(buses.map(bus => !(bus.c.valid && bus.c.opcode === Opcode.C.PROBE_ACK_DATA)).andR, "Need to be implemented")
    }
  }


  val slotSpawn = new Area{
    val push = upstream.a.toSlot.combStage()
    val oh = OHMasking.firstV2(~B(slots.map(_.valid)))
    val sel = OHToUInt(oh)
    val full = slots.map(_.valid).andR
    val lineConflicts = B(for(slot <- slots) yield slot.valid && slot.lineConflict.youngest && slot.address(lineRange) === push.address(lineRange))
    val shared,  unique = Bool()
    val put, get = False

    switch(push.opcode){
      is(Opcode.A.ACQUIRE_BLOCK, Opcode.A.ACQUIRE_PERM){
        shared := push.param === Param.Grow.NtoB
        unique := push.param === Param.Grow.NtoT || push.param === Param.Grow.BtoT
      }
      is(Opcode.A.PUT_FULL_DATA, Opcode.A.PUT_PARTIAL_DATA){
        shared := False
        unique := True
        put := True
      }
      is(Opcode.A.GET){
        shared := True
        unique := False
        get := True
      }
    }

    push.ready := !full
    when(push.fire){
      slotsMem.address.write(sel, push.address)
      slotsMem.size.write(sel, push.size)
      slotsMem.source.write(sel, push.source)
      slots.onMask(lineConflicts){ s =>
        s.lineConflict.youngest := False
      }
      slots.onMask(oh){ s =>
        s.valid := True
        s.spawn()
        s.address := push.address
//        s.source := push.source
        s.shared := shared
        s.unique := unique
        s.writeMem := put
        s.readMem := get
        s.lineConflict.youngest := True
        s.lineConflict.valid := lineConflicts.orR
        s.lineConflict.slotId := OHToUInt(lineConflicts)
        s.tagsReaded := True //TODO support cache
        s.probe.filtred := True //TODO support dictionary
        for((port, node) <- (s.probe.ports, nodes).zipped){
          val mapping = nodeToMasterMaskMapping(node)
          for((mpp , id) <- mapping){
            when(!mpp.sourceHit(push.source)){
              port.pending(id) := True
            }
          }
        }
      }
    }
  }

  val probe = new Area{
    val sel = new Area{
      val pendings = Vec(slots.map(s => s.probe.ports.map(_.pending.orR).orR))
      val arbiter = StreamArbiterFactory().roundRobin.build(NoData(), slotCount).io
      Vec(arbiter.inputs.map(_.valid)) := pendings
      when(arbiter.output.fire){
        slots.onMask(arbiter.chosenOH){slot =>
          slot.probe.ports.foreach{p =>
            p.pending := 0
            p.inflight := p.pending
          }
        }
      }
      case class ProbeContext() extends Bundle{
        val slotOh  = Bits(slotCount bits)
        val address = upBusParam.address()
        val param   = Bits(3 bits)
        val mask    = Vec(nodeToMasterMaskMapping.values.map(e => Bits(e.size bits)))
      }
      val ctx = ProbeContext()
      ctx.slotOh := arbiter.chosenOH
      ctx.address := slotAddress(arbiter.chosenOH)
      ctx.param   := (OhMux(arbiter.chosenOH, slots.map(_.unique)) ? B(Param.Cap.toN) | B(Param.Cap.toB)).resized
      for(nodeId <- 0 until nodes.size) {
        ctx.mask(nodeId) := OhMux(arbiter.chosenOH, slots.map(_.probe.ports(nodeId).pending))
      }
      val stream = arbiter.output.translateWith(ctx).s2mPipe() //S2m pipe ensure that the pending / inflight status are updated
    }

    val cmd = new Area{
      val input = sel.stream.combStage()
      val buses = for ((node, nodeId) <- p.nodes.zipWithIndex.filter(_._1.withBCE)) yield new Area {
        val bus = upstream.buses(nodeId).b
        val mapping = nodeToMasterMaskMapping(node)
        val fired = Reg(Bits(mapping.size bits)) init(0)
        val requests = input.mask(nodeId) & ~fired
        val masterOh = OHMasking.firstV2(requests)
        val ready = requests === 0
        bus.valid   := input.valid && requests.orR
        bus.opcode  := Opcode.B.PROBE_BLOCK
        bus.param   := input.param
        bus.source  := OhMux(masterOh, mapping.keys.map(m => U(m.bSourceId, upBusParam.sourceWidth bits)).toList)
        bus.address := input.address
        bus.size    := log2Up(p.blockSize)
        when(bus.fire){
          fired.asBools.onMask(masterOh)(_ := True)
        }
        when(input.fire){
          fired := 0
        }
      }
      input.ready := buses.map(_.ready).andR
    }

    val rsps = for((node, nodeId) <- p.nodes.zipWithIndex.filter(_._1.withBCE)) yield new Area{
      val c = upstream.c.toProbeRsp(nodeId)
      val mapping = nodeToMasterMaskMapping(node)
      val sourceIdHits = p.nodes.flatMap(_.m.masters).map(m => m -> m.sourceHit(c.source)).toMapLinked()
      val onSlots = for(slot <- slots) yield new Area{
        val hit = slot.valid && slot.lineConflict.youngest && slot.address(lineRange) === c.address(lineRange)
        val ctx = slot.probe.ports(nodeId)
        val notEnough = slot.unique && List(Param.Prune.TtoB, Param.Report.TtoT, Param.Report.BtoB).map(c.param === _).orR

        val update = for((m, id) <- mapping){
          when(c.fire && sourceIdHits(m)){
            ctx.inflight(id) := False
            ctx.pending(id) setWhen(notEnough) //Redo the probe
          }
        }
      }
    }
  }


  val ADDRESS = Stageable(upBusParam.address)
  val SIZE = Stageable(upBusParam.size)
  val SLOT_ID = Stageable(UInt(log2Up(slotCount) bits))
  val READ = Stageable(Bool())
  val WRITE = Stageable(Bool())
  val SOURCE = Stageable(upBusParam.source)

  val readPip = new Area{
    val aRead = Stream(ChannelA(downGetBusParam))
    //    val aWriteThrough = Stream(ChannelA(downBusParam))


    val frontend = new Pipeline{
      val s0 = new Stage {
        val requests = slots.map(s => s.valid && s.tagsReaded && s.probe.filtred && s.probe.ports.map(_.empty).andR && s.readMem && !s.readSent)
        val arbiter = StreamArbiterFactory().noLock.roundRobin.build(NoData(), slotCount).io
        (arbiter.inputs, requests).zipped.foreach(_.valid := _)
        driveFrom(arbiter.output)
        SLOT_ID := OHToUInt(arbiter.chosenOH)
        ADDRESS := slotAddress(arbiter.chosenOH)
        SIZE := slotSize(arbiter.chosenOH)

        slots.onMask(arbiter.chosenOH){s =>
          s.readSent := True
        }

        val ordering = io.ordering.toDownGet
        val source = slotSource(arbiter.chosenOH)
        ordering.valid := isFireing
        (ordering.upId,  ordering.upSource) := source
      }
      val s1 = new Stage(Connection.DIRECT())
    }

    val stageables = List(SLOT_ID, ADDRESS, SIZE)
    val readMem = new Pipeline{
      val s0 = new Stage{
        driveFrom(frontend.s1, True, stageables)
      }
      val s1 = new Stage(Connection.M2S()){
        haltIt(aRead.isStall)
        aRead.valid   := isValid
        aRead.opcode  := Opcode.A.GET
        aRead.param   := 0
        aRead.source  := SLOT_ID.resized
        aRead.size    := SIZE
        aRead.address := ADDRESS
      }
      build()
    }

    frontend.build()
  }

  val writePip = new Area{
    val aWrite = Stream(ChannelA(downPutBusParam))

    val frontend = new Pipeline{
      val s0 = new Stage {
        val requests = slots.map(s => s.valid && s.tagsReaded && s.probe.filtred && s.probe.ports.map(_.empty).andR && s.writeMem && !s.writeSent)
        val arbiter = StreamArbiterFactory().noLock.roundRobin.build(NoData(), slotCount).io
        (arbiter.inputs, requests).zipped.foreach(_.valid := _)
        driveFrom(arbiter.output)
        SLOT_ID := OHToUInt(arbiter.chosenOH)
        ADDRESS := slotAddress(arbiter.chosenOH)
        SIZE := slotSize(arbiter.chosenOH)
        SOURCE := slotSource(arbiter.chosenOH)

        slots.onMask(arbiter.chosenOH){s =>
          s.writeSent := True
        }

        val ordering = io.ordering.toDownPut
        ordering.valid := isFireing
        (ordering.upId,  ordering.upSource) := this(SOURCE)
      }

      val s1 = new Stage(Connection.M2S()){
        val upBufferRead = upstream.a.withData.buffer.ram.readSyncPort()
        val upBufferHits = upstream.a.withData.buffer.source.map(_ === SOURCE).asBits & upstream.a.withData.buffer.allocated
        val upBufferId = OHToUInt(upBufferHits)
        val counter = Reg(downPutBusParam.beat) init(0)
        val beatsMinusOne = sizeToBeatMinusOne(downGetBusParam, SIZE)
        val isLast = counter === beatsMinusOne
        val BEAT_ADDRESS = insert(ADDRESS | (counter << log2Up(downPutParam.dataBytes)).resized)

        forkIt(!isLast)
        upBufferRead.cmd.valid := False
        upBufferRead.cmd.payload := upBufferId @@ (BEAT_ADDRESS.resize(log2Up(p.blockSize)) >> log2Up(downPutParam.dataBytes))
        when(isForked){
          upBufferRead.cmd.valid := True
          counter := counter + 1
          when(isLast){
            counter := 0
            upstream.a.withData.buffer.clear := upBufferHits
          }
        }
      }

      val s2 = new Stage(Connection.M2S()){
        val BUFFER = insert(s1.upBufferRead.rsp)
      }

      val s3 = new Stage(Connection.M2S()){
        haltIt(aWrite.isStall)
        aWrite.valid   := isValid
        aWrite.opcode  := Opcode.A.PUT_PARTIAL_DATA
        aWrite.param   := 0
        aWrite.source  := SLOT_ID.resized
        aWrite.size    := SIZE
        aWrite.address := s1.BEAT_ADDRESS
        aWrite.mask    := s2.BUFFER.mask
        aWrite.data    := s2.BUFFER.data
        aWrite.corrupt := False
      }
    }

    frontend.build()
  }

  case class DispatchD(d : Stream[ChannelD], oh : Bits)
  val dispatchD = ArrayBuffer[DispatchD]()

  val downToUp = new Area{
    val get = new Area {
      val downD = io.downGet.d
      val isLast = downD.isLast()
      val slotId = downD.source.resize(log2Up(slotCount))
      val upSource = slotsMem.source.readAsync(slotId)
      val upHits = B(p.nodes.map(_.m.sourceHit(upSource)))
      val upD = Stream(ChannelD(upBusParam))
      upD << downD
      upD.source.removeAssignments() := upSource
      upD.sink.removeAssignments() := 0 //TODO
      dispatchD += DispatchD(upD, upHits)
      when(downD.fire && isLast) {
        slots.onSel(slotId) { s =>
          s.valid := False
        }
      }
    }
    val put = new Area{
      val downD = io.downPut.d
      val slotId = downD.source.resize(log2Up(slotCount))
      val upSource = slotsMem.source.readAsync(slotId)
      val upHits = B(p.nodes.map(_.m.sourceHit(upSource)))
      val upD = Stream(ChannelD(upBusParam))
      upD << downD.translateWith(downD.withDontCareData())
      upD.source.removeAssignments() := upSource
      upD.sink.removeAssignments() := 0 //TODO
      dispatchD += DispatchD(upD, upHits)
      when(downD.fire) {
        slots.onSel(slotId) { s =>
          s.valid := False
        }
      }
    }
  }

  val mergeDownA = new Area{
    io.downGet.a << StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](_.isLast()).on(List(readPip.aRead))
    io.downPut.a << StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](_.isLast()).on(List(writePip.aWrite))
  }

  val dispatchUpD = new Area{
//    val groups = (0 until p.nodes.size).map(List(_)) //Full connection for now
    val groups = List((0 until p.nodes.size)) //Single node for now

    val demuxes = for(d <- dispatchD) yield StreamDemuxOh(d.d, groups.map(_.map(d.oh.apply).orR))
    val logics = for((group, groupId) <- groups.zipWithIndex) yield new Area{
      val inputs = demuxes.map(_(groupId))
      val arbiter = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelD](_.isLast()).build(ChannelD(upBusParam), inputs.size).io
      (arbiter.inputs, inputs).zipped.map(_ << _)
      val nodeOh = (dispatchD, arbiter.chosenOH.asBools).zipped.map(_.oh.andMask(_)).reduceBalancedTree(_ | _)
      val dispatch = StreamDemuxOh(arbiter.output, group.map(nodeOh.apply))
      (group.map(upstream.buses.apply), dispatch).zipped.foreach(_.d << _)
    }
  }

  //TODO remove
  upstream.buses.foreach{bus =>
    bus.e.ready := True
  }
}





object CoherentHubGen extends App{
  def basicConfig = CoherentHubParameters(
    nodes     = List.fill(1)(
      NodeParameters(
        m = MastersParameters(List.tabulate(4)(mId =>
          MasterParameters(
            name = null,
            mapping = List.fill(1)(MasterSource(
              emits = MasterTransfers(
                get = SizeRange(64),
                putFull = SizeRange(64),
                putPartial = SizeRange(64),
                acquireT = SizeRange(64),
                acquireB = SizeRange(64),
                probeAckData = SizeRange(64)
              ),
              id = SizeMapping(mId*4, 4),
              addressRange = List(SizeMapping(0, 1 << 16))
            ))
          ))
        ),
        s = SlavesParameters(List(
          SlaveParameters(
            name = null,
            emits = SlaveTransfers(
              probe = SizeRange(64)
            ),
            sinkId = SizeMapping(0, 8)
          )
        )),
        dataBytes = 4
      )
    ),
    slotCount = 2,
    cacheSize = 1024,
    wayCount  = 2,
    lineSize  = 64
  )
  def gen = new CoherentHub(basicConfig)
  SpinalVerilog(gen)

}
/*
TODO warning :
- probe_data need to be properly handled
- If C to D data bypass is implemented, then the slot should be hold until dirty data is cleanedup. Also Future C on matching address should be holded until data is cleanedup !
 */