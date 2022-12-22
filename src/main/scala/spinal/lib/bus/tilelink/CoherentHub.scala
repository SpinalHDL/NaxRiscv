package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.fsm._
import spinal.lib.pipeline._

case class CoherentHubParameters(nodes : Seq[NodeParameters],
                                 slotCount : Int,
                                 cacheSize: Int,
                                 wayCount: Int,
                                 lineSize : Int,
                                 cBufferCount : Int = 4){
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

 */
class CoherentHub(p : CoherentHubParameters) extends Component{
  import p._

  val slotDownOffset = 0
  val cDownIdOffset = slotDownOffset + slotCount*2
  val downIdCount = cDownIdOffset + p.cBufferCount

  val bps = p.nodes.map(_.toBusParameter())
  val upParam = NodeParameters.mergeMasters(p.nodes)
  val upBusParam = upParam.toBusParameter()
  val downParam = NodeParameters(
    m = MastersParameters(
      List(MasterParameters(
        name = this,
        mapping = List(MasterSource(
          id = SizeMapping(0, downIdCount),
          emits    = MasterTransfers(
            get        = SizeRange(1, p.blockSize),
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
  val downBusParam = downParam.toBusParameter()

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
    val upstreams = Vec(bps.map(e => slave(Bus(e))))
    val downstream = master(Bus(downBusParam))
  }

  val nodeToMasterMaskMapping = p.nodes.map(node => node -> node.m.masters.zipWithIndex.filter(_._1.emits.withBCE).toMapLinked()).toMapLinked()
  val slots = for(i <- 0 until slotCount) yield new Area{
    val id = i
    val fire = False
    val valid = RegInit(False) clearWhen(fire)
    val address = Reg(upBusParam.address) //TODO optimize in mem
    val source  = Reg(upBusParam.source())
    val shared = Reg(Bool())
    val unique = Reg(Bool())
    val tagsReaded = Reg(Bool())

    val lineConflict = new Area{
      val youngest = Reg(Bool()) //TODO set when conflict is resolved
      val valid = Reg(Bool()) //TODO clear
      val slotId = Reg(UInt(log2Up(slotCount) bits))
    }

    val probe = new Area{
      val filtred = Reg(Bool())
      val ports = for((node, id) <- p.nodes.zipWithIndex) yield new Area{
        val pending =  Reg(Bits(nodeToMasterMaskMapping(node).size bits))
        val inflight = Reg(Bits(nodeToMasterMaskMapping(node).size bits))
        val empty = (pending ## inflight) === 0
      }
    }
  }

  val slotsMem = new Area{
    val address = Mem.fill(slotCount)(upBusParam.address)
    val size    = Mem.fill(slotCount)(upBusParam.size)
  }

  def slotAddress(oh : Bits) = slotsMem.address.readAsync(OHToUInt(oh))
  def slotSize(oh : Bits) = slotsMem.size.readAsync(OHToUInt(oh))

  val upstream = new Area{
    val buses = io.upstreams.zipWithIndex.map{case (bus, id) => bus.withSourceOffset(id, upParam.m.sourceWidth)}
    val a = new Area{
      val filtred = buses.map(bus => bus.a.takeWhen(bus.a.isFirst))
      val arbiter = StreamArbiterFactory().noLock.roundRobin.build(ChannelA(upBusParam), p.nodes.size)
      (arbiter.io.inputs, filtred).zipped.foreach(_ << _)
      val toSlot = arbiter.io.output.combStage()
    }
    val c = new Area{
      val toProbeRsp = buses.map(bus => bus.c.toFlow.takeWhen(bus.c.isProbeKind))
      assert(buses.map(bus => !(bus.c.valid && bus.c.opcode === Opcode.C.PROBE_ACK_DATA)).andR, "Need to be implemented")
    }
  }


  val slotSpawn = new Area{
    val push = upstream.a.toSlot.combStage()
    val oh = OHMasking.firstV2(B(slots.map(_.valid)))
    val sel = OHToUInt(oh)
    val full = slots.map(_.valid).andR
    val lineConflicts = B(for(slot <- slots) yield slot.valid && slot.lineConflict.youngest && slot.address(lineRange) === push.address(lineRange))
    val shared = Bool()
    val unique = Bool()

    switch(push.opcode){
      is(Opcode.A.ACQUIRE_BLOCK, Opcode.A.ACQUIRE_PERM){
        shared := push.param === Param.Grow.NtoB
        unique := push.param === Param.Grow.NtoT || push.param === Param.Grow.BtoT
      }
      is(Opcode.A.PUT_FULL_DATA, Opcode.A.PUT_PARTIAL_DATA){
        shared := False
        unique := True
      }
      is(Opcode.A.GET){
        shared := True
        unique := False
      }
    }

    push.ready := !full
    when(push.fire){
      slotsMem.address.write(sel, push.address)
      slotsMem.size.write(sel, push.size)
      slots.onMask(lineConflicts){ s =>
        s.lineConflict.youngest := False
      }
      slots.onMask(oh){ s =>
        s.valid := True
        s.address := push.address
        s.source := push.source
        s.shared := shared
        s.unique := unique
        s.lineConflict.youngest := True
        s.lineConflict.valid := lineConflicts.orR
        s.lineConflict.slotId := OHToUInt(lineConflicts)
        s.tagsReaded := True //TODO support cache
        s.probe.filtred := True //TODO support dictionary
        for((port, node) <- (s.probe.ports, nodes).zipped){
          val mapping = nodeToMasterMaskMapping(node)
          for((mpp , id) <- mapping){
            when(!mpp.mapping.map(_.id.hit(push.source)).orR){
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
      val arbiter = StreamArbiterFactory().transactionLock.roundRobin.build(NoData(), slotCount).io
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
      val stream = arbiter.output.translateWith(ctx)
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
        bus.valid   := requests.orR
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
      val sourceIdHits = p.nodes.flatMap(_.m.masters).map(m => m -> m.mapping.map(_.id.hit(c.source)).orR).toMapLinked()
      val onSlots = for(slot <- slots) yield new Area{
        val hit = slot.valid && slot.lineConflict.youngest && slot.address(lineRange) === c.address
        val ctx = slot.probe.ports(nodeId)
        val notEnough = slot.unique && List(Param.Prune.TtoB, Param.Report.TtoT, Param.Report.BtoB).map(c.param === _).orR

        val update = for((m, id) <- mapping){
          when(sourceIdHits(m)){
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

  val slotToDown = new Area{
    val aRead = Stream(ChannelA(downBusParam))
//    val aWriteThrough = Stream(ChannelA(downBusParam))


    val frontend = new Pipeline{
      val s0 = new Stage {
        val requests = slots.map(s => s.valid && s.tagsReaded && s.probe.filtred && s.probe.ports.map(_.empty).andR)
        val arbiter = StreamArbiterFactory().noLock.roundRobin.build(NoData(), slotCount).io
        (arbiter.inputs, requests).zipped.foreach(_.valid := _)
        driveFrom(arbiter.output)
        SLOT_ID := OHToUInt(arbiter.chosenOH)
        ADDRESS := slotAddress(arbiter.chosenOH)
        SIZE := slotSize(arbiter.chosenOH)
      }
      val s1 = new Stage(Connection.DIRECT())
    }

    val readMem = new Pipeline{
      val s0 = new Stage{
        driveFrom(frontend.s1, True, List(SLOT_ID, ADDRESS, SIZE))
      }
      val s1 = new Stage(Connection.M2S()){
        haltIt(aRead.isStall)
        aRead.valid   := isValid
        aRead.opcode  := Opcode.A.GET
        aRead.param   := 0
        aRead.source  := SLOT_ID.resized
        aRead.size    := SIZE
        aRead.address := ADDRESS
        aRead.mask    .assignDontCare()
        aRead.data    .assignDontCare()
        aRead.corrupt .assignDontCare()
      }
      build()
    }

    frontend.build()
  }

  val downA = new Area{
    val stream = StreamArbiterFactory().lowerFirst.lambdaLock[ChannelA](!_.isLast()).on(List(slotToDown.aRead))
    io.downstream.a << stream
  }

  //TODO remove
  upstream.buses.foreach{bus =>
    bus.d.setIdle()
    bus.e.ready := True
  }
  io.downstream.d.ready := False
}





object CoherentHubGen extends App{
  SpinalVerilog(new CoherentHub(
    CoherentHubParameters(
      nodes     = List.fill(2)(
        NodeParameters(
          m = MastersParameters(List(
            MasterParameters(
              name = null,
              mapping = List(MasterSource(
                emits = MasterTransfers(
                  acquireT = SizeRange(64),
                  acquireB = SizeRange(64),
                  probeAckData = SizeRange(64)
                ),
                id = SizeMapping(0, 4),
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
  ))
}
/*
TODO warning :
- probe_data need to be properly handled
 */