package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.AddressTranslationPortUsage.LOAD_STORE
import naxriscv.interfaces.{AddressTranslationPortUsage, AddressTranslationRsp, AddressTranslationService}
import naxriscv.lsu.DataCachePlugin
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.pipeline.{Stage, Stageable}

import scala.collection.mutable.ArrayBuffer

case class MmuPortLevel(id : Int, ways : Int, depth : Int){
  assert(isPow2(depth))
}
case class MmuPluginParameter(rspAt : Int,
                              levels : Seq[MmuPortLevel],
                              priority : Int,
                              readAt : Int,
                              hitsAt : Int,
                              ctrlAt : Int)


case class MmuSpec(levels : Seq[MmuLevel], entryBytes : Int, preWidth : Int, postWidth : Int, satpMode : Int)
case class MmuLevel(addressOffset : Int, entryOffset : Int, width : Int){
  def vpn(address : UInt) : UInt = address(addressOffset, width bits)
  def ppn(address : Bits) : UInt = address(entryOffset, width bits).asUInt
}

class MmuPluginPlugin(spec : MmuSpec, ioRange : UInt => Bool) extends Plugin with AddressTranslationService{
  override def preWidth  = spec.preWidth
  override def postWidth = spec.postWidth
  override def withTranslation = true


  Global.PC_WIDTH.set(preWidth)
  Global.PC_TRANSLATED_WIDTH.set(postWidth)

  case class PortSpec(stages: Seq[Stage], preAddress: Stageable[UInt], usage : AddressTranslationPortUsage, p: MmuPluginParameter, rsp : AddressTranslationRsp){
    val readStage = stages(p.readAt)
    val hitsStage = stages(p.hitsAt)
    val ctrlStage = stages(p.ctrlAt)
  }
  val portSpecs = ArrayBuffer[PortSpec]()

  override def newTranslationPort(stages: Seq[Stage], preAddress: Stageable[UInt], usage : AddressTranslationPortUsage, pAny: Any) = {
    val p = pAny.asInstanceOf[MmuPluginParameter]
    portSpecs.addRet(new PortSpec(stages, preAddress, usage, p, new AddressTranslationRsp(this, 1, stages(p.rspAt)))).rsp
  }


  override def wakerCount = 1
  override def wakes = ???

  val setup = create early new Area{
    val cache = getService[DataCachePlugin]
    val cacheLoad = cache.newLoadPort(priority = 1)
  }

  val logic = create late new Area{
    lock.await()

    val priv = getService[PrivilegedPlugin]

    case class StorageEntry(levelId : Int, depth : Int) extends Bundle {
      val w = spec.levels.drop(levelId).map(_.width).sum
      val valid, exception = Bool()
      val virtualAddress  = UInt(w-log2Up(depth) bits)
      val physicalAddress = UInt(w bits)
      val allowRead, allowWrite, allowExecute, allowUser = Bool

      def hit(address : UInt) = valid && virtualAddress === address(spec.levels(levelId).addressOffset + log2Up(depth), w - log2Up(depth) bits)
      def physicalAddressFrom(address : UInt) = physicalAddress @@ address(0, spec.levels(levelId).addressOffset bits)
    }

    val csr = new Area {
      val satp = new Area {
        val (modeWidth, ppnWidth) = Global.XLEN.get match {
          case 32 => (1, 20) //20 instead of 22 to avoid 34 physical bits
          case 64 => (4, 44)
        }
        val mode = Reg(Bits(modeWidth bits))
        val ppn = Reg(UInt(ppnWidth bits))
      }
      val status = new Area{
        val mxr = Reg(Bool())
        val sum = Reg(Bool())
        val mprv = Reg(Bool()) //Also, clear it on xret going lower than machine mode
      }
    }


    assert(portSpecs.map(_.p.priority).distinct.size == portSpecs.size, "MMU ports needs different priorities")
    val portSpecsSorted = portSpecs.sortBy(_.p.priority).reverse
    val ports = for(e <- portSpecsSorted) yield new Area{
      val portSpec = e
      import portSpec._

      val storage = for(e <- portSpec.p.levels) yield new Area{
        val level = e
        val specLevel = spec.levels(level.id)
        def newEntry() = StorageEntry(level.id, level.depth)
        val ways = List.fill(level.ways)(Mem.fill(level.depth)(newEntry()))
        val writes = ways.map(_.writePort.setIdle())
        val lineRange = specLevel.addressOffset + log2Up(level.depth) -1 downto specLevel.addressOffset

        val write = new Area{
          val mask    = Bits(level.ways bits)
          val address = UInt(log2Up(level.depth) bits)
          val data    = newEntry()

          mask := 0
          address.assignDontCare()
          data.assignDontCare()

          for((way, sel) <- (ways, mask.asBools).zipped){
            way.write(address, data, sel)
          }
        }
        val allocId = Counter(level.ways)

        val ENTRIES = Stageable(Vec.fill(level.ways)(newEntry()))
        val HITS = Stageable(Bits(level.ways bits))

        val readAddress = readStage(portSpec.preAddress)(lineRange)
        for((way, wayId) <- ways.zipWithIndex) {
          readStage(ENTRIES)(wayId) := way.readAsync(readAddress)
          hitsStage(HITS)(wayId) := hitsStage(ENTRIES)(wayId).hit(hitsStage(portSpec.preAddress))
        }
      }


      val ctrl = new Area{
        import ctrlStage._
        def entriesMux[T <: Data](f : StorageEntry => T) : T = OhMux.or(hits, entries.map(f))

        val hits = Cat(storage.map(s => ctrlStage(s.HITS)))
        val entries = storage.flatMap(s => ctrlStage(s.ENTRIES))
        val hit = hits.orR
        val needRefill = isValid && !hit && requireMmuLockup
        val needRefillAddress = ctrlStage(portSpec.preAddress)
        val lineAllowExecute = entriesMux(_.allowExecute)
        val lineAllowRead    = entriesMux(_.allowRead)
        val lineAllowWrite   = entriesMux(_.allowWrite)
        val lineAllowUser    = entriesMux(_.allowUser)
        val lineException    = entriesMux(_.exception)
        val lineTranslated   = entriesMux(_.physicalAddressFrom(portSpec.preAddress))


        val requireMmuLockup = csr.satp.mode === spec.satpMode
        requireMmuLockup clearWhen(!csr.status.mprv && priv.isMachine())
        when(priv.isMachine()) {
          if (portSpec.usage == LOAD_STORE) {
            requireMmuLockup clearWhen (!csr.status.mprv || priv.logic.machine.mstatus.mpp === 3)
          } else {
            requireMmuLockup := False
          }
        }

        import portSpec.rsp.keys._
        IO := ioRange(TRANSLATED)
        when(requireMmuLockup) {
          REDO          := !hit
          TRANSLATED    := lineTranslated
          ALLOW_EXECUTE := lineAllowExecute
          ALLOW_READ    := lineAllowRead || csr.status.mxr && lineAllowExecute
          ALLOW_WRITE   := lineAllowWrite
          PAGE_FAULT    := hit && (lineException || lineAllowUser && priv.isSupervisor() && !csr.status.sum || !lineAllowUser && priv.isUser())
        } otherwise {
          REDO          := False
          TRANSLATED    := portSpec.preAddress
          ALLOW_EXECUTE := True
          ALLOW_READ    := True
          ALLOW_WRITE   := True
          PAGE_FAULT    := False
        }
      }
    }


    val refill = new StateMachine{
      val IDLE = new State
      val CMD, RSP = List.fill(spec.levels.size)(new State)

      val portOh = Reg(Bits(ports.size bits))
      val virtual = Reg(UInt(preWidth bits))

      val portsRequests = ports.map(_.ctrl.needRefill).asBits()
      val portsRequest  = portsRequests.orR
      val portsOh       = OHMasking.first(portsRequests)
      val portsAddress  = OhMux.or(portsOh, ports.map(_.ctrl.needRefillAddress))

      setEntry(IDLE)

      IDLE whenIsActive {
        when(portsRequest) {
          load.address := csr.satp.ppn @@ spec.levels.last.vpn(portsAddress) @@ U(0, log2Up(spec.entryBytes) bits)
          virtual := portsAddress
          goto(CMD(spec.levels.size - 1))
        }
      }

      val doWake = isActive(IDLE)
      portSpecs.foreach(_.rsp.wake := doWake)

      val load = new Area{
        val address = Reg(UInt(postWidth bits))

        def cmd = setup.cacheLoad.cmd
        val rsp = setup.cacheLoad.rsp.stage()
        val readed = rsp.data.subdivideIn(spec.entryBytes*8 bits).read((address >> log2Up(spec.entryBytes)).resized)

        cmd.virtual           := address
        cmd.size              := U(log2Up(spec.entryBytes))
        cmd.redoOnDataHazard  := True
        cmd.unlocked          := False

        setup.cacheLoad.translated.physical := address
        setup.cacheLoad.translated.abord    := False

        setup.cacheLoad.cancels := 0

        val flags = readed.resized.as(MmuEntryFlags())
        val exception = !flags.V || (!flags.R && flags.W) || rsp.fault
        val leaf = flags.R || flags.X
        val levelToPhysicalAddress = List.fill(spec.levels.size)(UInt(postWidth bits))
        val levelException = List.fill(spec.levels.size)(False)
        val nextLevelBase = U(0, postWidth bits)
        for((level, id) <- spec.levels.zipWithIndex) {
          nextLevelBase(level.addressOffset, level.width bits) := level.ppn(readed)
          levelToPhysicalAddress(id) := 0
          for((e, eId) <- spec.levels.zipWithIndex){
            levelToPhysicalAddress(id)(e.addressOffset, e.width bits) := (if(eId < id) e.vpn(virtual) else e.ppn(readed))
            if(eId < id) levelException(id) setWhen(e.ppn(readed) =/= 0)
          }
        }
      }


      val fetch = for((level, levelId) <- spec.levels.zipWithIndex) yield new Area{
        def doneLogic() : Unit = {
          for(port <- ports){
            import port._
            val storageLevelId = portSpec.p.levels.filter(_.id <= levelId).map(_.id).max
            val storageLevel = storage.find(_.level.id == storageLevelId).get
            val specLevel = storageLevel.specLevel

            storageLevel.write.mask                 := UIntToOh(storageLevel.allocId)
            storageLevel.write.address              := virtual(storageLevel.lineRange)
            storageLevel.write.data.exception       := load.exception || load.levelException(levelId)
            storageLevel.write.data.virtualAddress  := virtual.takeHigh(widthOf(storageLevel.write.data.virtualAddress)).asUInt
            storageLevel.write.data.physicalAddress := load.levelToPhysicalAddress(levelId) >> specLevel.addressOffset
            storageLevel.write.data.allowRead       := load.flags.R
            storageLevel.write.data.allowWrite      := load.flags.W
            storageLevel.write.data.allowExecute    := load.flags.X
            storageLevel.write.data.allowUser       := load.flags.U
            storageLevel.allocId.increment()
          }

          goto(IDLE)
        }

        CMD(levelId) whenIsActive{
          load.cmd.valid := True
          when(load.cmd.ready){
            goto(RSP(levelId))
          }
        }

        RSP(levelId) whenIsActive{
          if(levelId == 0) load.exception setWhen(!load.leaf)
          when(load.rsp.valid){
            when(load.rsp.redo){
              goto(CMD(levelId))
            } otherwise {
              levelId match {
                case 0 => doneLogic
                case _ => {
                  when(load.leaf || load.exception) {
                    doneLogic
                  } otherwise {
                    val targetLevelId = levelId - 1
                    load.address := load.nextLevelBase
                    load.address(log2Up(spec.entryBytes), level.width bits) := spec.levels(targetLevelId).vpn(virtual)
                    goto(CMD(targetLevelId))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}


case class MmuEntryFlags() extends Bundle{
  val V, R, W ,X, U, G, A, D = Bool()
}