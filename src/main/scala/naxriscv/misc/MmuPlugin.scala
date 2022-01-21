package naxriscv.misc

import naxriscv._
import naxriscv.interfaces.{AddressTranslationRsp, AddressTranslationService}
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
case class MmuPluginParameter(rspAt : Int, levels : Seq[MmuPortLevel])


case class MmuSpec(levels : Seq[MmuLevel], entryBytes : Int, preWidth : Int, postWidth : Int)
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

  case class Spec(stages: Seq[Stage], preAddress: Stageable[UInt], p: MmuPluginParameter, rsp : AddressTranslationRsp)
  val specs = ArrayBuffer[Spec]()

  override def newTranslationPort(stages: Seq[Stage], preAddress: Stageable[UInt], pAny: Any) = {
    val p = pAny.asInstanceOf[MmuPluginParameter]
    specs.addRet(new Spec(stages, preAddress, p, new AddressTranslationRsp(this, 1, stages(p.rspAt)))).rsp
  }


  override def wakerCount = 1
  override def wakes = ???

  val setup = create early new Area{
    val cache = getService[DataCachePlugin]
    val cacheLoad = cache.newLoadPort(priority = 1)
  }

  val logic = create late new Area{
    lock.await()

    case class StorageEntry(levelId : Int, depth : Int) extends Bundle {
      val w = spec.levels.drop(levelId).map(_.width).sum
      val valid, exception = Bool()
      val virtualAddress  = UInt(w-log2Up(depth) bits)
      val physicalAddress = UInt(w bits)
      val allowRead, allowWrite, allowExecute, allowUser = Bool
    }


    val satp = new Area{
      val (modeWidth, ppnWidth) = Global.XLEN.get match {
        case 32 => (1, 20) //20 instead of 22 to avoid 34 physical bits
        case 64 => (4, 44)
      }
      val mode = Reg(Bits(modeWidth bits))
      val ppn  = Reg(UInt(ppnWidth bits))
    }


    val ports = for(e <- specs) yield new Area{
      val portSpec = e
      import portSpec._

      val storage = for(e <- portSpec.p.levels) yield new Area{
        val level = e
        val specLevel = spec.levels(level.id)
        val ways = List.fill(level.ways)(Mem.fill(level.depth)(StorageEntry(level.id, level.depth)))
        val writes = ways.map(_.writePort.setIdle())

        val write = new Area{
          val mask    = Bits(level.ways bits)
          val address = UInt(log2Up(level.depth) bits)
          val data    = StorageEntry(level.id, level.depth)

          mask := 0
          address.assignDontCare()
          data.assignDontCare()

          for((way, sel) <- (ways, mask.asBools).zipped){
            way.write(address, data, sel)
          }
        }
        val allocId = Counter(level.ways)
      }
    }


    val refill = new StateMachine{
      val IDLE = new State
      val CMD, RSP = List.fill(spec.levels.size)(new State)

      val portOh = Reg(Bits(ports.size bits))
      val virtual = Reg(UInt(preWidth bits))

      setEntry(IDLE)
      IDLE whenIsActive{
        when(???) {
          load.address := satp.ppn @@ spec.levels.last.vpn(virtual) @@ U(0, log2Up(spec.entryBytes) bits)
          goto(CMD(spec.levels.size - 1))
        }
      }


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
            storageLevel.write.address              := virtual(specLevel.addressOffset, log2Up(storageLevel.level.depth) bits)
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