package naxriscv.fetch

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import naxriscv.interfaces.{AddressTranslationService, InitCycles, JumpCmd, JumpService}
import spinal.lib.pipeline._
import naxriscv.utilities.Plugin
import naxriscv._
import naxriscv.Global._
import naxriscv.fetch._
import naxriscv.Fetch._

import scala.collection.mutable.ArrayBuffer
import Frontend._


class PcPlugin(var resetVector : BigInt = 0x80000000l, fetchPcIncAt : Int = 1) extends Plugin with JumpService{
  case class JumpSpec(interface :  Flow[JumpCmd], priority : Int, aggregationPriority : Int)
  val jumpsSpec = ArrayBuffer[JumpSpec]()
  override def createJumpInterface(priority : Int, aggregationPriority : Int = 0): Flow[JumpCmd] = {
    jumpsSpec.addRet(JumpSpec(Flow(JumpCmd(PC_WIDTH)), priority, aggregationPriority)).interface
  }


  val setup = create early new Area{
    val fetch = getService[FetchPlugin]
    fetch.lock.retain()
  }

  val logic = create late new Area{
    val stage = setup.fetch.getStage(0)
    val fetch = getService[FetchPlugin]
    val pipeline = fetch.getPipeline()
    import stage._

    val sliceRange = (SLICE_RANGE_LOW + log2Up(SLICE_COUNT) - 1 downto SLICE_RANGE_LOW)

    val jump = new Area {
      val sortedByStage = jumpsSpec.sortWith(_.priority > _.priority)
      val valids = sortedByStage.map(_.interface.valid)
      val cmds = sortedByStage.map(_.interface.payload)
      val oh = OHMasking.firstV2(valids.asBits)

      val grouped = sortedByStage.groupByLinked(_.aggregationPriority).toList.sortBy(_._1).map(_._2)
      var target = PC()
      for(group <- grouped){
        val indexes = group.map(e => sortedByStage.indexOf(e))
        val goh = indexes.map(i => oh(i))
        val mux = OhMux.or(goh, group.map(_.interface.pc))
        if(group == grouped.head) target := mux else when(goh.orR){
          KeepAttribute(target)
          target \= mux
        }
      }

      val pcLoad = Flow(JumpCmd(pcWidth = widthOf(PC)))
      pcLoad.valid   := jumpsSpec.map(_.interface.valid).orR
      pcLoad.pc := target
    }

    val init = new Area{
      val requests = getServicesOf[InitCycles]
      val request = (0 +: requests.map(_.initCycles)).max
      val counter = Reg(UInt(log2Up(request) + 1 bits)) init(0)
      val booted = counter.msb
      counter := counter + U(!booted)
    }

    val fetchPc = new Area{
      //PC calculation without Jump
      val output = Stream(PC)
      val pcReg = Reg(PC) init(resetVector) addAttribute(Verilator.public)
      val correction = False
      val correctionReg = RegInit(False) setWhen(correction) clearWhen(output.fire)
      val corrected = correction || correctionReg
      val pcRegPropagate = False
      val inc = RegInit(False) clearWhen(correction || pcRegPropagate) setWhen(output.fire) clearWhen(!output.valid && output.ready)
      val pc = pcReg + (U(inc) << sliceRange.high+1)


      val flushed = False

      when(inc) {
        pc(sliceRange) := 0
      }

      when(jump.pcLoad.valid) {
        correction := True
        pc := jump.pcLoad.pc
        flushed := True
      }

      when(init.booted && (output.ready || correction || pcRegPropagate)){
        pcReg := pc
      }

      pc(0) := False
      if(!RVC) pc(1) := False

      val fetcherHalt = False
      output.valid := !fetcherHalt && init.booted
      output.payload := pc
    }

    fetchPc.output.ready := stage.isReady
    stage.valid := fetchPc.output.valid
    stage(FETCH_PC) := fetchPc.output.payload

    val pcNext = new Area{
      val stage = fetch.getStage(fetchPcIncAt)
      stage(FETCH_PC_INC) := stage(FETCH_PC) + (1 << sliceRange.high+1)
      stage(FETCH_PC_INC)(sliceRange.high downto 0) := 0
    }

    setup.fetch.lock.release()
  }
}
