package naxriscv.platform


import naxriscv.execute.CsrAccessPlugin
import naxriscv.fetch.AlignerPlugin
import naxriscv.frontend.DecoderPlugin
import naxriscv.lsu2.Lsu2Plugin
import naxriscv.misc.{CommitPlugin, PrivilegedPlugin, RegFilePlugin, RobPlugin}
import naxriscv.{NaxRiscv, riscv}
import spinal.core.{Bool, assert}
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
class NaxriscvProbe(nax : NaxRiscv, hartId : Int){

  class RobCtx{
    var pc = -1l;
    var integerWriteValid = false
    var integerWriteData = -1l
    var floatWriteValid = false
    var floatWriteData = -1l
    var floatFlags = -1

    var csrValid = false
    var csrWriteDone = false
    var csrReadDone = false
    var csrAddress = -1
    var csrWriteData = -1l
    var csrReadData = -1l

    var lsuAddress = -1l
    var lsuLen = 0
    var storeValid = false
    var storeData = -1l
    var storeSqId = -1
    var loadValid = false
    var loadLqId = 0
    var loadData = -1l

    var isSc = false
    var scFailure = false

    def clear(){
      integerWriteValid = false;
      floatWriteValid = false;
      csrValid = false;
      csrWriteDone = false;
      csrReadDone = false;
      floatFlags = 0;
      loadValid = false
      storeValid = false
      isSc = false
    }
    clear()
  };

  val alignerPlugin = nax.framework.getService[AlignerPlugin]
  val csrAccessPlugin = nax.framework.getService[CsrAccessPlugin]
  val decodePlugin = nax.framework.getService[DecoderPlugin]
  val commitPlugin = nax.framework.getService[CommitPlugin]
  val privPlugin = nax.framework.getService[PrivilegedPlugin]
  val robPlugin = nax.framework.getService[RobPlugin]
  val lsuPlugin = nax.framework.getService[Lsu2Plugin]
  val intRf = nax.framework.getServiceWhere[RegFilePlugin](_.spec == riscv.IntRegFile)

  val commitWb = commitPlugin.logic.whitebox
  val commitMask = commitWb.commit.mask.simProxy()
  val commitRobId = commitWb.commit.robId.simProxy()
  val commitPc = commitWb.commit_pc.map(_.simProxy()).toArray
  val commitRobToPcValid = commitWb.robToPc.valid.simProxy()
  val commitRobToPcRobId = commitWb.robToPc.robId.simProxy()
  val commitRobToPcValue = commitWb.robToPc.pc.map(_.simProxy()).toArray

  val intRfEvents = intRf.logic.writeEvents.toArray
  val intRfEventsValid = intRfEvents.map(_.valid.simProxy())

  val ioBus = lsuPlugin.peripheralBus
  val ioBusCmdValid = ioBus.cmd.valid.simProxy()
  val ioBusCmdReady = ioBus.cmd.ready.simProxy()
  val ioBusRspValid = ioBus.rsp.valid.simProxy()

  val lsuWb = lsuPlugin.logic.sharedPip.cacheRsp.whitebox
  val lsuWbValid = lsuWb.valid.simProxy()
  val aguWb = lsuPlugin.logic.sharedPip.feed.agu
  val aguWbValid = aguWb.valid.simProxy()
  val wbWb = lsuPlugin.logic.writeback.rsp.whitebox
  val wbWbValid = wbWb.valid.simProxy()
  val lqFlush = lsuPlugin.logic.lqFlush.simProxy()
  val amoLoadWb = lsuPlugin.logic.special.atomic.loadWhitebox
  val amoLoadValid = amoLoadWb.valid.simProxy()
  val amoStoreWb = lsuPlugin.logic.special.atomic.storeWhitebox
  val amoStoreValid = amoStoreWb.valid.simProxy()

  var cycleSinceLastCommit = 0

  val ioInt = privPlugin.io.int
  class InterruptChecker(pin : Bool, id : Int){
    val proxy = pin.simProxy()
    var last = false
    def check() : Unit = {
      val value = proxy.toBoolean
      if(value != last){
        backends.foreach(_.setInterrupt(hartId, id, value))
        last = value
      }
    }
  }
  val intMachineTimer = new InterruptChecker(ioInt.machine.timer, 7)
  val intMachineSoftware = new InterruptChecker(ioInt.machine.software, 3)
  val intMachineExternal = new InterruptChecker(ioInt.machine.external, 11)
  val intSupervisorExternal = new InterruptChecker(ioInt.supervisor.external, 9)

  val csrAccess = csrAccessPlugin.logic.whitebox.csrAccess
  val csrAccessValid = csrAccess.valid.simProxy()

  val trapWb = privPlugin.logic.whitebox
  val trapFire = trapWb.trap.fire.simProxy()

  val backends = ArrayBuffer[TraceBackend]()
  val commitsCallbacks = ArrayBuffer[(Int, Long) => Unit]()
  nax.scopeProperties.restore()
  val xlen = decodePlugin.xlen

  val robArray = Array.fill(robPlugin.robSize)(new RobCtx)


  def add(tracer : TraceBackend) : this.type = {
    backends += tracer
    tracer.newCpuMemoryView(hartId, lsuPlugin.lqSize+1, lsuPlugin.sqSize) //+1 because AMO
    tracer.newCpu(hartId, "RV32IMA", "MSU", 32, hartId)
    tracer.setPc(hartId, 0x80000000)
    this
  }

  def checkRob() : Unit = {
    if(commitRobToPcValid.toBoolean){
      val robId = commitRobToPcRobId.toInt
      for(i <- 0 until commitRobToPcValue.size){
        var pc = commitRobToPcValue(i).toLong;
        if(xlen == 32) pc = (pc << 32) >> 32
        val ctx = robArray(robId + i)
        ctx.clear()
        ctx.pc = pc
      }
    }

    for(i <- 0 until intRfEvents.size){
      if(intRfEventsValid(i).toBoolean){
        val port = intRfEvents(i)
        val robId = port.robId.toInt
        val ctx = robArray(robId)
        ctx.integerWriteValid = true
        var data = port.data.toLong
        if(xlen == 32) data = (data << 32) >> 32
        ctx.integerWriteData = data
      }
    }

    if(csrAccessValid.toBoolean){
      val robId = csrAccess.robId.toInt
      val ctx = robArray(robId)
      ctx.csrValid = true
      ctx.csrAddress = csrAccess.address.toInt
      ctx.csrWriteDone = csrAccess.writeDone.toBoolean
      ctx.csrReadDone = csrAccess.readDone.toBoolean
      ctx.csrWriteData = csrAccess.write.toLong
      ctx.csrReadData = csrAccess.read.toLong
    }
  }

  def checkTrap(): Unit ={
    if(trapFire.toBoolean){
      val code = trapWb.trap.code.toInt
      val interrupt = trapWb.trap.interrupt.toBoolean
      backends.foreach(_.trap(hartId, interrupt, code))
    }
  }

  def checkCommits(): Unit ={
    var mask = commitMask.toInt
    cycleSinceLastCommit += 1
    if(cycleSinceLastCommit == 10000){
      simFailure("Nax didn't commited anything since too long")
    }
    for(i <- 0 until commitPlugin.commitCount){
      if((mask & 1) != 0){
        cycleSinceLastCommit = 0
        val robId = commitRobId.toInt + i
        val robCtx = robArray(robId)
        if(robCtx.loadValid){
          backends.foreach(_.loadCommit(hartId, robCtx.loadLqId))
        }
        if(robCtx.isSc){
          backends.foreach(_.storeConditional(hartId, robCtx.scFailure))
        }
        if(robCtx.storeValid){
          backends.foreach(_.storeCommit(hartId, robCtx.storeSqId, robCtx.lsuAddress, robCtx.lsuLen, robCtx.storeData))
        }
        if(robCtx.integerWriteValid){
          backends.foreach(_.writeRf(hartId, 0, 32, robCtx.integerWriteData))
        }
        if(robCtx.csrValid){
          if(robCtx.csrReadDone) backends.foreach(_.readRf(hartId, 4, robCtx.csrAddress, robCtx.csrReadData))
          if(robCtx.csrWriteDone) backends.foreach(_.writeRf(hartId, 4, robCtx.csrAddress, robCtx.csrWriteData))
        }
        backends.foreach(_.commit(hartId, robCtx.pc))
        commitsCallbacks.foreach(_(hartId, robCtx.pc))
      }
      mask >>= 1
    }
  }



  var ioAccess : TraceIo = null

  def checkIoBus(): Unit ={
    if(ioBusCmdValid.toBoolean && ioBusCmdReady.toBoolean){
      assert(ioAccess == null)
      ioAccess = new TraceIo(
        write   = ioBus.cmd.write.toBoolean,
        address = ioBus.cmd.address.toLong,
        data    = ioBus.cmd.data.toLong,
        mask    = ioBus.cmd.mask.toInt,
        size    = 1 << ioBus.cmd.size.toInt,
        error   = false
      )
    }
    if(ioBusRspValid.toBoolean){
      assert(ioAccess != null)
      ioAccess.error = ioBus.rsp.error.toBoolean
      if(!ioAccess.write) {
        val offset = (ioAccess.address) & (ioBus.p.dataWidth/8-1)
        val mask = (1l << ioAccess.size*8)-1
        ioAccess.data = (ioBus.rsp.data.toLong >> offset*8) & mask
      }
      backends.foreach(_.ioAccess(hartId, ioAccess))
      ioAccess = null
    }
  }

  def checkInterrupts(): Unit ={
    intMachineTimer.check()
    intMachineSoftware.check()
    intMachineExternal.check()
    intSupervisorExternal.check()
  }

  def checkLsu(): Unit ={
    if(lsuWbValid.toBoolean){
      val robId = lsuWb.robId.toInt
      val ctx = robArray(robId)
      ctx.lsuAddress = lsuWb.address.toLong
      ctx.lsuLen = 1 << lsuWb.size.toInt
      if(lsuWb.isLoad.toBoolean) {
        ctx.loadValid = true
        ctx.loadData = lsuWb.readData.toLong
        ctx.loadLqId = lsuWb.lqId.toInt
        backends.foreach(_.loadExecute(hartId, ctx.loadLqId, ctx.lsuAddress, ctx.lsuLen, ctx.loadData))
      } else {
        ctx.storeValid = true
      }
    }
    if(aguWbValid.toBoolean){
      val robId = aguWb.robId.toInt
      val ctx = robArray(robId)
      if(!aguWb.load.toBoolean) {
        ctx.storeSqId = (aguWb.aguId.toInt) % lsuPlugin.sqSize
        ctx.storeData = aguWb.data.toLong
      }
    }
    if(wbWbValid.toBoolean){
      val sqId = wbWb.sqId.toInt
      backends.foreach(_.storeBroadcast(hartId, sqId))
    }
    if(lqFlush.toBoolean){
      backends.foreach(_.loadFlush(hartId))
    }
    if(amoLoadValid.toBoolean){
      val robId = amoLoadWb.robIdV.toInt
      val ctx = robArray(robId)

      ctx.loadValid = true
      ctx.loadData = amoLoadWb.readData.toLong
      ctx.loadLqId = lsuPlugin.lqSize
      backends.foreach(_.loadExecute(hartId, ctx.loadLqId, ctx.lsuAddress, ctx.lsuLen, ctx.loadData))
    }
    if(amoStoreValid.toBoolean){
      val robId = amoStoreWb.robIdV.toInt
      val ctx = robArray(robId)
      ctx.storeValid = true
      ctx.storeData = amoStoreWb.storeData.toLong

      if(amoStoreWb.isSc.toBoolean){
        val passed = amoStoreWb.scPassed.toBoolean
        if(!passed) ctx.storeValid = false
        ctx.isSc = true
        ctx.scFailure = !passed
      }
    }
  }

  nax.clockDomain.onSamplings{
    checkLsu()
    checkCommits()
    checkTrap()
    checkIoBus()
    checkRob()
    checkInterrupts()
  }
}
