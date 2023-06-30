// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.platform

import naxriscv.NaxRiscv
import naxriscv.debug.EmbeddedJtagPlugin
import naxriscv.fetch.FetchCachePlugin
import naxriscv.lsu.{DataCachePlugin, LsuPlugin}
import naxriscv.misc.PrivilegedPlugin
import naxriscv.utilities.{NaxScope, Plugin}
import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbInterconnectGenerator}
import spinal.lib.generator.Dependable

class NaxRiscvBmbGenerator()(implicit interconnect: BmbInterconnectGenerator = null) extends Area{
  val plugins = Handle[Seq[Plugin]]
  val iMem, dMemWrite, dMemRead = Handle[Bmb]
  val iPeriph, dPeriph = Handle[Bmb]

  val externalInterrupt = Handle[Bool]
  val externalSupervisorInterrupt = Handle[Bool]
  val timerInterrupt = Handle[Bool]
  val softwareInterrupt = Handle[Bool]

  def setTimerInterrupt(that: Handle[Bool]) =    Dependable(that, timerInterrupt){timerInterrupt := that}
  def setSoftwareInterrupt(that: Handle[Bool]) = Dependable(that, softwareInterrupt){softwareInterrupt := that}

  val logic = Handle(new Area{
    val cpu = new NaxRiscv(plugins)
    NaxScope(cpu.database) on {
      for(plugin <- cpu.plugins) plugin match {
        case p: FetchCachePlugin => new Composite(p){
          val (periph, mem) = p.mem.ioSplit()
          iPeriph.load(mem.toBmb())
          iMem.load(periph.toBmb())
        }
        case p: DataCachePlugin => new Composite(p){
          dMemWrite.load(p.mem.write.toBmb())
          dMemRead.load(p.mem.read.toBmb())
        }
        case p: LsuPlugin => new Composite(p){
          dPeriph.load(p.peripheralBus.toBmb())
        }
        case plugin: PrivilegedPlugin => {
          externalInterrupt load plugin.io.int.machine.external
          timerInterrupt load plugin.io.int.machine.timer
          softwareInterrupt load plugin.io.int.machine.software
          if (plugin.p.withSupervisor) externalSupervisorInterrupt load plugin.io.int.supervisor.external
        }
        //      case plugin: EmbeddedJtagPlugin => plugin.debugClockDomain {
        //        if(debugAskReset.get != null) when(RegNext(plugin.io.resetOut)) {
        //          debugAskReset.get()
        //        } else {
        //          debugReset.load(RegNext(plugin.io.resetOut))
        //        }
        //
        //        withDebug.get match {
        //          case DEBUG_JTAG => jtag <> plugin.io.bus.fromJtag()
        //          case DEBUG_JTAG_CTRL => jtagInstructionCtrl <> plugin.io.bus.fromJtagInstructionCtrl(jtagClockDomain, 0)
        //          case DEBUG_BUS => debugBus <> plugin.io.bus
        //          case DEBUG_BMB => debugBmb >> plugin.io.bus.fromBmb()
        //        }
        //      }
        case _ =>
      }
    }

  })

  if(interconnect != null){
    def add(that : Handle[Bmb]) = interconnect.addMaster(
      accessRequirements = that.derivate(_.p.access),
      bus                = that
    )
    add(iMem)
    add(iPeriph)
    add(dMemRead)
    add(dMemWrite)
    add(dPeriph)
  }
}
