package naxriscv.platform

import naxriscv.Config
import naxriscv.compatibility.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import naxriscv.fetch.FetchAxi4
import naxriscv.lsu.{DataCacheAxi4, LsuPeripheralAxiLite4}
import naxriscv.misc.PrivilegedPlugin
import naxriscv.utilities.Framework
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.misc.WishboneClint
import spinal.lib.misc.plic.WishbonePlic


class NaxRiscvLitex extends Component{
  val cpu = new Component {
    Config.properties()
    val plugins = Config.plugins()
    plugins += new FetchAxi4
    plugins += new DataCacheAxi4
    plugins += new LsuPeripheralAxiLite4
    val framework = new Framework(plugins)
  }

  val ram = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiRam.toIo()
    val dbus = cpu.framework.getService[DataCacheAxi4].logic.axi.toIo()
    Axi4SpecRenamer(ibus)
    Axi4SpecRenamer(dbus)
  }

  val peripheral = new Area{
    val ibus = cpu.framework.getService[FetchAxi4].logic.axiIo.toIo()
    val dbus = cpu.framework.getService[LsuPeripheralAxiLite4].logic.axi.toIo()
    AxiLite4SpecRenamer(ibus)
    AxiLite4SpecRenamer(dbus)

    val clintCtrl = new WishboneClint(1)
    val plicCtrl = new WishbonePlic(
      sourceCount = 32,
      targetCount = 2
    )

    val clint = clintCtrl.io.bus.toIo()
    val plic = plicCtrl.io.bus.toIo()
    val interrupt = plicCtrl.io.sources.toIo()

    val priv = cpu.framework.getService[PrivilegedPlugin].io
    priv.int.machine.timer       := clintCtrl.io.timerInterrupt(0)
    priv.int.machine.software    := clintCtrl.io.softwareInterrupt(0)
    priv.int.machine.external    := plicCtrl.io.targets(0)
    priv.int.supervisor.external := plicCtrl.io.targets(1)
  }
}

object LitexGen extends App{
  val spinalConfig = SpinalConfig(inlineRom = true)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  spinalConfig.generateVerilog(new NaxRiscvLitex)
}