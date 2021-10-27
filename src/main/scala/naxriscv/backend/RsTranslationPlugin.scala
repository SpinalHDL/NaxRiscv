package naxriscv.backend

import naxriscv.interfaces.{RegfileSpec, RenamerService}
import naxriscv.utilities.Plugin
import spinal.core._
import spinal.lib._

class RsTranslationPlugin(regfileConfig : RegfileSpec) extends Plugin with RenamerService {
  override def newTranslationPort() = ???
  override def rollbackToCommit() = rollback := True

//  case class Port(cmd : Flow[])
  val rollback = create early Bool()
  val setup = create early new Area{

  }

  val logic = create late new Area{
    val commited, issued = Mem.fill(regfileConfig.size)(UInt(log2Up(regfileConfig.size) bits))
    val updated = Vec.fill(regfileConfig.size)(Reg(Bool)) //TODO init


  }
}
