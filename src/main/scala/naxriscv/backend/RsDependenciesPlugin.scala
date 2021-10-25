package naxriscv.backend

import naxriscv.interfaces.{RegfileSpec, RenamerService}
import naxriscv.utilities.Plugin
import spinal.core._

class RsDependenciesPlugin(regfileConfig : RegfileSpec) extends Plugin  {

  val setup = create early new Area{

  }

  val logic = create late new Area{


  }
}
