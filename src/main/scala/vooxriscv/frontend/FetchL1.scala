package vooxriscv.frontend
import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbParameter, BmbSourceParameter}
import vooxriscv.frontend.Frontend._
import vooxriscv.utilities._

class FetchL1(firstStageAt : Int = 0) extends Plugin with FetchPipelineRequirements {
  override def stagesCountMin = firstStageAt + 3

  val setup = create early new Area{
    val pipeline = getService(classOf[FrontendPlugin])
    pipeline.lock.retain()

    val pcPlugin = getService(classOf[PcPlugin])
//    val redoJump = pcPlugin.createJumpInterface()
  }

  val logic = create late new Area{
//    val bmbConfig = BmbAccessParameter(
//      addressWidth = PC_WIDTH,
//      dataWidth = FETCH_DATA_WIDTH
//    ).addSources(1, BmbSourceParameter(
//      contextWidth = 0,
//      lengthWidth = 2,
//      alignment = BmbParameter.BurstAlignement.LENGTH,
//      canWrite = false
//    ))
//    val cmdStage = setup.pipeline.getStage(0)
//    val rspStage = setup.pipeline.getStage(1)
//    val bus = master(Bmb(bmbConfig))
//    bus.cmd.valid := cmdStage.isValid
//    cmdStage.haltIt(bus.cmd.isStall)

    val injectStage = setup.pipeline.getStage(stagesCountMin-1)
    injectStage(WORD) := 0x1111
    injectStage(MASK) := 0x1


    setup.pipeline.lock.release()
  }
}
