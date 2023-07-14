package naxriscv.sandbox.demo

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

object DemoPipeline extends App{
  import spinal.lib.pipeline._
  SimConfig.withFstWave.compile(new Component{
    val x,y,z = in UInt(8 bits)
    val result = out UInt(16 bits)

    val pip = new Pipeline{
      val X, Y, Z = Stageable(UInt(8 bits))
      val PC = Stageable(UInt(32 bits))

      val A = new Stage{
        X := x
        Y := y
        Z := z
        (PC , 0) := 42
        (PC , 1) := 42
        (PC , "TRUE") := 42
        (PC , "FALSE") := 42

        valid := True
      }
      val B = new Stage(Connection.M2S()){
        val XY = insert(X * Y)

        when(XY < 100){
          haltIt()
        }
      }
      val C = new Stage(Connection.M2S()){
        result := B.XY + Z
      }

      def func()(implicit loc : spinal.idslplugin.Location): Unit ={
        val miaou = Bool()
        miaou.setLambdaName(A.isNamed && B.isNamed)(s"aaa_${A.getName()}_${B.getName()}_bbb_${loc.file}_${loc.line}")
      }

      func()

      build()
    }
  }).doSim{ dut =>
    dut.clockDomain.forkStimulus(10)
    List(dut.x, dut.y, dut.z).foreach(_ #= 0)
    dut.clockDomain.waitSampling()
    dut.clockDomain.waitSampling()
    dut.x #= 1
    dut.y #= 2
    dut.z #= 3
    dut.clockDomain.waitSampling()
    List(dut.x, dut.y, dut.z).foreach(_ #= 0)
    dut.clockDomain.waitSampling(10)
    println("<3")
  }
}




object DemoThread extends App{
  import spinal.core.fiber._
  SpinalVerilog(new Component{
    val jumpsPorts = ArrayBuffer[Flow[UInt]]()
    val jumpsPortsLock = Lock()

    val pcThread = hardFork {
      jumpsPortsLock.await()
      val pc = Reg(UInt(32 bits))
      for (port <- jumpsPorts) {
        println("Got a jump port")
        when(port.valid) {
          pc := port.payload
        }
      }
    }

    jumpsPortsLock.retain()
    val predictorThread = hardFork {
      val predictorJump = Flow(UInt(32 bits))
      jumpsPorts += predictorJump
      jumpsPortsLock.release()
    }
  })
}