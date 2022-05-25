package naxriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

case class FpuCore(p : FpuParameter) extends Component{
  val io = new Bundle {
    val ports = Vec.fill(p.portCount)(slave(FpuPort(p)))
  }

  val flt = new Area{
    val frontend = new Pipeline{
      val arbiter = addStage(new Stage{
        val logic = StreamArbiterFactory.noLock.roundRobin.build(FpuFloatCmd(p.rvd, p.robIdWidth), p.portCount)
        logic.io.inputs <> Vec(io.ports.map(_.floatCmd))

        val CMD = insert(logic.io.output.payload)
        val SOURCE = insert(logic.io.chosen)
      })

      val unpack = addStage(new Stage{
//        val RS = insert(arbiter.CMD.rs.map { rs =>
//          val ret = FloatUnpacked(
//            exponentWidth = p.exponentWidth,
//            factorMax = (BigInt(1) << p.mantissaWidth+1) - 1,
//            factorExp = -p.mantissaWidth
//          )
//          val f32 = new Area{
//            val mantissa = input.value(0, 23 bits).asUInt
//            val exponent = input.value(23, 8 bits).asUInt
//            val sign     = input.value(31)
//          }
//          val f64 = p.withDouble generate new Area{
//            val mantissa = input.value(0, 52 bits).asUInt
//            val exponent = input.value(52, 11 bits).asUInt
//            val sign     = input.value(63)
//          }
//
//          val recodedExpOffset = UInt(p.internalExponentSize bits)
//          val passThroughFloat = p.internalFloating()
//          passThroughFloat.special := False
//
//          whenDouble(input.format){
//            passThroughFloat.sign := f64.sign
//            passThroughFloat.exponent := f64.exponent.resized
//            passThroughFloat.mantissa := f64.mantissa
//            recodedExpOffset := exponentF64Subnormal
//          } {
//            passThroughFloat.sign := f32.sign
//            passThroughFloat.exponent := f32.exponent.resized
//            passThroughFloat.mantissa := f32.mantissa << (if (p.withDouble) 29 else 0)
//            recodedExpOffset := exponentF32Subnormal
//          }
//
//
//          val manZero = passThroughFloat.mantissa === 0
//          val expZero = passThroughFloat.exponent === 0
//          val expOne =  passThroughFloat.exponent(7 downto 0).andR
//          if(p.withDouble) {
//            expZero.clearWhen(input.format === FpuFormat.DOUBLE && input.value(62 downto 60) =/= 0)
//            expOne.clearWhen(input.format === FpuFormat.DOUBLE && input.value(62 downto 60) =/= 7)
//          }
//
//          val isZero      =  expZero &&  manZero
//          val isSubnormal =  expZero && !manZero
//          val isInfinity  =  expOne  &&  manZero
//          val isNan       =  expOne  && !manZero
//
//          ret.mode     :=
//          ret.quiet    :=
//          ret.sign     :=
//          ret.exponent :=
//          ret.factor   :=
//          ret
//        })
        val CMD = insert(arbiter.CMD.withoutRs())
      })
    }
  }
}
