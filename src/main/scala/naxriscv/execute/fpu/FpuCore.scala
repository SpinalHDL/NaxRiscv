package naxriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Connection.{DIRECT, M2S}
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

case class FpuCore(p : FpuParameter) extends Component{
  val io = new Bundle {
    val ports = Vec.fill(p.portCount)(slave(FpuPort(p)))
  }

  val exponentOne = (if(p.rvd) 1 << 10 else 1 << 7)-1
  val exponentF32Subnormal = exponentOne-127
  val exponentF64Subnormal = exponentOne-1023
  val exponentF32Infinity = exponentOne+127+1
  val exponentF64Infinity = exponentOne+1023+1
  def whenDouble(format : FpuFormat.C)(yes : => Unit)(no : => Unit): Unit ={
    if(p.rvd) when(format === FpuFormat.DOUBLE) { yes } otherwise{ no }
    if(!p.rvd) no
  }

  def muxDouble[T <: Data](format : FpuFormat.C)(yes : => T)(no : => T): T ={
    if(p.rvd) ((format === FpuFormat.DOUBLE) ? { yes } | { no })
    else no
  }

  val flt = new Area{
    val frontend = new Pipeline{
      val arbiter = new Stage{
        val logic = StreamArbiterFactory.noLock.roundRobin.build(FpuFloatCmd(p.rvd, p.robIdWidth), p.portCount)
        logic.io.inputs <> Vec(io.ports.map(_.floatCmd))

        valid := logic.io.output.valid
        logic.io.output.ready := isReady
        val CMD = insert(logic.io.output.payload)
        val SOURCE = insert(logic.io.chosen)
      }

      val unpack = new Stage{
        connect(arbiter, this)(M2S())

        val CMD = insert(arbiter.CMD.withoutRs())
        val rs = for(input <- arbiter.CMD.rs) yield new Area{
          val RS = Stageable(FloatUnpacked(
            exponentWidth = p.exponentWidth,
            factorMax = (BigInt(1) << p.mantissaWidth+1) - 1,
            factorExp = -p.mantissaWidth
          ))

          val f32 = new Area{
            val mantissa = input(0, 23 bits).asUInt
            val exponent = input(23, 8 bits).asUInt
            val sign     = input(31)
          }
          val f64 = p.rvd generate new Area{
            val mantissa = input(0, 52 bits).asUInt
            val exponent = input(52, 11 bits).asUInt
            val sign     = input(63)
          }

          val manZero = Bool()
          val expZero = Bool()
          val expOne  = Bool()
          val isSubnormal =  expZero && !manZero
          val recodedExpOffset = UInt(p.exponentWidth bits)
          val expRaw = UInt(p.exponentWidth bits)

          whenDouble(CMD.format){
            RS.sign := f64.sign
            expRaw := f64.exponent.resized
            RS.factor.raw := isSubnormal ## f64.mantissa
            RS.quiet := f64.mantissa.msb
            manZero := f64.mantissa === 0
            expZero := f64.exponent === 0
            expOne  := f64.exponent.andR
            recodedExpOffset := exponentF64Subnormal
          } {
            RS.sign := f32.sign
            expRaw := f32.exponent.resized
            RS.quiet := f32.mantissa.msb
            RS.factor.raw := isSubnormal ## (f32.mantissa << (if (p.rvd) 29 else 0))
            manZero := f32.mantissa === 0
            expZero := f32.exponent === 0
            expOne  := f32.exponent.andR
            recodedExpOffset := exponentF32Subnormal
          }
          RS.exponent := (expRaw -^ recodedExpOffset + recodedExpOffset + U(isSubnormal)).resized
          RS.mode := (expOne ## expZero).mux(
            default -> FloatMode.NORMAL(),
            1       -> (manZero ? FloatMode.ZERO | FloatMode.SUBNORMAL),
            2       -> (manZero ? FloatMode.INF | FloatMode.NAN)
          )
        }
      }

      val dispatch = new Stage{
        connect(unpack, this)(DIRECT())
      }
    }

//    class MergeInput() extends Bundle{
//      val source = Source()
//      val rd = p.rfAddress()
//      val value = p.writeFloating()
//      val scrap = Bool()
//      val roundMode = FpuRoundMode()
//      val format = p.withDouble generate FpuFormat()
//      val NV = Bool()
//      val DZ = Bool()
//    }

    val mul = new Pipeline{
      val input = new Stage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS  = frontend.unpack.rs.map(rs => insert(frontend.dispatch(rs.RS)))
        val hit = CMD.opcode === FpuOpcode.MUL
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)
        io.ports(0).floatCompletion.valid := isValid
        io.ports(0).floatCompletion.flags := FpuFlags().getZero
        io.ports(0).floatCompletion.robId := CMD.robId
        io.ports(0).floatCompletion.value := RS(0).sign ## B(RS(0).exponent + 1) ## RS(0).factor.raw.dropHigh(1)
      }

      val output = new Stage{
        connect(input, this){M2S()}
      }
    }

//    val backend = new Pipeline{
//      val merge = new Stage{
//        val inputs = ArrayBuffer[Stream[MergeInput]]()
//        inputs += mul.output.)
//        val arbitrated = StreamArbiterFactory.lowerFirst.noLock.on(inputs).toFlow
//      }
//
//      val output = new Stage{
//        connect(input, this){M2S()}
//      }
//    }
  }
  flt.frontend.build()
  flt.mul.build()
}
