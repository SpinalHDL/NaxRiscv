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
            RS.mantissa.raw := isSubnormal ## f64.mantissa
            RS.quiet := f64.mantissa.msb
            manZero := f64.mantissa === 0
            expZero := f64.exponent === 0
            expOne  := f64.exponent.andR
            recodedExpOffset := exponentF64Subnormal
          } {
            RS.sign := f32.sign
            expRaw := f32.exponent.resized
            RS.quiet := f32.mantissa.msb
            RS.mantissa.raw := isSubnormal ## (f32.mantissa << (if (p.rvd) 29 else 0))
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

    val mergeMantissaExp = -p.mantissaWidth-1
    case class MergeInput() extends Bundle{
      val value = FloatUnpacked(
        exponentWidth = p.exponentWidth+1,
        factorMax = (BigInt(1) << -mergeMantissaExp+3) - 1,
        factorExp = mergeMantissaExp
      )
      val format = FpuFormat()
      val roundMode = FpuRoundMode()
      val robId = UInt(p.robIdWidth bits)
      val scrap = Bool()
      val NV = Bool()
      val DZ = Bool()
    }

    val mul = new Pipeline{
      val input = new Stage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS  = frontend.unpack.rs.map(rs => insert(frontend.dispatch(rs.RS)))
        val hit = CMD.opcode === FpuOpcode.MUL
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)
      }

      val mul = new Stage(M2S()){
        val M1 = insert(input.RS(0).mantissa.raw.asUInt)
        val M2 = insert(input.RS(1).mantissa.raw.asUInt)
      }
      val sum1 = new Stage(DIRECT())
      val sum2 = new Stage(M2S()){
        val EXP = insert(input.RS(0).exponent +^ input.RS(1).exponent)
        val SIGN = insert(input.RS(0).sign ^ input.RS(1).sign)
      }
      val sum3 = new Stage(M2S()){
        val FACTOR_RAW = Stageable(UInt(p.mantissaWidth*2+2 bits))
        val FACTOR = insert(AFix(FACTOR_RAW) >> p.mantissaWidth*2)
      }

//      val spliter = new PipelinedMul(
//        rsA          = mul.M1,
//        rsB          = mul.M2,
//        result       = sum3.FACTOR_RAW,
//        splitWidthA  = if(p.rvd) 52+1 else 23+1,
//        splitWidthB  = if(p.rvd) 52+1 else 23+1,
//        sum1WidthMax = 18,
//        sum2WidthMax = 18,
//        mulStage     = mul,
//        sum1Stage    = sum1,
//        sum2Stage    = sum2,
//        sum3Stage    = sum3
//      )

      val output = new Stage(M2S()){
        val stream = Stream(MergeInput())
        haltIt(!stream.ready)
        stream.valid          := isValid
        stream.value.sign     := sum2.SIGN
        stream.value.exponent := sum2.EXP
        stream.value.mantissa   := this(sum3.FACTOR).truncated() //(spliter.keys.MUL_SUM3 >> mergeMantissaExp-(-p.mantissaWidth*2)).resized
        stream.format         := input.CMD.format
        stream.roundMode      := input.CMD.roundMode
        stream.robId          := input.CMD.robId
        stream.scrap          := False //TODO
        stream.NV             := False //TODO
        stream.DZ             := False //TODO
      }
    }

    val backend = new Pipeline{
      val merge = new Stage{
        val inputs = ArrayBuffer[Stream[MergeInput]]()
        inputs += mul.output.stream
        val arbitrated = StreamArbiterFactory.lowerFirst.noLock.on(inputs)
        val VALUE     = insert(arbitrated.value)
        val FORMAT    = insert(arbitrated.format)
        val ROUNDMODE = insert(arbitrated.roundMode)
        val ROBID     = insert(arbitrated.robId)
        val SCRAP     = insert(arbitrated.scrap)
        val NV        = insert(arbitrated.NV)
        val DZ        = insert(arbitrated.DZ)
        valid := arbitrated.valid
        arbitrated.ready := isReady
      }

      val output = new Stage(M2S()){
        io.ports(0).floatCompletion.valid := isFireing
        io.ports(0).floatCompletion.flags := FpuFlags().getZero
        io.ports(0).floatCompletion.robId := merge.ROBID
        io.ports(0).floatCompletion.value := 0//RS(0).sign ## B(RS(0).exponent + 1) ## RS(0).factor.raw.dropHigh(1)
      }
    }
  }
  flt.frontend.build()
  flt.mul.build()
  flt.backend.build()
}
