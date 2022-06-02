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

  val exponentFxxOne = (if(p.rvd) 1 << 10 else 1 << 7)-1
  val exponentF32Subnormal = exponentFxxOne-127
  val exponentF64Subnormal = exponentFxxOne-1023
  val exponentF32One = 127
  val exponentF64One = 1023
  val exponentF32Infinity = exponentFxxOne+127+1
  val exponentF64Infinity = exponentFxxOne+1023+1
  def whenDouble(format : FpuFormat.C)(yes : => Unit)(no : => Unit): Unit ={
    if(p.rvd) when(format === FpuFormat.DOUBLE) { yes } otherwise{ no }
    if(!p.rvd) no
  }

  def muxDouble[T <: Data](format : FpuFormat.C)(yes : => T)(no : => T): T ={
    if(p.rvd) ((format === FpuFormat.DOUBLE) ? { yes } | { no })
    else no
  }


  class FpuStage(implicit _pip: Pipeline = null)  extends Stage {
    assert(p.portCount == 1)
    throwIt(io.ports(0).unschedule)

    def this(connection: ConnectionLogic)(implicit _pip: Pipeline)  {
      this()
      chainConnect(connection)
    }

  }


  val flt = new Area{
    val frontend = new Pipeline{
      val arbiter = new FpuStage{
        val logic = StreamArbiterFactory.noLock.roundRobin.build(FpuFloatCmd(p.rvd, p.robIdWidth), p.portCount)
        logic.io.inputs <> Vec(io.ports.map(_.floatCmd))

        valid := logic.io.output.valid
        logic.io.output.ready := isReady
        val CMD = insert(logic.io.output.payload)
        val SOURCE = insert(logic.io.chosen)
      }

      val unpack = new FpuStage{
        connect(arbiter, this)(M2S())

        val CMD = insert(arbiter.CMD.withoutRs())
        val rs = for(input <- arbiter.CMD.rs) yield new Area{
          val RS = Stageable(FloatUnpacked(
            exponentMax =  (1 << p.exponentWidth-1) - 1,
            exponentMin = -(1 << p.exponentWidth-1) + 1,
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
            RS.mantissa.raw := !isSubnormal ## f64.mantissa
            RS.quiet := f64.mantissa.msb
            manZero := f64.mantissa === 0
            expZero := f64.exponent === 0
            expOne  := f64.exponent.andR
            recodedExpOffset := exponentF64One
          } {
            RS.sign := f32.sign
            expRaw := f32.exponent.resized
            RS.quiet := f32.mantissa.msb
            RS.mantissa.raw := !isSubnormal ## (f32.mantissa << (if (p.rvd) 29 else 0))
            manZero := f32.mantissa === 0
            expZero := f32.exponent === 0
            expOne  := f32.exponent.andR
            recodedExpOffset := exponentF32One
          }
          RS.exponent := expRaw - recodedExpOffset + U(isSubnormal) //TODO AFix shouldn't allow it
          RS.mode := (expOne ## expZero).mux(
            default -> FloatMode.NORMAL(),
            1       -> (manZero ? FloatMode.ZERO | FloatMode.NORMAL),
            2       -> (manZero ? FloatMode.INF | FloatMode.NAN)
          )
        }
      }

      val dispatch = new FpuStage{
        connect(unpack, this)(DIRECT())
      }
    }

    val mergeMantissaExp = -p.mantissaWidth-1
    case class MergeInput(valueType : HardType[FloatUnpacked]) extends Bundle{
      val value = valueType()
      val format = FpuFormat()
      val roundMode = FpuRoundMode()
      val robId = UInt(p.robIdWidth bits)
      val NV = Bool()
      val DZ = Bool()
    }

    val mul = new Pipeline{
      val input = new FpuStage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS  = frontend.unpack.rs.map(rs => insert(frontend.dispatch(rs.RS)))
        val hit = CMD.opcode === FpuOpcode.MUL || CMD.opcode === FpuOpcode.FMA
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)
      }

      val mul = new FpuStage(DIRECT())
      val sum1 = new FpuStage(DIRECT())
      val sum2 = new FpuStage(DIRECT())
      val sum3 = new FpuStage(DIRECT())
      val result = new FpuStage(DIRECT())

      val logic = new FpuMul(
        pipeline     = this,
        rs1          = input.RS(0),
        rs2          = input.RS(1),
        splitWidthA  = 18,
        splitWidthB  = 18,
        sum1WidthMax = 36,
        sum2WidthMax = 72,
        mulStage     = mul,
        sum1Stage    = sum1,
        sum2Stage    = sum2,
        sum3Stage    = sum3,
        resultStage  = result
      )

      val output = new FpuStage(DIRECT()){
        val merge = Stream(MergeInput(logic.result.RESULT))
        haltIt(!merge.ready && input.CMD.opcode === FpuOpcode.MUL)
        merge.valid     := isValid && input.CMD.opcode === FpuOpcode.MUL
        merge.value     := logic.result.RESULT
        merge.format    := input.CMD.format
        merge.roundMode := input.CMD.roundMode
        merge.robId     := input.CMD.robId
        merge.NV        := False //TODO
        merge.DZ        := False //TODO

        val add = Stream(new Bundle {
          val rs1 = logic.result.RESULT()
          val rs2 = input.RS(2)()
          val NV = Bool()
          val cmd = input.CMD()
        })
        haltIt(!add.ready && input.CMD.opcode === FpuOpcode.FMA)
        add.valid     := isValid && input.CMD.opcode === FpuOpcode.FMA
        add.rs1       := logic.result.RESULT
        add.rs2       := input.RS(2)
        add.NV        := False //TODO
        add.cmd       := input.CMD
      }
    }

    val add = new Pipeline{
      val input = new FpuStage{
        val CMD = insert(mul.output.add.valid ? mul.output.add.cmd | frontend.dispatch(frontend.unpack.CMD))
        val RS1 = insert(mul.output.add.valid ? mul.output.add.rs1 | frontend.dispatch(frontend.unpack.rs(0).RS))
        val RS2 = insert(mul.output.add.valid ? mul.output.add.rs2 | frontend.dispatch(frontend.unpack.rs(1).RS).invert(frontend.dispatch(frontend.unpack.CMD).arg(0)))
        val NV = mul.output.add.valid && mul.output.add.NV //TODO
        val hit = CMD.opcode === FpuOpcode.ADD
        valid := frontend.dispatch.isValid && hit || mul.output.add.valid
        frontend.dispatch.haltIt(hit && (!isReady || mul.output.add.valid))
        mul.output.add.ready := isReady

        RS1.sign
      }

      val preShiftStage = new FpuStage(DIRECT())
      val shifterStage = new FpuStage(DIRECT())
      val mathStage = new FpuStage(DIRECT())
      val logicResultStage = new FpuStage(DIRECT())

      val logic : FpuAdd = new FpuAdd(
        pipeline      = this,
        rs1           = input.RS1,
        rs2           = input.RS2,
        preShiftStage = preShiftStage,
        shifterStage  = shifterStage,
        mathStage     = mathStage,
        resultStage   = logicResultStage
      )


      val resultStage = new FpuStage(DIRECT()){
        val bitsIn = widthOf(logic.result.RESULT.mantissa.raw)
        val bitsAvailable = 3+p.mantissaWidth*2
        val bitsRequired = p.mantissaWidth+1+2
        val bitsShift  = bitsAvailable - bitsRequired
        val valuesCount = (bitsIn+bitsShift-1)/bitsShift

        val normalized = FloatUnpacked(
          exponentMax = logic.result.RESULT.exponentMax+ (valuesCount-1)*bitsShift,
          exponentMin = logic.result.RESULT.exponentMin,
          factorMax   = (BigInt(1) << bitsAvailable),
          factorExp   = -p.mantissaWidth*2
        )

//        (logic.result.RESULT.mantissa |<< 1*bitsShift).rounded(rounding=RoundType.CEIL).toAFix(normalized.mantissa)
        val values = (0 until valuesCount).map(i => (logic.result.RESULT.mantissa |<< i*bitsShift).rounded(rounding=RoundType.CEIL).toAFix(normalized.mantissa))
        val filled = (0 until valuesCount).map(i => logic.result.RESULT.mantissa.raw(bitsIn-i*bitsShift-1 downto ((bitsIn-i*bitsShift-bitsShift) max 0)) =/= 0)
        val exponentOffsets = (0 until valuesCount).map(i => AFix(i*bitsShift))
        val sel = OHToUInt(OHMasking.firstV2(filled.asBits()))

        normalized.exponent := logic.result.RESULT.exponent + exponentOffsets.read(sel)
        normalized.mantissa := values.read(sel)
        normalized.mode     := logic.result.RESULT.mode
        normalized.quiet    := logic.result.RESULT.quiet
        normalized.sign     := logic.result.RESULT.sign

        val stream = Stream(MergeInput(normalized))
        haltIt(!stream.ready)
        stream.valid     := isValid
        stream.value     := normalized
        stream.format    := input.CMD.format
        stream.roundMode := input.CMD.roundMode
        stream.robId     := input.CMD.robId
        stream.NV        := False //TODO
        stream.DZ        := False //TODO
      }
    }

    val backend = new Pipeline{
      val merge = new FpuStage{
        val inputs = ArrayBuffer[Stream[MergeInput]]()
        inputs += mul.output.merge
        inputs += add.resultStage.stream
        val exponentMin = inputs.map(_.value.exponentMin).min
        val exponentMax = inputs.map(_.value.exponentMax).max
        val factorExp = inputs.map(_.value.factorExp).min
        val factorMax = inputs.map(e => e.value.factorMax*(BigInt(1) << (e.value.factorExp - factorExp).max(0))).max

        val remapped = inputs.map{e =>
          val v = Stream(MergeInput(FloatUnpacked(
            exponentMax = exponentMax,
            exponentMin = exponentMin,
            factorExp   = factorExp,
            factorMax   = factorMax
          )))
          v << e
          v
        }

        val arbitrated = StreamArbiterFactory.lowerFirst.noLock.on(remapped)
        val VALUE     = insert(arbitrated.value)
        val FORMAT    = insert(arbitrated.format)
        val ROUNDMODE = insert(arbitrated.roundMode)
        val ROBID     = insert(arbitrated.robId)
        val NV        = insert(arbitrated.NV)
        val DZ        = insert(arbitrated.DZ)
        valid := arbitrated.valid
        arbitrated.ready := isReady
      }


      val logic = new FpuStage(DIRECT()){
        val MAN_EXP_RAW = insert(AFix(OHToUInt(OHMasking.lastV2(merge.VALUE.mantissa.raw)), maxValue = widthOf(merge.VALUE.mantissa.raw)-1, 0 exp))
        val MAN_EXP = insert(merge.VALUE.exponent + MAN_EXP_RAW)
        val EXP_SUBNORMAL = insert(AFix(muxDouble(merge.FORMAT)(S(-1023 - merge.VALUE.factorExp))(S(-127 - merge.VALUE.factorExp))))
        val SUBNORMAL = insert(MAN_EXP <= EXP_SUBNORMAL)
        val MAN_SHIFT = insert(Mux(!SUBNORMAL, apply(MAN_EXP_RAW).asUInt(), (EXP_SUBNORMAL-merge.VALUE.exponent + AFix(1)).asUInt()).resize(log2Up(merge.VALUE.mantissa.bitWidth+p.mantissaWidth+2)))
        val MAN_SHIFTED = insert(Shift.rightWithScrap(merge.VALUE.mantissa.raw << p.mantissaWidth+2, MAN_SHIFT))
        val MAN_RESULT = insert(MAN_SHIFTED(2, p.mantissaWidth bits))
        val EXP = insert(Mux(!SUBNORMAL, (MAN_EXP - EXP_SUBNORMAL).asUInt, U(0)))

        io.ports(0).floatCompletion.valid := isFireing
        io.ports(0).floatCompletion.flags := FpuFlags().getZero
        io.ports(0).floatCompletion.robId := merge.ROBID
        io.ports(0).floatCompletion.value := merge.VALUE.sign ## EXP.resize(11 bits) ## MAN_RESULT
      }

//      val logic = new FpuRounding(
//        pipeline = this,
//        RS       = merge.VALUE,
//        stage    = merge
//      )

//      val output = new FpuStage(M2S()){
//        io.ports(0).floatCompletion.valid := isFireing
//        io.ports(0).floatCompletion.flags := FpuFlags().getZero
//        io.ports(0).floatCompletion.robId := merge.ROBID
//        io.ports(0).floatCompletion.value := 0//RS(0).sign ## B(RS(0).exponent + 1) ## RS(0).factor.raw.dropHigh(1)
//      }
    }
  }


//    val unpackFsm = new Area{
//      val done, boot, patched = Reg(Bool())
//      val ohInputWidth = p.rsIntWidth max p.internalMantissaSize
//      val ohInput = Bits(ohInputWidth bits).assignDontCare()
//      when(!input.i2f) {
//        if(!p.withDouble) ohInput := input.value(0, 23 bits) << 9
//        if( p.withDouble) ohInput := passThroughFloat.mantissa.asBits
//      } otherwise {
//        ohInput(ohInputWidth-p.rsIntWidth-1 downto 0) := 0
//        ohInput(ohInputWidth-p.rsIntWidth, p.rsIntWidth bits) := input.value(p.rsIntWidth-1 downto 0)
//      }
//
//      val i2fZero = Reg(Bool)
//
//      val shift = new Area{
//        val by = Reg(UInt(log2Up(ohInputWidth) bits))
//        val input = UInt(ohInputWidth bits).assignDontCare()
//        var logic = input
//        for(i <- by.range){
//          logic \= by(i) ? (logic |<< (BigInt(1) << i)) | logic
//        }
//        val output = RegNextWhen(logic, !done)
//      }
//      shift.input := (ohInput.asUInt |<< 1).resized
//
//      when(input.valid && (input.i2f || isSubnormal) && !done){
//        busy := True
//        when(boot){
//          when(input.i2f && !patched && input.value(p.rsIntWidth-1) && input.arg(0)){
//            input.value.getDrivingReg(0, p.rsIntWidth bits) := B(input.value.asUInt.twoComplement(True).resize(p.rsIntWidth bits))
//            patched := True
//          } otherwise {
//            shift.by := OHToUInt(OHMasking.first((ohInput).reversed))
//            boot := False
//            i2fZero := input.value(p.rsIntWidth-1 downto 0) === 0
//          }
//        } otherwise {
//          done := True
//        }
//      }
//
//      val expOffset = (UInt(p.internalExponentSize bits))
//      expOffset := 0
//      when(isSubnormal){
//        expOffset := shift.by.resized
//      }
//
//      when(!input.isStall){
//        done := False
//        boot := True
//        patched := False
//      }
//    }

  flt.frontend.build()
  flt.mul.build()
  flt.add.build()
  flt.backend.build()
}
