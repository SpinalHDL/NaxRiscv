package naxriscv.execute.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.pipeline.Connection.{DIRECT, M2S}
import spinal.lib.pipeline._

import scala.collection.mutable.ArrayBuffer

case class FpuCore(p : FpuParameter) extends Component{
  val io = new Bundle {
    val ports = Vec.fill(p.portCount)(slave(FpuPort(p)))
  }

  val exponentF32One = 127
  val exponentF64One = 1023

  def whenDouble(format : FpuFormat.C)(yes : => Unit)(no : => Unit): Unit ={
    if(p.rvd) when(format === FpuFormat.DOUBLE) { yes } otherwise{ no }
    if(!p.rvd) no
  }

  def muxDouble[T <: Data](format : FpuFormat.C)(yes : => T)(no : => T): T ={
    if(p.rvd) ((format === FpuFormat.DOUBLE) ? { yes } | { no })
    else no
  }
  def muxDouble[T <: Data](format : Bool)(yes : => T)(no : => T): T ={
    if(p.rvd) ((format) ? { yes } | { no })
    else no
  }
  def muxRv64[T <: Data](format : Bool)(yes : => T)(no : => T): T ={
    if(p.rv64) ((format) ? { yes } | { no })
    else no
  }

  class FpuStage(implicit _pip: Pipeline = null)  extends Stage {
    assert(p.portCount == 1)
    val doThrow = io.ports(0).unschedule
    throwIt(doThrow)

    def this(connection: ConnectionLogic)(implicit _pip: Pipeline)  {
      this()
      chainConnect(connection)
    }
  }


  //Subnormal/Int to Float logic
  val unpacker = new Pipeline{
    val ohInputWidth = p.rsIntWidth max p.mantissaWidth
    case class Request() extends Bundle {
      val data = Bits(ohInputWidth bits)
    }
    case class Result() extends Bundle {
      val shift = UInt(log2Up(ohInputWidth+1) bits)
      val data = Bits(ohInputWidth bits)
    }

    val portCount = 2
    val arbiter = StreamArbiterFactory().noLock.lowerFirst.build(Request(), portCount)
    val results = Vec.fill(portCount)(Flow(Result()))

    val input = new FpuStage{
      valid := arbiter.io.output.valid
      arbiter.io.output.ready := True

      val args = insert(arbiter.io.output.payload)
      val source = insert(arbiter.io.chosen)
    }
    import input._

    val setup = new FpuStage(M2S()){
      val shiftBy = insert(OHToUInt(OHMasking.firstV2(args.data.reversed << 1)))
    }
    import setup._

    val logic = new FpuStage(M2S()){
      val shifter = args.data |<< shiftBy

      for((port, id) <- results.zipWithIndex){
        port.valid := False
        port.data := shifter
        port.shift := shiftBy
      }

      when(isValid) {
        results(source).valid := True
      }
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

      val unpack = new FpuStage(DIRECT()){
        val CMD = insert(arbiter.CMD.withoutRs())

        val fsmPortId = 0
        val fsmCmd = unpacker.arbiter.io.inputs(fsmPortId)
        val fsmRsp = unpacker.results(fsmPortId)
        fsmCmd.setIdle()

        val fsmRequesters = Bits(3 bits)
        val fsmServed = Bits(3 bits)
        val rs = for((input, inputId) <- arbiter.CMD.rs.zipWithIndex) yield new Area{
          val RS_PRE_NORM = Stageable(FloatUnpacked(
            exponentMax =  (1 << p.exponentWidth-1) - 1,
            exponentMin = -(1 << p.exponentWidth-1) + 1,
            mantissaWidth = p.mantissaWidth
          ))
          val RS = Stageable(FloatUnpacked(
            exponentMax =  (1 << p.exponentWidth-1) - 1,
            exponentMin = -(1 << p.exponentWidth-1) + 1 - p.mantissaWidth,
            mantissaWidth = p.mantissaWidth
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
          val recodedExpSub = SInt(p.exponentWidth+1 bits)
          val expRaw = UInt(p.exponentWidth bits)

          whenDouble(CMD.format){
            RS_PRE_NORM.sign := f64.sign
            expRaw := f64.exponent.resized
            RS_PRE_NORM.mantissa.raw := B(f64.mantissa)
            RS_PRE_NORM.quiet := f64.mantissa.msb
            manZero := f64.mantissa === 0
            expZero := f64.exponent === 0
            expOne  := f64.exponent.andR
            recodedExpOffset := exponentF64One
            recodedExpSub := -exponentF64One+1
          } {
            RS_PRE_NORM.sign := f32.sign
            expRaw := f32.exponent.resized
            RS_PRE_NORM.quiet := f32.mantissa.msb
            RS_PRE_NORM.mantissa.raw := B(f32.mantissa << (if (p.rvd) 29 else 0))
            manZero := f32.mantissa === 0
            expZero := f32.exponent === 0
            expOne  := f32.exponent.andR
            recodedExpOffset := exponentF32One
            recodedExpSub := -exponentF32One+1
          }
          RS_PRE_NORM.exponent := expRaw - recodedExpOffset
          RS_PRE_NORM.mode := (expOne ## expZero).mux(
            default -> FloatMode.NORMAL(),
            1       -> (manZero ? FloatMode.ZERO | FloatMode.NORMAL),
            2       -> (manZero ? FloatMode.INF | FloatMode.NAN)
          )
          apply(RS) := RS_PRE_NORM
          val normalizer = new Area{
            val valid = isValid && isSubnormal && (inputId match {
              case 0 => CMD.opcode =/= FpuOpcode.FMV_X_W
              case 1 => List(FpuOpcode.MUL, FpuOpcode.ADD, FpuOpcode.FMA, FpuOpcode.CMP, FpuOpcode.DIV, FpuOpcode.SQRT, FpuOpcode.MIN_MAX, FpuOpcode.SGNJ).map(CMD.opcode === _).orR
              case 2 => List(FpuOpcode.FMA).map(CMD.opcode === _).orR
            })
            val asked  = RegInit(False) setWhen(fsmRequesters(inputId) && !fsmRequesters.dropLow(inputId + 1).orR) clearWhen(!isStuck || isRemoved)
            val served = RegInit(False) setWhen(fsmRsp.valid && fsmServed.dropLow(inputId + 1).andR) clearWhen(!isStuck || isRemoved)
            fsmRequesters(inputId) := valid && !asked
            fsmServed(inputId)     := !valid || served

            val exponent = Reg(RS.exponent)
            val mantissa = Reg(RS.mantissa)

            when(fsmRequesters(inputId)) {
              fsmCmd.valid := True
              fsmCmd.data := RS_PRE_NORM.mantissa.raw << widthOf(fsmCmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
            }
            when(valid){
              RS.exponent := exponent
              RS.mantissa := mantissa
            }
            when(!served){
              exponent := recodedExpSub - fsmRsp.shift.intoSInt
              mantissa.raw := fsmRsp.data >> widthOf(fsmCmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
            }
            haltWhen(valid && !served)
          }

          if(p.rvd) when(CMD.format === FpuFormat.FLOAT && !input(63 downto 32).andR){
            RS.setNanQuiet
            RS.sign := False
            RS.exponent := AFix(128)
            RS.mantissa.raw := (default -> False, RS.mantissa.raw.high -> True)
          }
        }
      }

      val dispatch = new FpuStage(M2S())
    }

    case class MergeInput(valueType : HardType[FloatUnpacked]) extends Bundle{
      val value = valueType()
      val format = FpuFormat()
      val roundMode = FpuRoundMode()
      val robId = UInt(p.robIdWidth bits)
      val NV = Bool()
      val DZ = Bool()
      val preserve = Bool()
      val raw = Bits(p.rsFloatWidth bits)
      val rawEnable = Bool()
      val canonical = Bool()

      def rawIdle(): Unit ={
        raw.assignDontCare()
        rawEnable := False
      }
    }

    val mul = p.withMul generate new Pipeline{
      val input = new FpuStage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS  = frontend.unpack.rs.map(rs => insert(frontend.dispatch(rs.RS)))
        val hit = CMD.opcode === FpuOpcode.MUL || CMD.opcode === FpuOpcode.FMA
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)
      }

      val mul = new FpuStage(DIRECT())
      val sum1 = new FpuStage(DIRECT())
      val sum2 = new FpuStage(M2S())
      val sum3 = new FpuStage(DIRECT())
      val norm = new FpuStage(M2S())
      val result = new FpuStage(DIRECT())

      val logic = new FpuMul(
        pipeline     = this,
        rs1          = input.RS(0),
        rs2          = input.RS(1),
        splitWidthA  = 18,
        splitWidthB  = 18,
        sum1WidthMax = 36,
        sum2WidthMax = 1000,
        mulStage     = mul,
        sum1Stage    = sum1,
        sum2Stage    = sum2,
        sum3Stage    = sum3,
        normStage    = norm,
        resultStage  = result
      )

      val output = new FpuStage(DIRECT()){
        val merge = Stream(MergeInput(
          FloatUnpacked(
            exponentMax = logic.result.RESULT.exponentMax,
            exponentMin = logic.result.RESULT.exponentMin,
            mantissaWidth = p.mantissaWidth+2
          )
        ))
        haltIt(!merge.ready && input.CMD.opcode === FpuOpcode.MUL)
        merge.valid          := isValid && input.CMD.opcode === FpuOpcode.MUL
        merge.value.mode     := logic.result.RESULT.mode
        merge.value.quiet    := logic.result.RESULT.quiet
        merge.value.sign     := logic.result.RESULT.sign
        merge.value.exponent := logic.result.RESULT.exponent
        merge.value.mantissa := logic.result.RESULT.mantissa.rounded(RoundType.SCRAP)
        merge.format         := input.CMD.format
        merge.roundMode      := input.CMD.roundMode
        merge.robId          := input.CMD.robId
        merge.NV             := logic.result.NV
        merge.DZ             := False
        merge.canonical      := True
        merge.rawIdle()

        val add = p.withAdd generate Stream(new Bundle {
          val rs1 = logic.result.RESULT()
          val rs2 = input.RS(2)()
          val NV = Bool()
          val cmd = input.CMD()
        })
        if(p.withAdd) {
          haltIt(!add.ready && input.CMD.opcode === FpuOpcode.FMA)
          add.valid := isValid && input.CMD.opcode === FpuOpcode.FMA
          add.rs1 := logic.result.RESULT
          add.rs2 := input.RS(2)
          add.NV := logic.result.NV
          add.cmd := input.CMD
        }
      }
    }

    val add = p.withAdd generate new Pipeline{
      val input = new FpuStage{
        val CMD = insert(mul.output.add.valid ? mul.output.add.cmd | frontend.dispatch(frontend.unpack.CMD))
        val RS1 = insert((mul.output.add.valid ? mul.output.add.rs1 | frontend.dispatch(frontend.unpack.rs(0).RS)).invert(CMD.arg(1)))
        val RS2 = insert((mul.output.add.valid ? mul.output.add.rs2 | frontend.dispatch(frontend.unpack.rs(1).RS)).invert(CMD.arg(0)))
        val NV  = insert(mul.output.add.valid && mul.output.add.NV)
        val hit = frontend.dispatch(frontend.unpack.CMD).opcode === FpuOpcode.ADD
        valid := frontend.dispatch.isValid && hit || mul.output.add.valid
        frontend.dispatch.haltIt(hit && (!isReady || mul.output.add.valid))
        mul.output.add.ready := isReady
        val RDN = insert( CMD.roundMode === FpuRoundMode.RDN)
      }

      val preShiftStage = new FpuStage(DIRECT())
      val shifterStage = new FpuStage(M2S())
      val mathStage = new FpuStage(M2S())
      val normStage = new FpuStage(M2S())
      val logicResultStage = new FpuStage(M2S())

      val logic : FpuAdd = new FpuAdd(
        pipeline      = this,
        rs1           = input.RS1,
        rs2           = input.RS2,
        roundDown     = input.RDN,
        preShiftStage = preShiftStage,
        shifterStage  = shifterStage,
        mathStage     = mathStage,
        normStage     = normStage,
        resultStage   = logicResultStage
      )


      val resultStage = new FpuStage(DIRECT()){
        val normalized = FloatUnpacked(
          exponentMax = logic.result.RESULT.exponentMax,
          exponentMin = logic.result.RESULT.exponentMin,
          mantissaWidth = p.mantissaWidth + 2
        )


        normalized.exponent := logic.result.RESULT.exponent
        normalized.mantissa := logic.result.RESULT.mantissa.rounded(RoundType.SCRAP)
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
        stream.NV        := logic.result.NV || input.NV
        stream.DZ        := False
        stream.canonical := True
        stream.rawIdle()
      }
    }

    val div = p.withDiv generate new Pipeline{
      val internalMantissaSize = p.mantissaWidth
      val dividerShift = if(p.rvd) 0 else 1
      val divider = FpuDiv(internalMantissaSize + dividerShift)

      val input = new FpuStage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS  = frontend.unpack.rs.take(2).map(rs => insert(frontend.dispatch(rs.RS)))
        val hit = CMD.opcode === FpuOpcode.DIV
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)

        val cmdSent = RegInit(False) setWhen(divider.io.input.fire) clearWhen(isReady || isRemoved)
        divider.io.input.valid := valid && !cmdSent
        divider.io.input.a := U(RS(0).mantissa.raw << dividerShift)
        divider.io.input.b := U(RS(1).mantissa.raw << dividerShift)

        haltWhen(!cmdSent && !divider.io.input.ready)
      }

      import input._

      val result = new FpuStage(M2S()){
        val dividerScrap = divider.io.output.remain =/= 0 || divider.io.output.result(0, dividerShift bits) =/= 0
        val dividerResult = (divider.io.output.result >> dividerShift) @@ U(dividerScrap).resized

        divider.io.output.ready := !isValid || isReady
        haltWhen(!divider.io.output.valid)

        val needShift = !dividerResult.msb
        val mantissa = needShift ? dividerResult(0, internalMantissaSize+2 bits) | (dividerResult(1, internalMantissaSize+2 bits) | U(dividerResult(0)).resized)
        val exponent = RS(0).exponent - RS(1).exponent - AFix(U(needShift))

        val merge = Stream(MergeInput(
          FloatUnpacked(
            exponentMax = exponent.maxRaw.toInt,
            exponentMin = exponent.minRaw.toInt,
            mantissaWidth = p.mantissaWidth+2
          )
        ))
        merge.valid := isValid && divider.io.output.valid
        merge.value.setNormal
        merge.value.quiet := False
        merge.value.sign := RS(0).sign ^ RS(1).sign
        merge.value.exponent := exponent
        merge.value.mantissa := mantissa
        merge.format    := CMD.format
        merge.roundMode := CMD.roundMode
        merge.robId     := CMD.robId
        merge.canonical      := True
        merge.rawIdle()
        haltWhen(!merge.ready)

        val forceOverflow = RS(0).isInfinity || RS(1).isZero
        val infinitynan = RS(0).isZero && RS(1).isZero || RS(0).isInfinity && RS(1).isInfinity
        val forceNan = RS(0).isNan || RS(1).isNan || infinitynan
        val forceZero = RS(0).isZero || RS(1).isInfinity

        merge.NV := False
        merge.DZ := !forceNan && !RS(0).isInfinity && RS(1).isZero

        when(forceNan) {
          merge.value.setNanQuiet
          merge.NV setWhen((infinitynan || RS(0).isNanSignaling || RS(1).isNanSignaling))
        } elsewhen(forceOverflow) {
          merge.value.setInfinity
        } elsewhen(forceZero) {
          merge.value.setZero
        }
      }
    }

    val sqrt = p.withSqrt generate new Pipeline{
      val internalMantissaSize = p.mantissaWidth
      val sqrt = FpuSqrt(internalMantissaSize)

      val input = new FpuStage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS  = insert(frontend.dispatch(frontend.unpack.rs(0).RS))
        val hit = CMD.opcode === FpuOpcode.SQRT
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)

        val cmdSent = RegInit(False) setWhen(sqrt.io.input.fire) clearWhen(isReady || isRemoved)
        sqrt.io.input.valid := valid && !cmdSent

        val needShift = RS.exponent.raw.lsb
        sqrt.io.input.a := U(needShift ? (B"1" ## RS.mantissa ## B"0") | (B"01" ## RS.mantissa))

        haltWhen(!cmdSent && !sqrt.io.input.ready)
      }

      import input._

      val result = new FpuStage(M2S()){

        sqrt.io.output.ready := !isValid || isReady
        haltWhen(!sqrt.io.output.valid)

        val EXP = insert((RS.exponent >>| 1))
        val scrap = sqrt.io.output.remain =/= 0

        val merge = Stream(MergeInput(
          FloatUnpacked(
            exponentMax = EXP.maxRaw.toInt,
            exponentMin = EXP.minRaw.toInt,
            mantissaWidth = p.mantissaWidth+2
          )
        ))
        merge.value.setNormal
        merge.valid          := isValid && sqrt.io.output.valid
        merge.value.quiet    := False
        merge.value.sign     := RS.sign
        merge.value.exponent := EXP
        merge.value.mantissa.raw := sqrt.io.output.result ## scrap
        merge.NV        := False
        merge.DZ        := False
        merge.format    := CMD.format
        merge.roundMode := CMD.roundMode
        merge.robId     := CMD.robId
        merge.canonical := True
        merge.rawIdle()

        val negative  = !RS.isNan && !RS.isZero && RS.sign
        when(RS.isInfinity){
          merge.value.setInfinity
        }
        when(negative){
          merge.value.setNanQuiet
          merge.NV := True
        }
        when(RS.isNan){
          merge.value.setNanQuiet
          merge.NV := !RS.quiet
        }
        when(RS.isZero){
          merge.value.setZero
        }

        haltWhen(!merge.ready)
      }
    }

    val shared = new Pipeline{
      val input = new FpuStage{
        val CMD     = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS1     = insert(frontend.dispatch(frontend.unpack.rs(0).RS))
        val RS2     = insert(frontend.dispatch(frontend.unpack.rs(1).RS))
        val RS1_RAW = insert(frontend.dispatch(frontend.arbiter.CMD).rs(0))
        val hit     = List(FpuOpcode.F2I, FpuOpcode.CMP, FpuOpcode.MIN_MAX, FpuOpcode.SGNJ, FpuOpcode.FCLASS, FpuOpcode.FCVT_X_X, FpuOpcode.FMV_X_W).map(CMD.opcode === _).orR
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)
        val toFpuRf = insert(List(FpuOpcode.MIN_MAX, FpuOpcode.SGNJ, FpuOpcode.FCVT_X_X).map(CMD.opcode === _).orR)

        val f2iShiftFull = insert(AFix(p.rsIntWidth-1) - RS1.exponent)
        val f2iShift     = insert(U(f2iShiftFull.raw).sat(widthOf(f2iShiftFull.raw) - log2Up(p.rsIntWidth)-1))

        val bothZero           = insert(RS1.isZero && RS2.isZero)
        val expEqual           = insert(RS1.exponent === RS2.exponent)
        val rs1Equal           = insert(RS1.sign === RS2.sign && expEqual && RS1.mantissa === RS2.mantissa)
        val rs1ExpSmaller      = insert(RS1.exponent < RS2.exponent)
        val rs1MantissaSmaller = insert(RS1.mantissa < RS2.mantissa)
      }
      import input._

      val math = new FpuStage(M2S()){
        val haltOneCycle = CMD.opcode === FpuOpcode.F2I
        val haltedOnce = RegNext(True) init(False) clearWhen(isRemoved || isReady || !isValid)
        val selfHalted = haltOneCycle && !haltedOnce
        haltWhen(selfHalted)

        val rs1AbsSmaller = rs1ExpSmaller || expEqual && rs1MantissaSmaller
        rs1AbsSmaller.setWhen(RS2.isInfinity)
        rs1AbsSmaller.setWhen(RS1.isZero)
        rs1AbsSmaller.clearWhen(RS2.isZero)
        rs1AbsSmaller.clearWhen(RS1.isInfinity)
        rs1Equal setWhen(RS1.sign === RS2.sign && RS1.isInfinity && RS2.isInfinity)
        val rs1Smaller = (RS1.sign ## RS2.sign).mux(
          0 -> rs1AbsSmaller,
          1 -> False,
          2 -> True,
          3 -> (!rs1AbsSmaller && !rs1Equal)
        )

        val minMaxSelectRs2 = !(((rs1Smaller ^ CMD.arg(0)) && !RS1.isNan || RS2.isNan))
        val minMaxSelectNanQuiet = RS1.isNan && RS2.isNan
        val cmpResult = B(rs1Smaller && !bothZero && !CMD.arg(1) || (rs1Equal || bothZero) && !CMD.arg(0))
        when(RS1.isNan || RS2.isNan) { cmpResult := 0 }

        val sgnjResult      = (RS1.sign && CMD.arg(1)) ^ RS2.sign ^ CMD.arg(0)
        val fclassResult    = B(0, 32 bits)
        val expSubnormal    = muxDouble[SInt](CMD.format)(-1023)(-127)
        val rs1SubnormalExp = RS1.isNormal && RS1.exponent <= AFix(expSubnormal)
        fclassResult(0) :=  RS1.sign && RS1.isInfinity
        fclassResult(1) :=  RS1.sign && RS1.isNormal && !rs1SubnormalExp
        fclassResult(2) :=  RS1.sign && RS1.isNormal &&  rs1SubnormalExp
        fclassResult(3) :=  RS1.sign && RS1.isZero
        fclassResult(4) := !RS1.sign && RS1.isZero
        fclassResult(5) := !RS1.sign && RS1.isNormal &&  rs1SubnormalExp
        fclassResult(6) := !RS1.sign && RS1.isNormal && !rs1SubnormalExp
        fclassResult(7) := !RS1.sign && RS1.isInfinity
        fclassResult(8) := RS1.isNan && !RS1.quiet
        fclassResult(9) := RS1.isNan &&  RS1.quiet


        val signalQuiet = CMD.opcode === FpuOpcode.CMP && CMD.arg =/= 2
        val rs1NanNv = RS1.isNan && (!RS1.quiet || signalQuiet)
        val rs2NanNv = RS2.isNan && (!RS2.quiet || signalQuiet)
        val nv = List(FpuOpcode.CMP, FpuOpcode.MIN_MAX, FpuOpcode.FCVT_X_X).map(CMD.opcode === _).orR && rs1NanNv ||
                 List(FpuOpcode.CMP, FpuOpcode.MIN_MAX).map(CMD.opcode === _).orR && rs2NanNv
        val nx = False

        val f2i = new Area{
          val shifterWidth = (p.rsIntWidth+2) max (p.mantissaWidth+2)
          val shifter      = RegNext(Shift.rightWithScrap(True ## RS1.mantissa.raw ## B(0, shifterWidth-1-p.mantissaWidth bits), f2iShift))
          val (high, low)  = shifter.splitAt(shifterWidth-p.rsIntWidth)
          val unsigned     = U(high)
          val round        = low.msb ## low.dropHigh(1).orR
          val resign       = CMD.arg(0) && RS1.sign
          val increment    = CMD.roundMode.mux(
            FpuRoundMode.RNE -> (round(1) && (round(0) || unsigned(0))),
            FpuRoundMode.RTZ -> False,
            FpuRoundMode.RDN -> (round =/= 0 &&  RS1.sign),
            FpuRoundMode.RUP -> (round =/= 0 && !RS1.sign),
            FpuRoundMode.RMM -> (round(1))
          )
          val resultRaw = (Mux(resign, ~unsigned, unsigned) + (resign ^ increment).asUInt)
          val expMax = (CMD.arg(1) ? AFix(62) | AFix(30)) + AFix(!CMD.arg(0))
          val expMin = (CMD.arg(1) ? AFix(63) | AFix(31))
          val unsignedMin = muxRv64[UInt](CMD.arg(1)) (BigInt(1) << 63) (BigInt(1) << 31)
          val overflow  = (RS1.exponent > expMax || RS1.isInfinity) && !RS1.sign || RS1.isNan
          val underflow = (RS1.exponent > expMin || CMD.arg(0) && RS1.exponent === expMin && (unsigned =/= unsignedMin || increment) || !CMD.arg(0) && (unsigned =/= 0 || increment) || RS1.isInfinity) && RS1.sign
          val isZero = RS1.isZero
          if(p.rvd){
            overflow setWhen(!CMD.arg(1) && !RS1.sign && increment && unsigned(30 downto 0).andR && (CMD.arg(0) || unsigned(31)))
          }
          when(isZero){
            resultRaw := 0
          } elsewhen(underflow || overflow) {
            val low = overflow
            val high = CMD.arg(0) ^ overflow
            resultRaw := (31 -> high, default -> low)
            if(p.rsIntWidth == 64) when(CMD.arg(1)){
              resultRaw := (63 -> high, default -> low)
            }
            nv setWhen(CMD.opcode === FpuOpcode.F2I && !isZero)
          } otherwise {
            nx setWhen(CMD.opcode === FpuOpcode.F2I && round =/= 0)
          }
          val result = CombInit(resultRaw)
          if(p.rsIntWidth == 64) when(!CMD.arg(1)){
            result(63 downto 32) := (default -> resultRaw(31))
          }
        }


        val f2x = new Area{
          val result = RS1_RAW.resize(p.rsIntWidth bits)
          if(p.rv64) when(CMD.format === FpuFormat.FLOAT) {
            result(63 downto 32) := (default -> RS1_RAW(31))
          }
        }

        val intResult = insert(B(0, p.rsIntWidth bits))
        switch(CMD.opcode){
          is(FpuOpcode.F2I)     { intResult := f2i.result.asBits }
          is(FpuOpcode.CMP)     { intResult := cmpResult.resized }
          is(FpuOpcode.FCLASS)  { intResult := fclassResult.resized }
          is(FpuOpcode.FMV_X_W) { intResult := f2x.result.asBits }
        }


        val intWb = Stream(FpuIntWriteBack(p.robIdWidth, p.rsIntWidth))
        intWb.valid := valid && !toFpuRf && !selfHalted
        intWb.flags.NX := nx
        intWb.flags.UF := False
        intWb.flags.OF := False
        intWb.flags.DZ := False
        intWb.flags.NV := nv
        intWb.value := intResult
        intWb.robId := input.CMD.robId

        val merge = Stream(MergeInput(
          FloatUnpacked(
            exponentMax = RS1.exponentMax,
            exponentMin = RS1.exponentMin,
            mantissaWidth = p.mantissaWidth+2
          )
        ))


        merge.valid := valid && toFpuRf && !selfHalted
        merge.NV := nv
        merge.DZ := False
        merge.format    := CMD.format
        merge.roundMode := CMD.roundMode
        merge.robId     := CMD.robId
        merge.canonical := CMD.opcode =/= FpuOpcode.SGNJ

        merge.value.quiet    := RS1.quiet
        merge.value.sign     := RS1.sign
        merge.value.exponent := RS1.exponent
        merge.value.mantissa := RS1.mantissa
        merge.value.mode     := RS1.mode
        merge.rawIdle()

        switch(CMD.opcode){
          is(FpuOpcode.MIN_MAX){
            merge.value.quiet   clearWhen(!RS2.quiet)
            when(minMaxSelectRs2) {
              merge.value.sign     := RS2.sign
              merge.value.exponent := RS2.exponent
              merge.value.mantissa := RS2.mantissa
              merge.value.mode     := RS2.mode
            }
            when(minMaxSelectNanQuiet){
              merge.value.setNanQuiet
            }
          }
          is(FpuOpcode.SGNJ){
            merge.value.quiet   clearWhen(!RS2.quiet)
            merge.value.sign := sgnjResult
          }
          if(p.rvd) is(FpuOpcode.FCVT_X_X){
            merge.format := ((CMD.format === FpuFormat.FLOAT) ? FpuFormat.DOUBLE | FpuFormat.FLOAT)
            when(RS1.isNan){
              merge.value.setNanQuiet
            }
          }
        }


        haltIt(intWb.isStall || merge.isStall)
      }
      import math._
    }


    val i2f = new Pipeline{
      assert(p.portCount == 1)
      val input = new FpuStage {
        val intCmd = io.ports(0).intCmd
        valid := intCmd.valid
        intCmd.ready := isReady


        val ARGS = insert(intCmd.payload)

        val rs1Msb    = insert(if(!p.rv64) ARGS.rs1(31) else (ARGS.arg(1) ? ARGS.rs1(63) | ARGS.rs1(31)))
        val rs1Neg    = insert(ARGS.arg(0) && rs1Msb)
        val rs1Masked = insert(CombInit(ARGS.rs1))
        if(p.rv64) when(!ARGS.arg(1)) { rs1Masked(63 downto 32) := (default -> this(rs1Neg)) }
        val rs1Unsigned = insert(Mux[Bits](rs1Neg, ~rs1Masked, rs1Masked).asUInt + U(rs1Neg))
        val rs1Zero     = insert(rs1Masked === 0)
      }
      import input._

      val logic = new FpuStage(M2S()){
        val fsmPortId = 1
        val fsmCmd = unpacker.arbiter.io.inputs(fsmPortId)
        val fsmRsp = unpacker.results(fsmPortId)
        val asked    = RegInit(False) setWhen(fsmCmd.ready) clearWhen(!isStuck || isRemoved)
        val served   = RegInit(False) setWhen(fsmRsp.valid) clearWhen(!isStuck || isRemoved)
        val fsmResult = fsmRsp.toReg

        fsmCmd.valid := isValid && ARGS.opcode === FpuOpcode.I2F && !asked
        fsmCmd.data := rs1Unsigned.asBits.resized

        haltWhen(ARGS.opcode === FpuOpcode.I2F && !served)
        val merge = Stream(MergeInput(
          FloatUnpacked(
            exponentMax = p.rsIntWidth,
            exponentMin = 0,
            mantissaWidth = p.mantissaWidth+2
          )
        ))

        merge.valid     := isValid && !(ARGS.opcode === FpuOpcode.I2F && !served)
        merge.NV        := False
        merge.DZ        := False
        merge.format    := ARGS.format
        merge.roundMode := ARGS.roundMode
        merge.robId     := ARGS.robId
        merge.canonical := ARGS.opcode =/= FpuOpcode.SGNJ

        merge.value.quiet    := False
        merge.value.sign     := rs1Neg
        merge.value.exponent := unpacker.ohInputWidth - fsmResult.shift
        if(widthOf(fsmResult.data) > widthOf(merge.value.mantissa.raw)) {
          merge.value.mantissa.raw := fsmResult.data.takeHigh(p.mantissaWidth + 1) ## fsmResult.data.dropHigh(p.mantissaWidth + 1).orR
        } else {
          merge.value.mantissa.raw := fsmResult.data << widthOf(merge.value.mantissa.raw) - widthOf(fsmResult.data)
        }
        merge.value.setNormal
        merge.rawEnable      := ARGS.opcode === FpuOpcode.FMV_W_X
        merge.raw            := ARGS.rs1.resized
        when(rs1Zero){
          merge.value.setZero
        }
        haltWhen(!merge.ready)
      }
    }

    val backend = new Pipeline{
      val merge = new FpuStage{
        val inputs = ArrayBuffer[Stream[MergeInput]]()
        if(p.withMul)  inputs += mul.output.merge
        if(p.withAdd)  inputs += add.resultStage.stream
        if(p.withDiv)  inputs += div.result.merge
        if(p.withSqrt) inputs += sqrt.result.merge
        inputs += shared.math.merge
        inputs += i2f.logic.merge
        val exponentMin = inputs.map(_.value.exponentMin).min
        val exponentMax = inputs.map(_.value.exponentMax).max
        val remapped    = inputs.map{e =>
          val v = Stream(MergeInput(FloatUnpacked(
            exponentMax   = exponentMax,
            exponentMin   = exponentMin,
            mantissaWidth = p.mantissaWidth+2
          )))
          v << e
          v
        }

        val arbitrated = StreamArbiterFactory.lowerFirst.noLock.on(remapped)
        val VALUE      = insert(arbitrated.value)
        val FORMAT     = insert(arbitrated.format)
        val ROUNDMODE  = insert(arbitrated.roundMode)
        val ROBID      = insert(arbitrated.robId)
        val NV         = insert(arbitrated.NV)
        val DZ         = insert(arbitrated.DZ)
        val RAW        = insert(arbitrated.raw)
        val RAW_ENABLE = insert(arbitrated.rawEnable)
        val CANONICAL  = insert(arbitrated.canonical)
        valid := arbitrated.valid
        arbitrated.ready := isReady

        val EXP_SUBNORMAL    = insert(AFix(muxDouble[SInt](FORMAT)(-1023)(-127)))
        val EXP_DIF          = insert(EXP_SUBNORMAL - VALUE.exponent)
        val EXP_DIF_PLUS_ONE = insert(U(EXP_SUBNORMAL - VALUE.exponent) + 1)
      }
      import merge._


      val s1 = new FpuStage(M2S()) {
        val SUBNORMAL        = insert(EXP_DIF.isPositive() && VALUE.isNormal)
        val MAN_SHIFT_NO_SAT = insert(!SUBNORMAL ?[UInt] 0 | EXP_DIF_PLUS_ONE)
        val MAN_SHIFT        = insert(MAN_SHIFT_NO_SAT.sat(widthOf(MAN_SHIFT_NO_SAT) - log2Up(p.mantissaWidth + 2)))
        val MAN_SHIFTED      = insert(U(Shift.rightWithScrap(True ## merge.VALUE.mantissa.raw, MAN_SHIFT).dropHigh(1)))

        val f32ManPos        = p.mantissaWidth + 2 - 23
        val roundAdjusted    = insert(muxDouble(merge.FORMAT)(MAN_SHIFTED(0, 2 bits))(MAN_SHIFTED(f32ManPos - 2, 2 bits) | U(MAN_SHIFTED(f32ManPos - 2 - 1 downto 0).orR, 2 bits)))
        val manLsb           = insert(muxDouble(merge.FORMAT)(MAN_SHIFTED(2))(MAN_SHIFTED(f32ManPos)))
      }
      import s1._

      val s2 = new FpuStage(M2S()) {
        val ROUNDING_INCR    = insert(VALUE.isNormal && ROUNDMODE.mux(
          FpuRoundMode.RNE -> (roundAdjusted(1) && (roundAdjusted(0) || manLsb)),
          FpuRoundMode.RTZ -> False,
          FpuRoundMode.RDN -> (roundAdjusted =/= 0 && VALUE.sign),
          FpuRoundMode.RUP -> (roundAdjusted =/= 0 && !VALUE.sign),
          FpuRoundMode.RMM -> (roundAdjusted(1))
        ))
        val incrBy           = muxDouble(merge.FORMAT)(U(ROUNDING_INCR))(U(ROUNDING_INCR) << p.mantissaWidth - 23)
        val manIncrWithCarry = (MAN_SHIFTED >> 2) +^ incrBy
        val MAN_CARRY        = manIncrWithCarry.msb
        val MAN_INCR         = (manIncrWithCarry.dropHigh(1))
        val EXP_INCR         = insert(merge.VALUE.exponent + AFix(U(MAN_CARRY)))
        val EXP_MAX          = insert(AFix(muxDouble[SInt](merge.FORMAT)(1023)(127)))
        val EXP_MIN          = insert(AFix(muxDouble[SInt](merge.FORMAT)(-1023 - 52 + 1)(-127 - 23 + 1)))
        val EXP_OVERFLOW     = insert(EXP_INCR > EXP_MAX)
        val EXP_UNDERFLOW    = insert(EXP_INCR < EXP_MIN)
        val MAN_RESULT       = insert(MAN_INCR)
      }
      import s2._

      val s3 = new FpuStage(M2S()){
        val SUBNORMAL_FINAL = insert((EXP_SUBNORMAL - EXP_INCR).isPositive())
        val EXP = insert(!SUBNORMAL_FINAL ? (EXP_INCR - EXP_SUBNORMAL) | AFix(0))

        val mr = merge.VALUE.mantissa.raw
        val tinyRound = muxDouble(FORMAT) {mr.dropHigh(52).msb ## mr.dropHigh(53).orR} {mr.dropHigh(23).msb ## mr.dropHigh(24).orR}

        val tinyRoundingIncr = VALUE.isNormal && ROUNDMODE.mux(
          FpuRoundMode.RNE -> (tinyRound(1) && (tinyRound(0) || manLsb)),
          FpuRoundMode.RTZ -> False,
          FpuRoundMode.RDN -> (tinyRound =/= 0 &&  VALUE.sign),
          FpuRoundMode.RUP -> (tinyRound =/= 0 && !VALUE.sign),
          FpuRoundMode.RMM -> (tinyRound(1))
        )
        val tinyOverflow = muxDouble(FORMAT) {merge.VALUE.mantissa.raw.takeHigh(52).andR} {merge.VALUE.mantissa.raw.takeHigh(23).andR} && tinyRoundingIncr

        val expSet, expZero, expMax, manZero, manSet, manOne, manQuiet, positive = False
        val nx, of, uf = False
        switch(merge.VALUE.mode){
          is(FloatMode.ZERO){
            expZero := True
            manZero := True
          }
          is(FloatMode.INF){
            expSet := True
            manZero := True
          }
          is(FloatMode.NAN){
            expSet := True
            when(CANONICAL) {
              positive := True
              manZero := True
              manQuiet := merge.VALUE.quiet
            }
          }
          is(FloatMode.NORMAL){
            when(roundAdjusted =/= 0){
              nx := True
              when(SUBNORMAL_FINAL || SUBNORMAL && !tinyOverflow){
                uf := True
              }
            }
            when(EXP_OVERFLOW){
              nx := True
              of := True
              val doMax = merge.ROUNDMODE.mux(
                FpuRoundMode.RNE -> (False),
                FpuRoundMode.RTZ -> (True),
                FpuRoundMode.RDN -> (!merge.VALUE.sign),
                FpuRoundMode.RUP -> (merge.VALUE.sign),
                FpuRoundMode.RMM -> (False)
              )
              when(doMax){
                expMax := True
                manSet := True
              } otherwise {
                expSet := True
                manZero := True
              }
            }.elsewhen(EXP_UNDERFLOW){
              nx := True
              uf := True
              val doMin = merge.ROUNDMODE.mux(
                FpuRoundMode.RNE -> (False),
                FpuRoundMode.RTZ -> (False),
                FpuRoundMode.RDN -> (merge.VALUE.sign),
                FpuRoundMode.RUP -> (!merge.VALUE.sign),
                FpuRoundMode.RMM -> (False)
              ) || ROUNDING_INCR
              when(doMin){
                expZero := True
                manOne := True
              } otherwise {
                expZero := True
                manZero := True
              }
            }
          }
        }

        val fwb = io.ports(0).floatWriteback
        fwb.valid := isFireing
        fwb.robId := merge.ROBID

        fwb.flags.NX := nx
        fwb.flags.UF := uf
        fwb.flags.OF := of
        fwb.flags.DZ := DZ
        fwb.flags.NV := NV

        whenDouble(FORMAT){
          fwb.value := merge.VALUE.sign ## EXP.raw.resize(11 bits) ## MAN_RESULT
        }{
          fwb.value(31 downto 0) := merge.VALUE.sign ## EXP.raw.takeLow(8) ## MAN_RESULT.takeHigh(23)
          if(p.rvd) fwb.value(63 downto 32).setAll()
        }

        val wb = fwb.value
        when(expZero) {
          whenDouble(FORMAT) (wb(52, 11 bits).clearAll()) (wb(23, 8 bits).clearAll())
        }
        when(expSet) {
          whenDouble(FORMAT) (wb(52, 11 bits).setAll()) (wb(23, 8 bits).setAll())
        }
        when(expMax) {
          whenDouble(FORMAT) (wb(52, 11 bits) := 0x7FE) (wb(23, 8 bits) := 0xFE)
        }
        when(manZero) {
          whenDouble(FORMAT) (wb(0, 52 bits).clearAll()) (wb(0, 23 bits).clearAll())
        }
        when(manOne) {
          whenDouble(FORMAT) (wb(0, 52 bits) := 1) (wb(0, 23 bits) := 1)
        }
        when(manSet) {
          whenDouble(FORMAT) (wb(0, 52 bits).setAll()) (wb(0, 23 bits).setAll())
        }
        when(manQuiet) {
          whenDouble(FORMAT) (wb(51) := True) (wb(22) := True)
        }
        when(positive){
          whenDouble(FORMAT) (wb(63) := False) (wb(31) := False)
        }
        when(RAW_ENABLE){
          wb := RAW
          fwb.flags.clear()
        }
        if(p.rvd) when(FORMAT === FpuFormat.FLOAT){
          wb(63 downto 32).setAll()
        }
      }
    }
  }

  val int = new Area{
    io.ports(0).intWriteback << flt.shared.math.intWb
  }



  flt.frontend.build()
  if(p.withMul) flt.mul.build()
  if(p.withAdd) flt.add.build()
  if(p.withDiv) flt.div.build()
  if(p.withSqrt) flt.sqrt.build()
  flt.shared.build()
  flt.i2f.build()
  unpacker.build()
  flt.backend.build()
}


