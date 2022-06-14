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
    val doThrow = io.ports(0).unschedule
    throwIt(doThrow)

    def this(connection: ConnectionLogic)(implicit _pip: Pipeline)  {
      this()
      chainConnect(connection)
    }
  }


  //Subnormal/Int to Float logic
  val unpackFsm = new StateMachine{
    val ohInputWidth = p.rsIntWidth max p.mantissaWidth
    case class Request() extends Bundle {
      val i2f, signed = Bool()
      val data = Bits(ohInputWidth bits)
    }
    case class Result() extends Bundle {
      val shift = UInt(log2Up(ohInputWidth) bits)
      val data = Bits(ohInputWidth bits)
    }

    val portCount = 1
    val args = Reg(Request())
    val source = Reg(UInt(log2Up(portCount) bits))
    val kill = Bits(portCount bits)
    val killed = RegInit(False) setWhen(kill(source))

    val IDLE, INVERT, SHIFT = new State
    setEntry(IDLE)

    val arbiter = StreamArbiterFactory().noLock.lowerFirst.build(Request(), portCount)

    arbiter.io.output.ready := False
    IDLE whenIsActive{
      when(arbiter.io.output.valid){
        arbiter.io.output.ready := True
        args := arbiter.io.output.payload
        source := arbiter.io.chosen
        killed := kill(arbiter.io.chosen)
        goto(SHIFT)
        when(arbiter.io.output.i2f && arbiter.io.output.signed && arbiter.io.output.data.msb){
          goto(INVERT)
        }
      }
    }


    val shiftBy = OHToUInt(OHMasking.firstV2(args.data.reversed |<< 1))
    val shifter = args.data |<< shiftBy

    val results = Vec.fill(portCount)(Flow(Result()))
    for((port, id) <- results.zipWithIndex){
      port.valid := False
      port.data := shifter
      port.shift := shiftBy
    }
    SHIFT whenIsActive{
      when(!killed) {
        results(source).valid := True
      }
      goto(IDLE)
    }



//    val i2fZero = Reg(Bool)

//    val shift = new Area{
//      val by = Reg(UInt(log2Up(ohInputWidth) bits))
//      val input = UInt(ohInputWidth bits).assignDontCare()
//      var logic = input
//      for(i <- by.range){
//        logic \= by(i) ? (logic |<< (BigInt(1) << i)) | logic
//      }
//      val output = RegNextWhen(logic, !done)
//    }
//    shift.input := (ohInput.asUInt |<< 1).resized

//    when(input.valid && (input.i2f || isSubnormal) && !done){
//      busy := True
//      when(boot){
//        when(input.i2f && !patched && input.value(p.rsIntWidth-1) && input.arg(0)){
//          input.value.getDrivingReg(0, p.rsIntWidth bits) := B(input.value.asUInt.twoComplement(True).resize(p.rsIntWidth bits))
//          patched := True
//        } otherwise {
//          shift.by := OHToUInt(OHMasking.first((ohInput).reversed))
//          boot := False
//          i2fZero := input.value(p.rsIntWidth-1 downto 0) === 0
//        }
//      } otherwise {
//        done := True
//      }
//    }
//
//    val expOffset = (UInt(p.internalExponentSize bits))
//    expOffset := 0
//    when(isSubnormal){
//      expOffset := shift.by.resized
//    }
//
//    when(!input.isStall){
//      done := False
//      boot := True
//      patched := False
//    }
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

        val fsmPortId = 0
        val fsmCmd = unpackFsm.arbiter.io.inputs(fsmPortId)
        val fsmRsp = unpackFsm.results(fsmPortId)
        fsmCmd.setIdle()
        unpackFsm.kill(fsmPortId) := isRemoved

        val fsmRequesters = Bits(3 bits)
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
            val valid = CombInit(isSubnormal)
            val served = RegInit(False) setWhen(fsmRsp.valid && !fsmRequesters.dropLow(inputId + 1).orR) clearWhen(!isStuck || isRemoved)
            val exponent = Reg(RS.exponent)
            val mantissa = Reg(RS.mantissa)
            fsmRequesters(inputId) := valid && !served

            when(fsmRequesters(inputId)) {
              fsmCmd.valid := True
              fsmCmd.data := RS_PRE_NORM.mantissa.raw << widthOf(fsmCmd.data) - widthOf(RS_PRE_NORM.mantissa.raw)
              fsmCmd.i2f := False
              fsmCmd.signed := False
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
        }
      }

      val dispatch = new FpuStage{
        connect(unpack, this)(DIRECT())
      }
    }

    case class MergeInput(valueType : HardType[FloatUnpacked]) extends Bundle{
      val value = valueType()
      val format = FpuFormat()
      val roundMode = FpuRoundMode()
      val robId = UInt(p.robIdWidth bits)
      val NV = Bool()
      val DZ = Bool()
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
      val sum2 = new FpuStage(DIRECT())
      val sum3 = new FpuStage(DIRECT())
      val norm = new FpuStage(DIRECT())
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
        merge.valid     := isValid && input.CMD.opcode === FpuOpcode.MUL
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
        val RS1 = insert(mul.output.add.valid ? mul.output.add.rs1 | frontend.dispatch(frontend.unpack.rs(0).RS))
        val RS2 = insert(mul.output.add.valid ? mul.output.add.rs2 | frontend.dispatch(frontend.unpack.rs(1).RS).invert(frontend.dispatch(frontend.unpack.CMD).arg(0)))
        val NV = mul.output.add.valid && mul.output.add.NV
        val hit = CMD.opcode === FpuOpcode.ADD
        valid := frontend.dispatch.isValid && hit || mul.output.add.valid
        frontend.dispatch.haltIt(hit && (!isReady || mul.output.add.valid))
        mul.output.add.ready := isReady
        val RDN = insert( CMD.roundMode === FpuRoundMode.RDN)
      }

      val preShiftStage = new FpuStage(DIRECT())
      val shifterStage = new FpuStage(DIRECT())
      val mathStage = new FpuStage(DIRECT())
      val normStage = new FpuStage(DIRECT())
      val logicResultStage = new FpuStage(DIRECT())

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
        merge.valid := isValid && sqrt.io.output.valid
        merge.value.setNormal
        merge.value.quiet := False
        merge.value.sign := RS.sign
        merge.value.exponent := EXP
        merge.value.mantissa.raw := sqrt.io.output.result ## scrap
        merge.NV := False
        merge.DZ := False
        merge.format    := CMD.format
        merge.roundMode := CMD.roundMode
        merge.robId     := CMD.robId

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

    val f2x = new Pipeline{
      val input = new FpuStage{
        val CMD = insert(frontend.unpack(frontend.unpack.CMD))
        val RS  = insert(frontend.unpack(frontend.arbiter.CMD).rs(0))
        val hit = CMD.opcode === FpuOpcode.FMV_X_W
        valid := frontend.unpack.isValid && hit
        frontend.unpack.haltIt(hit && !isReady)
      }

      val result = new FpuStage(M2S()){
        val wb = Stream(FpuIntWriteBack(p.robIdWidth, p.rsIntWidth))
        wb.valid := valid
        wb.flags.clear()
        wb.value := input.RS
        wb.robId := input.CMD.robId

        haltIt(!wb.ready)
      }
    }

    val shared = new Pipeline{
      val input = new FpuStage{
        val CMD = insert(frontend.dispatch(frontend.unpack.CMD))
        val RS1  = insert(frontend.dispatch(frontend.unpack.rs(0).RS))
        val RS2  = insert(frontend.dispatch(frontend.unpack.rs(1).RS))
        val hit =  List(FpuOpcode.F2I, FpuOpcode.CMP, FpuOpcode.MIN_MAX, FpuOpcode.SGNJ, FpuOpcode.FCLASS, FpuOpcode.FCVT_X_X).map(CMD.opcode === _).orR
        valid := frontend.dispatch.isValid && hit
        frontend.dispatch.haltIt(hit && !isReady)

        val toFpuRf = insert(List(FpuOpcode.MIN_MAX, FpuOpcode.SGNJ, FpuOpcode.FCVT_X_X).map(CMD.opcode === _).orR)
      }
      import input._

      val math = new FpuStage(DIRECT()){


        val bothZero = RS1.isZero && RS2.isZero
        val expEqual = RS1.exponent === RS2.exponent
        val rs1Equal = RS1.sign === RS2.sign && expEqual && RS1.mantissa === RS2.mantissa
        val rs1ExpSmaller = RS1.exponent < RS2.exponent
        val rs1MantissaSmaller = RS1.mantissa < RS2.mantissa
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
        val sgnjRs1Sign = CombInit(RS1.sign)
        val sgnjRs2Sign = CombInit(RS2.sign)
//        if(p.rvd){
//          sgnjRs2Sign setWhen(RS2Boxed && input.format === FpuFormat.DOUBLE)
//        }
        val sgnjResult = (sgnjRs1Sign && CMD.arg(1)) ^ sgnjRs2Sign ^ CMD.arg(0)
        val fclassResult = B(0, 32 bits)
        val expSubnormal = muxDouble[SInt](CMD.format)(-1023)(-127)
        val rs1Subnormal = RS1.isNormal && RS1.exponent <= AFix(expSubnormal)
        fclassResult(0) :=  RS1.sign && RS1.isInfinity
        fclassResult(1) :=  RS1.sign && RS1.isNormal
        fclassResult(2) :=  RS1.sign && rs1Subnormal
        fclassResult(3) :=  RS1.sign && RS1.isZero
        fclassResult(4) := !RS1.sign && RS1.isZero
        fclassResult(5) := !RS1.sign && rs1Subnormal
        fclassResult(6) := !RS1.sign && RS1.isNormal
        fclassResult(7) := !RS1.sign && RS1.isInfinity
        fclassResult(8) := RS1.isNan && !RS1.quiet
        fclassResult(9) := RS1.isNan &&  RS1.quiet

        val intResult = insert(B(0, p.rsIntWidth bits))
        switch(CMD.opcode){
//          is(FpuOpcode.F2I)     { intResult := f2i.result.asBits }
          is(FpuOpcode.CMP)     { intResult := cmpResult.resized }
          is(FpuOpcode.FCLASS)  { intResult := fclassResult.resized }
        }

        val wb = Stream(FpuIntWriteBack(p.robIdWidth, p.rsIntWidth))
        wb.valid := valid && !toFpuRf
        wb.flags.clear()
        wb.value := intResult
        wb.robId := input.CMD.robId
        
        val merge = Stream(MergeInput(
          FloatUnpacked(
            exponentMax = RS1.exponentMax,
            exponentMin = RS1.exponentMin,
            mantissaWidth = p.mantissaWidth+2
          )
        ))

        merge.valid := input.valid && toFpuRf
        merge.NV := False
        merge.DZ := False
        merge.format    := CMD.format
        merge.roundMode := CMD.roundMode
        merge.robId     := CMD.robId

        merge.value.quiet    := RS1.quiet
        merge.value.sign     := RS1.sign
        merge.value.exponent := RS1.exponent
        merge.value.mantissa := RS1.mantissa
        merge.value.mode     := RS1.mode

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
            when(!RS1.isNan) {
              merge.value.sign := sgnjResult
            }
//            if(p.withDouble) when(input.rs1Boxed && input.format === FpuFormat.DOUBLE){
//              merge.value.sign := input.rs1.sign
//              merge.format := FpuFormat.FLOAT
//            }
          }
          if(p.rvd) is(FpuOpcode.FCVT_X_X){
            merge.format := ((CMD.format === FpuFormat.FLOAT) ? FpuFormat.DOUBLE | FpuFormat.FLOAT)
            when(RS1.isNan){
              merge.value.setNanQuiet
            }
          }
        }


        haltIt(wb.ready || merge.isStall)
      }
      import math._
    }

//    val cmp = new Pipeline{
//      val input = new FpuStage{
//        val CMD = insert(frontend.unpack(frontend.unpack.CMD))
//        val RS  = frontend.unpack.rs.take(2).map(rs => insert(frontend.dispatch(rs.RS)))
//        val hit = CMD.opcode === FpuOpcode.CMP || CMD.opcode === FpuOpcode.MIN_MAX
//        valid := frontend.unpack.isValid && hit
//        frontend.unpack.haltIt(hit && !isReady)
//      }
//
//      import input._
//
//      val math = new FpuStage(DIRECT()){
//
//      }
//      import math._
//
//      val result = new FpuStage(DIRECT()){
//        val wb = Stream(FpuIntWriteBack(p.robIdWidth, p.rsIntWidth))
//        wb.valid := valid && CMD.opcode === FpuOpcode.CMP
//        wb.flags.clear()
//        wb.value := input.RS
//        wb.robId := input.CMD.robId
//
//        val merge = Stream(MergeInput(
//          FloatUnpacked(
//            exponentMax = EXP.maxRaw.toInt,
//            exponentMin = EXP.minRaw.toInt,
//            mantissaWidth = p.mantissaWidth+2
//          )
//        ))
//
//        haltIt(!wb.ready)
//      }
//    }

    val backend = new Pipeline{
      val merge = new FpuStage{
        val inputs = ArrayBuffer[Stream[MergeInput]]()
        if(p.withMul) inputs += mul.output.merge
        if(p.withAdd) inputs += add.resultStage.stream
        if(p.withDiv) inputs += div.result.merge
        if(p.withSqrt) inputs += sqrt.result.merge
        inputs += shared.math.merge
        val exponentMin = inputs.map(_.value.exponentMin).min
        val exponentMax = inputs.map(_.value.exponentMax).max
        val remapped = inputs.map{e =>
          val v = Stream(MergeInput(FloatUnpacked(
            exponentMax = exponentMax,
            exponentMin = exponentMin,
            mantissaWidth = p.mantissaWidth+2
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
      import merge._


      val logic = new FpuStage(DIRECT()){
        val EXP_SUBNORMAL = insert(AFix(muxDouble[SInt](merge.FORMAT)(-1023)(-127)))
        val EXP_DIF = insert(EXP_SUBNORMAL - merge.VALUE.exponent)
        val SUBNORMAL = insert(EXP_DIF.isPositive())
        val MAN_SHIFT_NO_SAT = insert(!SUBNORMAL ?[UInt] 0 | (U(EXP_DIF)+1))
        val MAN_SHIFT = insert(MAN_SHIFT_NO_SAT.sat(widthOf(MAN_SHIFT_NO_SAT) - log2Up(p.mantissaWidth+2)))
        val MAN_SHIFTED = insert(U(Shift.rightWithScrap(True ## merge.VALUE.mantissa.raw, MAN_SHIFT).dropHigh(1)))

        val f32ManPos = p.mantissaWidth-23
        val roundAdjusted = muxDouble(merge.FORMAT)(MAN_SHIFTED(0, 2 bits))(MAN_SHIFTED(f32ManPos-2, 2 bits) | U(MAN_SHIFTED(f32ManPos-2-1 downto 0).orR, 2 bits))
        val manLsb = muxDouble(merge.FORMAT)(MAN_SHIFTED(2))(MAN_SHIFTED(f32ManPos))
        val ROUNDING_INCR = VALUE.isNormal && ROUNDMODE.mux(
          FpuRoundMode.RNE -> (roundAdjusted(1) && (roundAdjusted(0) || manLsb)),
          FpuRoundMode.RTZ -> False,
          FpuRoundMode.RDN -> (roundAdjusted =/= 0 && VALUE.sign),
          FpuRoundMode.RUP -> (roundAdjusted =/= 0 && !VALUE.sign),
          FpuRoundMode.RMM -> (roundAdjusted(1))
        )

        val incrBy = muxDouble(merge.FORMAT)(U(ROUNDING_INCR))(U(ROUNDING_INCR) << p.mantissaWidth-23)
        val manIncrWithCarry = (MAN_SHIFTED >> 2) +^ U(ROUNDING_INCR)
        val MAN_CARRY = manIncrWithCarry.msb
        val MAN_INCR = (manIncrWithCarry.dropHigh(1))
        val EXP_INCR = merge.VALUE.exponent + AFix(U(MAN_CARRY))
        val EXP_MAX = insert(AFix(muxDouble[SInt](merge.FORMAT)(1023)(127)))
        val EXP_MIN = insert(AFix(muxDouble[SInt](merge.FORMAT)(-1023-52+1)(-127-23+1)))
        val EXP_OVERFLOW = insert(EXP_INCR > EXP_MAX)
        val EXP_UNDERFLOW = insert(EXP_INCR < EXP_MIN)
        val MAN_RESULT = insert(MAN_INCR)



        val EXP = insert(!(EXP_SUBNORMAL - EXP_INCR).isPositive() ? (EXP_INCR-EXP_SUBNORMAL) | AFix(0))



        val expSet, expZero, expMax, manZero, manSet, manOne, manQuiet, positive = False
        val nx, of, uf = False //TODO
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
            positive := True
            expSet := True
            manZero := True
            manQuiet := merge.VALUE.quiet
          }
          is(FloatMode.NORMAL){
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

        io.ports(0).floatCompletion.valid := isFireing
        io.ports(0).floatCompletion.flags := FpuFlags().getZero
        io.ports(0).floatCompletion.robId := merge.ROBID
        io.ports(0).floatCompletion.value := merge.VALUE.sign ## EXP.raw.resize(11 bits) ## MAN_RESULT

        assert(!valid || merge.FORMAT === FpuFormat.DOUBLE)
        when(expZero) {
          io.ports(0).floatCompletion.value(52, 11 bits).clearAll()
        }
        when(expSet) {
          io.ports(0).floatCompletion.value(52, 11 bits).setAll()
        }
        when(expMax) {
          io.ports(0).floatCompletion.value(52, 11 bits) := 0x7FE
        }
        when(manZero) {
          io.ports(0).floatCompletion.value(0, 52 bits).clearAll()
        }
        when(manOne) {
          io.ports(0).floatCompletion.value(0, 52 bits) := 1
        }
        when(manSet) {
          io.ports(0).floatCompletion.value(0, 52 bits).setAll()
        }
        when(manQuiet) {
          io.ports(0).floatCompletion.value(51) := True
        }
        when(positive){
          io.ports(0).floatCompletion.value(63) := False
        }
      }
    }
  }

  val int = new Area{
    val backend = new Pipeline {
      val inputs = ArrayBuffer[Stream[FpuIntWriteBack]]()
      inputs += flt.f2x.result.wb
      inputs += flt.shared.math.wb
      io.ports(0).intWriteback << StreamArbiterFactory.lowerFirst.noLock.on(inputs)
    }
  }



  flt.frontend.build()
  if(p.withMul) flt.mul.build()
  if(p.withAdd) flt.add.build()
  if(p.withDiv) flt.div.build()
  if(p.withSqrt) flt.sqrt.build()
  flt.shared.build()
  flt.f2x.build()
  flt.backend.build()
}


