package naxriscv.riscv
import naxriscv.Global
import spinal.core._

object Const {
  def funct7Range = 31 downto 25
  def rdRange = 11 downto 7
  def funct3Range = 14 downto 12
  def rs2Range = 24 downto 20
  def rs1Range = 19 downto 15
  def rs3Range = 31 downto 27
  def csrRange = 31 downto 20
  def rsRange(id : Int) = List(rs1Range, rs2Range,rs3Range)(id)
}

case class IMM(instruction  : Bits) extends Area{
  // immediates
  def i = instruction(31 downto 20)
  def h = instruction(31 downto 24)
  def s = instruction(31 downto 25) ## instruction(11 downto 7)
  def b = instruction(31) ## instruction(7) ## instruction(30 downto 25) ## instruction(11 downto 8)
  def u = instruction(31 downto 12) ## U"x000"
  def j = instruction(31) ## instruction(19 downto 12) ## instruction(20) ## instruction(30 downto 21)
  def z = instruction(19 downto 15)

  // sign-extend immediates
  def i_sext = S(B((19 downto 0) -> i(11)) ## i)
  def h_sext = S(B((23 downto 0) -> h(7))  ## h)
  def s_sext = S(B((19 downto 0) -> s(11)) ## s)
  def b_sext = S(B((18 downto 0) -> b(11)) ## b ## False)
  def j_sext = S(B((10 downto 0) -> j(19)) ## j ## False)

  assert(Global.XLEN.get == 32)
}


object CSR {
  val MCAUSE_ENUM = new {
    val STORE_PAGE_FAULT = 15
    val STORE_MISALIGNED = 6
    val STORE_ACCESS_FAULT = 7

    val LOAD_PAGE_FAULT = 13
    val LOAD_MISALIGNED = 4
    val LOAD_ACCESS_FAULT = 5

    val ILLEGAL_INSTRUCTION = 2
    val BREAKPOINT = 3
    val ECALL_USER = 8
    val ECALL_SUPERVISOR = 9
    val ECALL_MACHINE = 11
  }

  def MVENDORID = 0xF11 // MRO Vendor ID.
  def MARCHID   = 0xF12 // MRO Architecture ID.
  def MIMPID    = 0xF13 // MRO Implementation ID.
  def MHARTID   = 0xF14 // MRO Hardware thread ID.Machine Trap Setup
  def MSTATUS   = 0x300 // MRW Machine status register.
  def MISA      = 0x301 // MRW ISA and extensions
  def MEDELEG   = 0x302 // MRW Machine exception delegation register.
  def MIDELEG   = 0x303 // MRW Machine interrupt delegation register.
  def MIE       = 0x304 // MRW Machine interrupt-enable register.
  def MTVEC     = 0x305 // MRW Machine trap-handler base address. Machine Trap Handling
  def MSCRATCH  = 0x340 // MRW Scratch register for machine trap handlers.
  def MEPC      = 0x341 // MRW Machine exception program counter.
  def MCAUSE    = 0x342 // MRW Machine trap cause.
  def MBADADDR  = 0x343 // MRW Machine bad address.
  def MIP       = 0x344 // MRW Machine interrupt pending.
  def MBASE     = 0x380 // MRW Base register.
  def MBOUND    = 0x381 // MRW Bound register.
  def MIBASE    = 0x382 // MRW Instruction base register.
  def MIBOUND   = 0x383 // MRW Instruction bound register.
  def MDBASE    = 0x384 // MRW Data base register.
  def MDBOUND   = 0x385 // MRW Data bound register.
  def MCYCLE    = 0xB00 // MRW Machine cycle counter.
  def MINSTRET  = 0xB02 // MRW Machine instructions-retired counter.
  def MCYCLEH   = 0xB80 // MRW Upper 32 bits of mcycle, RV32I only.
  def MINSTRETH = 0xB82 // MRW Upper 32 bits of minstret, RV32I only.

  val SSTATUS     = 0x100
  val SIE         = 0x104
  val STVEC       = 0x105
  val SCOUNTEREN  = 0x106
  val SSCRATCH    = 0x140
  val SEPC        = 0x141
  val SCAUSE      = 0x142
  val SBADADDR    = 0x143
  val SIP         = 0x144
  val SATP        = 0x180

  def UCYCLE   = 0xC00 // UR Machine ucycle counter.
  def UCYCLEH  = 0xC80
  def UTIME    = 0xC01 // rdtime
  def UTIMEH   = 0xC81
  def UINSTRET  = 0xC02 // UR Machine instructions-retired counter.
  def UINSTRETH = 0xC82 // UR Upper 32 bits of minstret, RV32I only.

  val FFLAGS = 0x1
  val FRM = 0x2
  val FCSR = 0x3
}

