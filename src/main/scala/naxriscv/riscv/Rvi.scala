package naxriscv.riscv

import naxriscv.riscv.IntRegFile.TypeU
import spinal.core._

object Rvi{
  import IntRegFile._

  def ADD                = TypeR(M"0000000----------000-----0110011")
  def SUB                = TypeR(M"0100000----------000-----0110011")
  def SLL                = TypeR(M"0000000----------001-----0110011")
  def SLT                = TypeR(M"0000000----------010-----0110011")
  def SLTU               = TypeR(M"0000000----------011-----0110011")
  def XOR                = TypeR(M"0000000----------100-----0110011")
  def SRL                = TypeR(M"0000000----------101-----0110011")
  def SRA                = TypeR(M"0100000----------101-----0110011")
  def OR                 = TypeR(M"0000000----------110-----0110011")
  def AND                = TypeR(M"0000000----------111-----0110011")


  def ADDI               = TypeI(M"-----------------000-----0010011")
  def SLLI               = TypeI(M"000000-----------001-----0010011")
  def SLTI               = TypeI(M"-----------------010-----0010011")
  def SLTIU              = TypeI(M"-----------------011-----0010011")
  def XORI               = TypeI(M"-----------------100-----0010011")
  def SRLI               = TypeI(M"000000-----------101-----0010011")
  def SRAI               = TypeI(M"010000-----------101-----0010011")
  def ORI                = TypeI(M"-----------------110-----0010011")
  def ANDI               = TypeI(M"-----------------111-----0010011")


  def ADDW               = TypeR(M"0000000----------000-----0111011")
  def SUBW               = TypeR(M"0100000----------000-----0111011")
  def ADDIW              = TypeI(M"-----------------000-----0011011")

  def SLLW                = TypeR(M"0000000----------001-----0111011")
  def SRLW                = TypeR(M"0000000----------101-----0111011")
  def SRAW                = TypeR(M"0100000----------101-----0111011")
  def SLLIW               = TypeI(M"000000-----------001-----0011011")
  def SRLIW               = TypeI(M"000000-----------101-----0011011")
  def SRAIW               = TypeI(M"010000-----------101-----0011011")

  def LUI   = TypeU(M"-------------------------0110111")
  def AUIPC = TypeUPC(M"-------------------------0010111")


  def BEQ  =  TypeB(M"-----------------000-----1100011")
  def BNE  =  TypeB(M"-----------------001-----1100011")
  def BLT  =  TypeB(M"-----------------100-----1100011")
  def BGE  =  TypeB(M"-----------------101-----1100011")
  def BLTU =  TypeB(M"-----------------110-----1100011")
  def BGEU =  TypeB(M"-----------------111-----1100011")
  def JALR =  TypeI(M"-----------------000-----1100111")
  def JAL  =  TypeJ(M"-------------------------1101111")



  def LB                 = TypeILQ(M"-----------------000-----0000011")
  def LH                 = TypeILQ(M"-----------------001-----0000011")
  def LW                 = TypeILQ(M"-----------------010-----0000011")
  def LD                 = TypeILQ(M"-----------------011-----0000011")
  def LBU                = TypeILQ(M"-----------------100-----0000011")
  def LHU                = TypeILQ(M"-----------------101-----0000011")
  def LWU                = TypeILQ(M"-----------------110-----0000011")

  def SB                 = TypeSSQ(M"-----------------000-----0100011")
  def SH                 = TypeSSQ(M"-----------------001-----0100011")
  def SW                 = TypeSSQ(M"-----------------010-----0100011")
  def SD                 = TypeSSQ(M"-----------------011-----0100011")

  def LR                 = TypeILQ(M"00010--00000-----010-----0101111")
  def SC                 = TypeASQ(M"00011------------010-----0101111")

  def AMOSWAP            = TypeASQ(M"00001------------010-----0101111")
  def AMOADD             = TypeASQ(M"00000------------010-----0101111")
  def AMOXOR             = TypeASQ(M"00100------------010-----0101111")
  def AMOAND             = TypeASQ(M"01100------------010-----0101111")
  def AMOOR              = TypeASQ(M"01000------------010-----0101111")
  def AMOMIN             = TypeASQ(M"10000------------010-----0101111")
  def AMOMAX             = TypeASQ(M"10100------------010-----0101111")
  def AMOMINU            = TypeASQ(M"11000------------010-----0101111")
  def AMOMAXU            = TypeASQ(M"11100------------010-----0101111")


  def MUL                = TypeR(M"0000001----------000-----0110011")
  def MULH               = TypeR(M"0000001----------001-----0110011")
  def MULHSU             = TypeR(M"0000001----------010-----0110011")
  def MULHU              = TypeR(M"0000001----------011-----0110011")


  def DIV                = TypeR(M"0000001----------100-----0110011")
  def DIVU               = TypeR(M"0000001----------101-----0110011")
  def REM                = TypeR(M"0000001----------110-----0110011")
  def REMU               = TypeR(M"0000001----------111-----0110011")

  def MULW               = TypeR(M"0000001----------000-----0111011")
  def DIVW               = TypeR(M"0000001----------100-----0111011")
  def DIVUW              = TypeR(M"0000001----------101-----0111011")
  def REMW               = TypeR(M"0000001----------110-----0111011")
  def REMUW              = TypeR(M"0000001----------111-----0111011")

  def CSRRW              = TypeI (M"-----------------001-----1110011")
  def CSRRS              = TypeI (M"-----------------010-----1110011")
  def CSRRC              = TypeI (M"-----------------011-----1110011")
  def CSRRWI             = TypeIC(M"-----------------101-----1110011")
  def CSRRSI             = TypeIC(M"-----------------110-----1110011")
  def CSRRCI             = TypeIC(M"-----------------111-----1110011")

  def EBREAK             = TypeNone(M"00000000000100000000000001110011")
  def ECALL              = TypeNone(M"00000000000000000000000001110011")
  def MRET               = TypeNone(M"00110000001000000000000001110011")
  def SRET               = TypeNone(M"00010000001000000000000001110011")
  def URET               = TypeNone(M"00000000001000000000000001110011")
  def FENCEI             = TypeNone(M"00000000000000000001000000001111")
  def WFI                = TypeNone(M"00010000010100000000000001110011")

  def FENCE              = TypeNone(M"-----------------000-----0001111")
  def FENCE_I            = TypeNone(M"-----------------001-----0001111")
  def SFENCE_VMA         = TypeNone(M"0001001----------000000001110011")

  def FLUSH_DATA         = TypeNone(M"-------00000-----101-----0001111")

//  def LR                 = M"00010--00000-----010-----0101111"
//  def SC                 = M"00011------------010-----0101111"
//
//  def AMOSWAP            = M"00001------------010-----0101111"
//  def AMOADD             = M"00000------------010-----0101111"
//  def AMOXOR             = M"00100------------010-----0101111"
//  def AMOAND             = M"01100------------010-----0101111"
//  def AMOOR              = M"01000------------010-----0101111"
//  def AMOMIN             = M"10000------------010-----0101111"
//  def AMOMAX             = M"10100------------010-----0101111"
//  def AMOMINU            = M"11000------------010-----0101111"
//  def AMOMAXU            = M"11100------------010-----0101111"

}



class AtomicAlu(op : Bits,
                swap : Bool,
                mem : Bits,
                rf : Bits) extends Area{
  val compare  = op.msb
  val unsigned = op(1)
  val addSub = (rf.asSInt + Mux(compare, ~mem, mem).asSInt + Mux(compare, S(1), S(0))).asBits
  val less = Mux(rf.msb === mem.msb, addSub.msb, Mux(unsigned, mem.msb, rf.msb))
  val selectRf = swap ? True | (op.lsb ^ less)

  val result = (op | (swap ## B"00")).mux(
    B"000"  -> addSub,
    B"001"  -> (rf ^ mem),
    B"010"  -> (rf | mem),
    B"011"  -> (rf & mem),
    default -> (selectRf ? rf | mem)
  )
}