#!/usr/bin/env python3

import subprocess
import random
import os

if os.getenv("NAXRISCV_SEED"):
    random.seed(os.getenv("NAXRISCV_SEED"))

NAXRISCV_TEST_FPU_FACTOR = 1.0
if os.getenv("NAXRISCV_TEST_FPU_FACTOR"):
    NAXRISCV_TEST_FPU_FACTOR = float(os.getenv("NAXRISCV_TEST_FPU_FACTOR"))


riscv_tests = [
	"rv32ui-p-lui",
	"rv32ui-p-auipc",
	"rv32ui-p-jal",
	"rv32ui-p-jalr",
	"rv32ui-p-beq",
	"rv32ui-p-bge",
	"rv32ui-p-bgeu",
	"rv32ui-p-blt",
	"rv32ui-p-bltu",
	"rv32ui-p-bne",
	"rv32ui-p-add",
	"rv32ui-p-addi",
	"rv32ui-p-and",
	"rv32ui-p-andi",
	"rv32ui-p-or",
	"rv32ui-p-ori",
	"rv32ui-p-sll",
	"rv32ui-p-slli",
	"rv32ui-p-slt",
	"rv32ui-p-slti",
	"rv32ui-p-sra",
	"rv32ui-p-srai",
	"rv32ui-p-srl",
	"rv32ui-p-srli",
	"rv32ui-p-sub",
	"rv32ui-p-xor",
	"rv32ui-p-xori"
]

riscv64_tests = [
    "rv64ui-p-add",
    "rv64ui-p-addi",
    "rv64ui-p-addiw",
    "rv64ui-p-addw",
    "rv64ui-p-and",
    "rv64ui-p-andi",
    "rv64ui-p-auipc",
    "rv64ui-p-beq",
    "rv64ui-p-bge",
    "rv64ui-p-bgeu",
    "rv64ui-p-blt",
    "rv64ui-p-bltu",
    "rv64ui-p-bne",
    "rv64ui-p-jal",
    "rv64ui-p-jalr",
    "rv64ui-p-or",
    "rv64ui-p-ori",
    "rv64ui-p-sll",
    "rv64ui-p-slli",
    "rv64ui-p-slliw",
    "rv64ui-p-sllw",
    "rv64ui-p-slt",
    "rv64ui-p-slti",
    "rv64ui-p-sltiu",
    "rv64ui-p-sltu",
    "rv64ui-p-sra",
    "rv64ui-p-srai",
    "rv64ui-p-sraiw",
    "rv64ui-p-sraw",
    "rv64ui-p-srl",
    "rv64ui-p-srli",
    "rv64ui-p-srliw",
    "rv64ui-p-srlw",
    "rv64ui-p-sub",
    "rv64ui-p-subw",
    "rv64ui-p-xor",
    "rv64ui-p-xori",
]

riscvTestMemory = [
	"rv32ui-p-lb",
	"rv32ui-p-lbu",
	"rv32ui-p-lh",
	"rv32ui-p-lhu",
	"rv32ui-p-lw",
	"rv32ui-p-sb",
	"rv32ui-p-sh",
	"rv32ui-p-sw"
]

riscv64TestMemory = [
    "rv64ui-p-lb",
    "rv64ui-p-lbu",
    "rv64ui-p-lh",
    "rv64ui-p-lhu",
    "rv64ui-p-lui",
    "rv64ui-p-lw",
    "rv64ui-p-lwu",
    "rv64ui-p-ld",
    "rv64ui-p-sb",
    "rv64ui-p-sh",
    "rv64ui-p-sw",
    "rv64ui-p-sd",
]


riscvTestAmo = [
    "rv32ua-p-amoswap_w",
    "rv32ua-p-amoor_w",
    "rv32ua-p-amoand_w",
    "rv32ua-p-amoxor_w",
    "rv32ua-p-amoadd_w",
    "rv32ua-p-amomaxu_w",
    "rv32ua-p-amomax_w",
    "rv32ua-p-amominu_w",
    "rv32ua-p-amomin_w",
]

riscv64TestAmo = [
    "rv64ua-p-amoadd_d",
    "rv64ua-p-amoand_d",
    "rv64ua-p-amomax_d",
    "rv64ua-p-amomaxu_d",
    "rv64ua-p-amomin_d",
    "rv64ua-p-amominu_d",
    "rv64ua-p-amoor_d",
    "rv64ua-p-amoswap_d",
    "rv64ua-p-amoxor_d",
    "rv64ua-p-amoadd_w",
    "rv64ua-p-amoand_w",
    "rv64ua-p-amomaxu_w",
    "rv64ua-p-amomax_w",
    "rv64ua-p-amomin_w",
    "rv64ua-p-amominu_w",
    "rv64ua-p-amoor_w",
    "rv64ua-p-amoswap_w",
    "rv64ua-p-amoxor_w",
]


riscv32TestFloat = [
    "rv32uf-p-fmadd",
    "rv32uf-p-fadd",
    "rv32uf-p-fcmp",
    "rv32uf-p-fcvt_w",
    "rv32uf-p-ldst",
    "rv32uf-p-recoding",
    "rv32uf-p-fclass",
    "rv32uf-p-fcvt",
    "rv32uf-p-fdiv",
    "rv32uf-p-fmin",
    "rv32uf-p-move"
]


riscv32TestDouble = [
    "rv32ud-p-fmadd",
    "rv32ud-p-fadd",
    "rv32ud-p-fcvt",
    "rv32ud-p-recoding",
    "rv32ud-p-fclass",
    "rv32ud-p-fcvt_w",
    "rv32ud-p-fmin",
    "rv32ud-p-fcmp",
    "rv32ud-p-fdiv",
    "rv32ud-p-ldst"
]

riscv64TestFloat = [
    "rv64uf-p-fcvt",
    "rv64uf-p-recoding",
    "rv64uf-p-fclass",
    "rv64uf-p-fadd",
    "rv64uf-p-fmin",
    "rv64uf-p-fmadd",
    "rv64uf-p-ldst",
    "rv64uf-p-fcmp",
    "rv64uf-p-fcvt_w",
    "rv64uf-p-fdiv",
    "rv64uf-p-move"
]


riscv64TestDouble = [
    "rv64ud-p-ldst",
    "rv64ud-p-move",
    "rv64ud-p-structural",
    "rv64ud-p-fcvt",
    "rv64ud-p-fdiv",
    "rv64ud-p-fclass",
    "rv64ud-p-recoding",
    "rv64ud-p-fmin",
    "rv64ud-p-fmadd",
    "rv64ud-p-fcvt_w",
    "rv64ud-p-fadd",
    "rv64ud-p-fcmp"
]

riscvTestMul = [
	"rv32um-p-mul",
	"rv32um-p-mulh",
	"rv32um-p-mulhsu",
	"rv32um-p-mulhu"
]

riscvTestDiv = [
	"rv32um-p-div",
	"rv32um-p-divu",
	"rv32um-p-rem",
	"rv32um-p-remu"
]


riscv64TestMul = [
    "rv64um-p-mul",
    "rv64um-p-mulh",
    "rv64um-p-mulhsu",
    "rv64um-p-mulhu",
    "rv64um-p-mulw",
]

riscv64TestDiv = [
    "rv64um-p-div",
    "rv64um-p-divu",
    "rv64um-p-rem",
    "rv64um-p-remu",
    "rv64um-p-divuw",
    "rv64um-p-divw",
    "rv64um-p-remuw",
    "rv64um-p-remw",
]



riscv32TestRvc = [
    "rv32uc-p-rvc",
]

riscv64TestRvc = [
    "rv64uc-p-rvc",
]

def listPrefix(prefix, l):
    return list(map(lambda x : prefix + x, l))

riscvArch32i = listPrefix("rv32i_m/I/", [
    "add-01",
    "addi-01",
    "and-01",
    "andi-01",
    "auipc-01",
    "beq-01",
    "bge-01",
    "bgeu-01",
    "blt-01",
    "bltu-01",
    "bne-01",
    "fence-01",
    "jal-01",
    "jalr-01",
    "lb-align-01",
    "lbu-align-01",
    "lh-align-01",
    "lhu-align-01",
    "lui-01",
    "lw-align-01",
    "or-01",
    "ori-01",
    "sb-align-01",
    "sh-align-01",
    "sll-01",
    "slli-01",
    "slt-01",
    "slti-01",
    "sltiu-01",
    "sltu-01",
    "sra-01",
    "srai-01",
    "srl-01",
    "srli-01",
    "sub-01",
    "sw-align-01",
    "xor-01",
    "xori-01",
])

riscvArch32M = listPrefix("rv32i_m/M/", [
    "div-01",
    "divu-01",
    "mul-01",
    "mulh-01",
    "mulhsu-01",
    "mulhu-01",
    "rem-01",
    "remu-01",
])

riscvArch32Zifencei = listPrefix("rv32i_m/Zifencei/", [
    "Fencei",
])

riscvArch32C = listPrefix("rv32i_m/C/", [
    "cadd-01",
    "caddi-01",
    "caddi16sp-01",
    "caddi4spn-01",
    "cand-01",
    "candi-01",
    "cbeqz-01",
    "cbnez-01",
    "cebreak-01",
    "cj-01",
    "cjal-01",
    "cjalr-01",
    "cjr-01",
    "cli-01",
    "clui-01",
    "clw-01",
    "clwsp-01",
    "cmv-01",
    "cnop-01",
    "cor-01",
    "cslli-01",
    "csrai-01",
    "csrli-01",
    "csub-01",
    "csw-01",
    "cswsp-01",
    "cxor-01",
])

riscvArch32Priv = listPrefix("rv32i_m/privilege/", [
    "ebreak",
    "ecall",
    "misalign1-jalr-01",
    "misalign2-jalr-01",
    "misalign-beq-01",
    "misalign-bge-01",
    "misalign-bgeu-01",
    "misalign-blt-01",
    "misalign-bltu-01",
    "misalign-bne-01",
    "misalign-jal-01",
    "misalign-lh-01",
    "misalign-lhu-01",
    "misalign-lw-01",
    "misalign-sh-01",
    "misalign-sw-01",
])






riscvArch64i = listPrefix("rv64i_m/I/", [
    "add-01",
    "addi-01",
    "addiw-01",
    "addw-01",
    "and-01",
    "andi-01",
    "auipc-01",
    "beq-01",
    "bge-01",
    "bgeu-01",
    "blt-01",
    "bltu-01",
    "bne-01",
    "fence-01",
    "jal-01",
    "jalr-01",
    "lb-align-01",
    "lbu-align-01",
    "ld-align-01",
    "lh-align-01",
    "lhu-align-01",
    "lui-01",
    "lw-align-01",
    "lwu-align-01",
    "or-01",
    "ori-01",
    "sb-align-01",
    "sd-align-01",
    "sh-align-01",
    "sll-01",
    "slli-01",
    "slliw-01",
    "sllw-01",
    "slt-01",
    "slti-01",
    "sltiu-01",
    "sltu-01",
    "sra-01",
    "srai-01",
    "sraiw-01",
    "sraw-01",
    "srl-01",
    "srli-01",
    "srliw-01",
    "srlw-01",
    "sub-01",
    "subw-01",
    "sw-align-01",
    "xor-01",
    "xori-01",
])

riscvArch64M = listPrefix("rv64i_m/M/", [
    "div-01",
    "divu-01",
    "divuw-01",
    "divw-01",
    "mul-01",
    "mulh-01",
    "mulhsu-01",
    "mulhu-01",
    "mulw-01",
    "rem-01",
    "remu-01",
    "remuw-01",
    "remw-01",
])

riscvArch64Zifencei = listPrefix("rv64i_m/Zifencei/", [
    "Fencei",
])

riscvArch64C = listPrefix("rv64i_m/C/", [
    "cadd-01",
    "caddi-01",
    "caddi16sp-01",
    "caddi4spn-01",
    "caddiw-01",
    "caddw-01",
    "cand-01",
    "candi-01",
    "cbeqz-01",
    "cbnez-01",
    "cebreak-01",
    "cj-01",
    "cjalr-01",
    "cjr-01",
    "cld-01",
    "cldsp-01",
    "cli-01",
    "clui-01",
    "clw-01",
    "clwsp-01",
    "cmv-01",
    "cnop-01",
    "cor-01",
    "csd-01",
    "csdsp-01",
    "cslli-01",
    "csrai-01",
    "csrli-01",
    "csub-01",
    "csubw-01",
    "csw-01",
    "cswsp-01",
    "cxor-01",
])

riscvArch64Priv = listPrefix("rv64i_m/privilege/", [
    "ebreak",
    "ecall",
    "misalign1-jalr-01",
    "misalign2-jalr-01",
    "misalign-beq-01",
    "misalign-bge-01",
    "misalign-bgeu-01",
    "misalign-blt-01",
    "misalign-bltu-01",
    "misalign-bne-01",
    "misalign-jal-01",
    "misalign-ld-01",
    "misalign-lh-01",
    "misalign-lhu-01",
    "misalign-lw-01",
    "misalign-lwu-01",
    "misalign-sd-01",
    "misalign-sh-01",
    "misalign-sw-01",
])


riscvArch32C = listPrefix("rv32i_m/C/", [
    "caddi16sp-01",
    "cxor-01",
    "cnop-01",
    "cslli-01",
    "cmv-01",
    "clwsp-01",
    "csrai-01",
    "cj-01",
    "cand-01",
    "cebreak-01",
    "cli-01",
    "csub-01",
    "caddi4spn-01",
    "cbnez-01",
    "clw-01",
    "csw-01",
    "candi-01",
    "cswsp-01",
    "cjalr-01",
    "caddi-01",
    "clui-01",
    "cadd-01",
    "cbeqz-01",
    "cjr-01",
    "csrli-01",
    "cor-01",
    "cjal-01",
])

riscvArch64C = listPrefix("rv64i_m/C/", [
    "caddi16sp-01",
    "csubw-01",
    "csdsp-01",
    "cxor-01",
    "cnop-01",
    "cslli-01",
    "cmv-01",
    "clwsp-01",
    "csrai-01",
    "cj-01",
    "cand-01",
    "cebreak-01",
    "cldsp-01",
    "cli-01",
    "csub-01",
    "caddi4spn-01",
    "cbnez-01",
    "caddw-01",
    "csd-01",
    "clw-01",
    "caddiw-01",
    "csw-01",
    "candi-01",
    "cswsp-01",
    "cjalr-01",
    "caddi-01",
    "cld-01",
    "clui-01",
    "cadd-01",
    "cbeqz-01",
    "cjr-01",
    "csrli-01",
    "cor-01",
])

fpuTestRvf32 = [
    [0,   "fmv.x.w"   , "f32"],
    [31,  "fmv.s.x" ,  "f32"],
    [101, "fadd.s"    , "f32"],
    [102, "fsub.s"    , "f32"],
    [103, "fmul.s"    , "f32"],
    [104, "fdiv.s"    , "f32"],
    [105, "fsqrt.s"   , "f32"],
    [106, "fmadd.s"   , "f32"],
    [107, "fmsub.s"   , "f32"],
    [108, "fnmadd.s"  , "f32"],
    [109, "fnmsub.s"  , "f32"],
    [110, "fsgnj.s"   , "f32"],
    [111, "fsgnjn.s"  , "f32"],
    [112, "fsgnjx.s"  , "f32"],
    [113, "fmin.s"    , "f32"],
    [114, "fmax.s"    , "f32"],
    [115, "fle.s"     , "f32"],
    [116, "feq.s"     , "f32"],
    [117, "flt.s"     , "f32"],
    [118, "fclass.s"  , "f32"],
    [119, "fcvt.s.wu" , "ui32"],
    [120, "fcvt.s.w"  , "i32"],
    [121, "fcvt.wu.s" , "f32"],
    [122, "fcvt.w.s"  , "f32"],
]

fpuTestRvf64 = [
    [127, "fcvt.s.lu" , "ui64"],
    [128, "fcvt.s.l"  , "i64"],
    [129, "fcvt.lu.s" , "f32"],
    [130, "fcvt.l.s"  , "f32"],
    [31, "fmv.s.x_64", "f64"],
    [202, "fcvt.s.wu_64" , "ui64"],
    [203, "fcvt.s.w_64"  , "i64"],
]

fpuTestRvd32 = [
    [1, "fadd.d"    , "f64"],
    [2, "fsub.d"    , "f64"],
    [3, "fmul.d"    , "f64"],
    [4, "fdiv.d"    , "f64"],
    [5, "fsqrt.d"   , "f64"],
    [6, "fmadd.d"   , "f64"],
    [7, "fmsub.d"   , "f64"],
    [8, "fnmadd.d"  , "f64"],
    [9, "fnmsub.d"  , "f64"],
    [10, "fsgnj.d"  , "f64"],
    [11, "fsgnjn.d" , "f64"],
    [12, "fsgnjx.d" , "f64"],
    [13, "fmin.d"   , "f64"],
    [14, "fmax.d"   , "f64"],
    [15, "fle.d"    , "f64"],
    [16, "feq.d"    , "f64"],
    [17, "flt.d"    , "f64"],
    [18, "fclass.d" , "f64"],
    [19, "fcvt.d.wu", "ui32"],
    [20, "fcvt.d.w" , "i32"],
    [21, "fcvt.wu.d", "f64"],
    [22, "fcvt.w.d" , "f64"],
    [23, "fcvt.d.s" , "f64"],
    [24, "fcvt.s.d" , "f64"],
]

fpuTestRvd64 = [
    [25, "fmv.x.d"  , "f64"],
    [26, "fmv.d.x"  , "ui64"],
    [27, "fcvt.d.lu", "ui64"],
    [28, "fcvt.d.l" , "i64"],
    [29, "fcvt.lu.d", "f64"],
    [30, "fcvt.l.d" , "f64"],


    [200, "fcvt.d.wu_64", "ui64"],
    [201, "fcvt.d.w_64" , "i64"],
]

import os

naxriscv_gen_folder = os.getenv('NAXRISCV_GEN_FOLDER')
naxriscv_header = os.getenv('NAXRISCV_HEADER')
naxriscv_header_path = naxriscv_header if naxriscv_header else naxriscv_gen_folder+"/nax.h" if naxriscv_gen_folder else '../../../../nax.h'

naxriscv_software = os.getenv('NAXRISCV_SOFTWARE')
if not naxriscv_software:
    naxriscv_software = '../../../../ext/NaxSoftware'

freertos_count = 4
linux_count = 1

if os.getenv('FREERTOS_COUNT'):
    freertos_count = int(os.getenv('FREERTOS_COUNT'))
if os.getenv('LINUX_COUNT'):
    linux_count = int(os.getenv('LINUX_COUNT'))

file = open(naxriscv_header_path,mode='r')
naxHeader = file.read()
file.close()
import re

def getInt(key):
    return int(re.findall(key + " (\d+)", naxHeader)[0])

def getBoolean(key):
    return len(re.findall(key + " true", naxHeader)) != 0


xlen = getInt("XLEN")
rvc = getBoolean("RVC")
rvf = getBoolean("RVF")
rvd = getBoolean("RVD")
rva = getBoolean("RVA")



if xlen == 64:
    arch="rv64im"
    archLinux="rv64im"
else:
    arch="rv32im"
    archLinux="rv32im"

if rva:
    arch += "a"
    archLinux += "a"


if rvf:
    arch += "f"
    archLinux += "f"
if rvd:
    arch += "d"
    archLinux += "d"

if rvc:
    arch += "c"
    archLinux += "c"


naxSoftware = [
	["lsu", "baremetal/lsu/build/rv32im/lsu.elf"],
]

nax64Software = []

naxSoftwareRegular = [
    "machine", "supervisor", "mmu_sv32", "dhrystone", "coremark"
]
nax64SoftwareRegular = [
    "machine", "supervisor",  "mmu_sv39", "dhrystone", "coremark",
]

freertos = ["blocktim", "countsem", "EventGroupsDemo", "flop", "integer", "QPeek",
            "QueueSet", "recmutex", "semtest", "TaskNotify", "dynamic",
            "GenQTest", "PollQ", "QueueOverwrite", "QueueSetPolling", "sp_flop", "test1"]

for e in naxSoftwareRegular:
    naxSoftware.append([e, f"baremetal/{e}/build/{arch}/{e}.elf"])


for e in random.sample(freertos, freertos_count):
    naxSoftware.append(["freertos/" + e, f"baremetal/freertosDemo/build/{e}/{arch}/freertosDemo.elf"])

for e in nax64SoftwareRegular:
    nax64Software.append([e, f"baremetal/{e}/build/{arch}/{e}.elf"])

# naxSoftware.append(["coremark", f"baremetal/coremark/coremark_{arch}.elf"])

tests = []
testsFast = []
testsFpu = []
ouputs = []

with open('tests.mk', 'w') as f:

    def rvTest(name, elf=None, timeout=10000, passs="pass", start="test_2", startAdd = 0):
        if not elf:
            elf = name
        outputDir = "output/riscv_tests/" + name
        rule = outputDir +"/PASS"
        tests.append(rule)
        testsFast.append(rule)
        ouputs.append(outputDir)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", name,
            "--output-dir", outputDir,
            "--load-elf", f"{naxriscv_software}/riscv-tests/{elf}",
            "--start-symbol", start,
            "--start-add", str(startAdd),
            "--pass-symbol", passs,
            "--fail-symbol", "fail",
            "--timeout", str(timeout),
            "--seed", str(random.randint(0, 100000000)),
           "${ARGS}"
        ]))
        f.write(f"\n\n")

    def rvArch(name, elf=None, timeout=1000000, passs="pass"):
        if not elf:
            elf = name
        outputDir = "output/riscv_arch/" + name
        rule = outputDir +"/PASS"
        tests.append(rule)
        testsFast.append(rule)
        ouputs.append(outputDir)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", name,
            "--output-dir", outputDir,
            "--load-elf", f"{naxriscv_software}/riscv-arch-test/{elf}.elf",
            "--pass-symbol", passs,
            "--timeout", str(timeout),
            "--seed", str(random.randint(0, 100000000)),
           "${ARGS}"
        ]))
        f.write(f"\n\n")


    def fpuTest(name, vector, testId):
        outputDir = "output/nax/fpu_test/" + name
        rule = outputDir +"/PASS"
        tests.append(rule)
        testsFast.append(rule)
        testsFpu.append(rule)
        ouputs.append(outputDir)
        testCount = int(0x50000*NAXRISCV_TEST_FPU_FACTOR)
        if "sqrt" in name:
            testCount //= 16
        if "div" in name:
            testCount //= 8
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", name,
            "--output-dir", outputDir,
            "--load-bin", f"{naxriscv_software}/baremetal/fpu_test/vector/{vector}.bin,0x90000000",
            "--load-u32", f"{hex(testCount)},0xA0000000",
            "--load-u32", f"{hex(testId)},0xA0000004",
            "--load-elf", f"{naxriscv_software}/baremetal/fpu_test/build/{archLinux}/fpu_test.elf",
            "--pass-symbol", "pass",
            "--fail-symbol", "fail",
            "--timeout", str(500000000),
            "--seed", str(random.randint(0, 100000000)),
           "${ARGS}"
        ]))
        f.write(f"\n\n")

    def fpuTest3():
        name = "fpu_test3"
        outputDir = "output/nax/" + name
        rule = outputDir +"/PASS"
        tests.append(rule)
        testsFast.append(rule)
        testsFpu.append(rule)
        ouputs.append(outputDir)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", name,
            "--output-dir", outputDir,
            "--load-bin", f"{naxriscv_software}/baremetal/fpu_test/vector/f32.bin,0x90000000",
            "--load-elf", f"{naxriscv_software}/baremetal/fpu_test3/build/{archLinux}/fpu_test3.elf",
            "--pass-symbol", "pass",
            "--fail-symbol", "fail",
            "--timeout", str(500000000),
            "--seed", str(random.randint(0, 100000000)),
           "${ARGS}"
        ]))
        f.write(f"\n\n")


    def regularSoftware(name, path):
        outputDir = "output/nax/" + name
        rule = outputDir +"/PASS"
        tests.append(rule)
        ouputs.append(outputDir)
        if name in naxSoftwareRegular:
            testsFast.append(rule)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", name,
            "--output-dir", outputDir,
            "--load-elf", f"{naxriscv_software}/{path}",
            "--start-symbol", "_start",
            "--pass-symbol", "pass",
            "--fail-symbol", "fail",
            "--no-putc-flush",
            "--seed", str(random.randint(0, 100000000)),
           "${ARGS}"
        ]))
        f.write(f"\n\n")

    def regularBaremetal(name):
        regularSoftware(name, f"baremetal/{name}/build/{arch}/{name}.elf")

    if xlen == 64:
        for name in riscv64_tests + riscv64TestMemory + riscv64TestMul + riscv64TestDiv + riscv64TestAmo:
            rvTest(name)

        if rvc:
            for name in riscv64TestRvc:
                rvTest(name, startAdd=-8)


        if rvf:
            for name in riscv64TestFloat:
                rvTest(name, start = "_start")
        if rvd:
            for name in riscv64TestDouble:
                rvTest(name, start = "_start")

        for name in riscvArch64i + riscvArch64M + riscvArch64Zifencei:
            rvArch(name)

        if rvc:
            for name in riscvArch64C:
                rvArch(name)


        if rvf:
            regularBaremetal("fpu_test2")
            fpuTest3()
            for e in fpuTestRvf32:
                fpuTest(e[1], e[2], e[0])
            if xlen == 64:
                for e in fpuTestRvf64:
                    fpuTest(e[1], e[2], e[0])

        if rvd:
            for e in fpuTestRvd32:
                fpuTest(e[1], e[2], e[0])
            if xlen == 64:
                for e in fpuTestRvd64:
                    fpuTest(e[1], e[2], e[0])


        for spec in nax64Software:
            regularSoftware(spec[0], spec[1])




    if xlen == 32:
        for name in riscv_tests + riscvTestMemory + riscvTestMul + riscvTestDiv + riscvTestAmo:
            rvTest(name)

        if rvc:
            for name in riscv32TestRvc:
                rvTest(name, startAdd=-8)

        if rvf:
            for name in riscv32TestFloat:
                rvTest(name, start = "_start")
        if rvd:
            for name in riscv32TestDouble:
                rvTest(name, start = "_start")

        rvTest("rv32ua-p-lrsc_1234", elf="rv32ua-p-lrsc", timeout=300000, passs="test_5")
        rvTest("rv32ua-p-lrsc_6", elf="rv32ua-p-lrsc", timeout=100000, start="test_6")


        if rvf:
            regularBaremetal("fpu_test2")
            fpuTest3()
            for e in fpuTestRvf32:
                fpuTest(e[1], e[2], e[0])

        if rvd:
            for e in fpuTestRvd32:
                fpuTest(e[1], e[2], e[0])

        for name in riscvArch32i + riscvArch32M + riscvArch32Zifencei:
            rvArch(name)

        if rvc:
            for name in riscvArch32C:
                rvArch(name)

        for spec in naxSoftware:
            regularSoftware(spec[0], spec[1])



    for i in range(linux_count):
        outputDir = "output/nax/buildroot/run" + str(i)
        rule = outputDir +"/PASS"
        imagePath = f"{naxriscv_software}/buildroot/images/" + archLinux

        tests.append(rule)
        ouputs.append(outputDir)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
        f"""./obj_dir/VNaxRiscv \\
           --seed {i}                  \\
           --name buildroot_run{i}    \\
           --output-dir  {outputDir}   \\
           --load-bin {imagePath}/fw_jump.bin,0x80000000 \\
           --load-bin {imagePath}/linux.dtb,0x80F80000 \\
           --load-bin {imagePath}/Image,0x80400000 \\
           --load-bin {imagePath}/rootfs.cpio,0x81000000 \\
           --no-stdin                  \\
           --no-putc-flush          \\
           --seed={str(random.randint(0, 100000000))} \\
           --getc "buildroot login" \\
           --putc "root" \\
           --getc "#" \\
           --putc "cat /proc/cpuinfo" \\
           --getc "#" \\
           --putc "echo 1+2+3*4 | bc" \\
           --getc "#" \\
           --putc "micropython" \\
           --getc ">>> " \\
           --putc "import math" \\
           --getc ">>> " \\
           --putc "math.sin(math.pi/4)" \\
           --getc ">>> " \\
           --putc "from sys import exit" \\
           --getc ">>> " \\
           --putc "exit()" \\
           --getc "#" \\
           --putc "ls /" \\
           --getc "#" \\
           --success \\
           ${{ARGS}} """
        ]))
        f.write(f"\n\n")



    f.write(f"""TESTS_COUNT={len(tests)}\n""")

    f.write(f"""test-report:\n""")
    for test in tests:
        f.write(f"""\t@[ ! -f {test} ] && echo "{test} didn't passed"  || true\n""")
    f.write(f"""\t@PASSED=$(shell find {" ".join(tests)} | wc -l) && echo $$PASSED/$(TESTS_COUNT) passed\n""")  #; [ $PASSED $(TESTS_COUNT) ] && true || false
    f.write(f"\n\n")

# 	@PASSED=$(shell find output/riscv_tests/rv32ui-p-lui/PASS output/riscv_tests/rv32ui-p-auipc/PASS output/riscv_tests/rv32ui-p-jal/PASS output/riscv_tests/rv32ui-p-jalr/PASS output/riscv_tests/rv32ui-p-beq/PASS output/riscv_tests/rv32ui-p-bge/PASS output/riscv_tests/rv32ui-p-bgeu/PASS output/riscv_tests/rv32ui-p-blt/PASS output/riscv_tests/rv32ui-p-bltu/PASS output/riscv_tests/rv32ui-p-bne/PASS output/riscv_tests/rv32ui-p-add/PASS output/riscv_tests/rv32ui-p-addi/PASS output/riscv_tests/rv32ui-p-and/PASS output/riscv_tests/rv32ui-p-andi/PASS output/riscv_tests/rv32ui-p-or/PASS output/riscv_tests/rv32ui-p-ori/PASS output/riscv_tests/rv32ui-p-sll/PASS output/riscv_tests/rv32ui-p-slli/PASS output/riscv_tests/rv32ui-p-slt/PASS output/riscv_tests/rv32ui-p-slti/PASS output/riscv_tests/rv32ui-p-sra/PASS output/riscv_tests/rv32ui-p-srai/PASS output/riscv_tests/rv32ui-p-srl/PASS output/riscv_tests/rv32ui-p-srli/PASS output/riscv_tests/rv32ui-p-sub/PASS output/riscv_tests/rv32ui-p-xor/PASS output/riscv_tests/rv32ui-p-xori/PASS output/riscv_tests/rv32ui-p-lb/PASS output/riscv_tests/rv32ui-p-lbu/PASS output/riscv_tests/rv32ui-p-lh/PASS output/riscv_tests/rv32ui-p-lhu/PASS output/riscv_tests/rv32ui-p-lw/PASS output/riscv_tests/rv32ui-p-sb/PASS output/riscv_tests/rv32ui-p-sh/PASS output/riscv_tests/rv32ui-p-sw/PASS output/nax/load/PASS | wc -l) && \
# 	echo $$PASSED/$(TESTS_COUNT) passed


    f.write(f"""test-all: {" ".join(tests)}\n""")
    f.write(f"\n\n")

    f.write(f"""test-fast: {" ".join(testsFast)}\n""")
    f.write(f"\n\n")

    f.write(f"""test-fpu: {" ".join(testsFpu)}\n""")
    f.write(f"\n\n")


    f.write(f"""test-clean:\n""")
    f.write(f"""\trm -rf {" ".join(ouputs)}\n""")

    f.write(f"\n\n")



# print(tests)


# for test in tests:
#     subprocess.run(test)