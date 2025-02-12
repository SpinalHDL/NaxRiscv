#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

import subprocess
import random
import os
import argparse
import sys

if os.getenv("NAXRISCV_SEED"):
    random.seed(os.getenv("NAXRISCV_SEED"))

NAXRISCV_TEST_FPU_FACTOR = 0.1
if os.getenv("NAXRISCV_TEST_FPU_FACTOR"):
    NAXRISCV_TEST_FPU_FACTOR = float(os.getenv("NAXRISCV_TEST_FPU_FACTOR"))


riscv32_tests = [
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


#naxriscv_gen_folder = os.getenv('NAXRISCV_GEN_FOLDER')
#naxriscv_header = os.getenv('NAXRISCV_HEADER')
#naxriscv_header_path = naxriscv_header if naxriscv_header else naxriscv_gen_folder+"/nax.h" if naxriscv_gen_folder else 'nax.h'

# socSim workspace path
workspace_path = None
if os.getenv('WORKSPACE_PATH'):
    workspace_path = os.getenv('WORKSPACE_PATH') #workspace_path = s"simWorkspace/rvls"
#print(f"\nSocDemo workspace path: {workspace_path}\n")

# socSim workspace name
workspace_name = None
if os.getenv('WORKSPACE_NAME'):
    workspace_name = os.getenv('WORKSPACE_NAME') #workspace_name = s"$name" (config_rv[XLEN][ISA])

# Name of output directory in socSim workspace
workspace_output_dir = None
if os.getenv('WORKSPACE_OUTPUT_DIR'):
    workspace_output_dir = os.getenv('WORKSPACE_OUTPUT_DIR') #workspace_output_dir = s"logs"

# Reference path to calculate relative path
#base_path = "simWorkspace"
# Relative path extraction
#socdemo_gen_folder_rel = os.path.relpath(workspace_path, base_path)
#print(f"SocDemo gen folder relatif: {socdemo_gen_folder_rel}\n")

naxriscv_software = os.getenv('NAXRISCV_SOFTWARE')
if naxriscv_software is None:
    naxriscv_software = 'ext/NaxSoftware'

freertos_count = 4
linux_count = 1 #verifier si l'image linux est disponible pour la config
nax_count = None
if os.getenv('FREERTOS_COUNT'):
    freertos_count = int(os.getenv('FREERTOS_COUNT'))
if os.getenv('LINUX_COUNT'):
    linux_count = int(os.getenv('LINUX_COUNT'))
if os.getenv('NAXRISCV_COUNT'):
    nax_count = int(os.getenv('NAXRISCV_COUNT'))

# Create an ArgumentParser object
parser = argparse.ArgumentParser(description="Parameters and simulation options for NaxRiscv")

# Ajouter des arguments positionnels pour la liste
# Architecture parameters 
parser.add_argument('--xlen', type=int, default=32, help='[XLEN] specify the value of xlen 32 or 64')
parser.add_argument('--naxCount', type=int, default=1, help='[NAXCOUNT] specify the number of Naxriscv cores')
parser.add_argument('--withRvc', action='store_true', help='Add [--withRvc] option to activate Compressed Instructions Extension')
parser.add_argument('--withFloat', action='store_true', help='Add [--withFloat] option  to activate Single-precision floating-point extension')
parser.add_argument('--withDouble', action='store_true', help='Add [--withDouble] option to activate Double-precision floating-point extension')
parser.add_argument('--noRva', action='store_true', help='Add [--noRva] option to disable the Atomic extension, which is enabled by default')
parser.add_argument('--noL2', action='store_true', help='Add [--no-l2] option to disable the l2 cache, which is enabled by default')
# Simulation parameters
parser.add_argument('--noRvls', action='store_true', help='Add [--noRvls] option to Disable rvls, so you dont need to compile it, but the NaxRiscv behaviour will not be checked.')
parser.add_argument('--dualSim', action='store_true', help='Add [--dualSim] option to activate Double simulation, one preceding the other, which will trigger the second simulation to capture the wave if the first fails.')
parser.add_argument('--trace', action='store_true', help='Add [--trace] option to enable wave capture')
# Workspace definition
parser.add_argument('--workspacePath', type=str, default="./simWorkspace/SocDemo", help='Set the path in the simWorkspace folder')
parser.add_argument('--workspaceName', type=str, default='.', help='set workspace name in workspaceDir folder')
parser.add_argument('--workspaceOutputDir', type=str, default='logs', help='set Name of output directory in socSim workspace')

# Parsing command line arguments
args = parser.parse_args()

# Use argument values for constant initialisation
# Naxriscv core
xlen = args.xlen
rvc = args.withRvc
rvf = args.withFloat
rvd = args.withDouble
rva = not args.noRva
# Naxriscv SoC
naxCount = args.naxCount if nax_count is None else nax_count

withL2 = not args.noL2
# Naxriscv simulation
withRvls = not args.noRvls
dualSim = args.dualSim
traceIt = args.trace
# Set socSim workspace path
workspacePath = args.workspacePath if workspace_path is None else workspace_path
workspaceName = args.workspaceName if workspace_name is None else workspace_name
workspaceOutputDir = args.workspaceOutputDir if workspace_output_dir is None else workspace_output_dir
'''
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
'''


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

# Config name
config_name = archLinux + "su"
print(f"ISA arch: {config_name}\n")

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

if(rvf or rvd):
    freertos.remove("sp_flop")
    freertos.remove("flop")

for e in naxSoftwareRegular:
    naxSoftware.append([e, f"baremetal/{e}/build/{arch}/{e}.elf"])


for e in random.sample(freertos, freertos_count):
    naxSoftware.append(["freertos/" + e, f"baremetal/freertosDemo/build/{e}/{arch}/freertosDemo.elf"])

for e in nax64SoftwareRegular:
    nax64Software.append([e, f"baremetal/{e}/build/{arch}/{e}.elf"])

# naxSoftware.append(["coremark", f"baremetal/coremark/coremark_{arch}.elf"])

# Tests list
rules = []
testsPass = []
testsFail = []
testsFast = []
testsFpu = []
testsBuildroot = []
ouputs = []
ouputsFast = []
ouputsFpu = []
ouputsBuildroot = []

def find_project_directory():
    # Get the directory of the current script
    current_directory = os.path.dirname(__file__)
    project_directory = os.path.dirname(__file__)
    first_part_path = None
    #print(f"directory of the current script: {current_directory}\n")
    # Traverse up until finding the project directory
    for _ in range(current_directory.count(os.path.sep)): # Iterate over the number of path separators
        if "NaxRiscv" in os.listdir(current_directory) or "workspace" in os.listdir(current_directory):
            naxriscv_path = os.path.join(current_directory, "NaxRiscv")
            workspace_path = os.path.join(current_directory, "workspace")
            #print(f"naxriscv_path: {naxriscv_path}\n")
            #print(f"workspace_path: {workspace_path}\n")
            # Check if one directory is a subdirectory of the other
            if naxriscv_path.startswith(workspace_path) or workspace_path.startswith(naxriscv_path):
                raise ValueError("Both 'NaxRiscv' and 'workspace' cannot be subdirectories of each other.")
            first_part_path = naxriscv_path if os.path.exists(naxriscv_path) else workspace_path
            break
        # Move up one directory
        current_directory = os.path.dirname(current_directory)
    if first_part_path is None:
        raise ValueError("Could not find both 'NaxRiscv' or 'workspace' like parent directories.")
    #print(f"Returned directory : {project_directory}\n")
    return project_directory, first_part_path

def get_absolute_path(relative_path):
    _, first_part_path = find_project_directory()
    return os.path.join(first_part_path, relative_path)


# Get the testsGenRvls script path
#script_path = os.path.abspath(__file__)
#print(f"\nScript path : {script_path}\n")

# Split the script_path and take the absolute path and first part of script_path
#script_path_parts = script_path.split(os.path.sep)
#print(f"Script path parts : {script_path_parts}\n")

# Get the absolute path of the testsGenRvls script
#script_path_abs = os.path.sep + os.path.join(*script_path_parts[0:-1])
#print(f"\nScript path abs: {script_path_abs}\n")

# Find the index of 'NaxRiscv' in the path
#naxriscv_index = script_path_parts.index('NaxRiscv')
#naxriscv_index = script_path_parts.index('NaxRiscv')
#print(f"naxriscv_index : {naxriscv_index}\n") 

# Get the relative path of the testsGenRvls script
#script_path_rel = os.path.sep.join(script_path_parts[naxriscv_index:-1])
#print(f"\nScript path relative: {script_path_rel}\n")

# Extract the relevant part of the path until 'NaxRiscv'
#first_script_path = os.path.sep.join(script_path_parts[:naxriscv_index])
#print(f"First script path : {first_script_path}\n") 

# Path to the tests directory 
#tests_directory_relative = "NaxRiscv/src/test/python/naxriscv"
# Create the absolute path to the tests directory
#tests_directory_abs = os.path.join(first_script_path, tests_directory_relative)
#print(f"Absolute tests directory: {tests_directory_abs}\n")

def makefile_include_testsrvls():
    try:
        project_directory, _ = find_project_directory()
        #print(f"\nMakefile must be created at {project_directory} ")
        # Set path to Makefile
        makefile_path = os.path.join(project_directory, "Makefile")
        #print(f"Makefile path { makefile_path} ")
        # Create the Makefile if it does not exist, otherwise replace it
        with open(makefile_path, "w+") as f:
            f.write("\n-include testsRvls.mk\n")
            #print(f"\nMakefile created at {makefile_path} and the path to testsRvls.mk has been added.\n")
    except Exception as e:
        print(f"An error occurred: {e}")


def all_testsRvls():
    try:
        tests_directory_abs = get_absolute_path("src/test/python/naxriscv")
        all_testsrvls_mk_path = os.path.join(tests_directory_abs, "all_testsRvls.mk")
        #print(f"All MK in one : {all_testsrvls_mk_path}\n")
        project_directory, _ = find_project_directory()
        include_line = f"\n-include {os.path.join(project_directory, 'testsRvls.mk')}\n"
        
        if not os.path.exists(all_testsrvls_mk_path):
            # If the file does not exist, it is created
            with open(all_testsrvls_mk_path, "w") as f:
                f.write(include_line)
            #print(f"File {all_testsrvls_mk_path} created.\n")
        else:
            # If the file exists, read its contents
            with open(all_testsrvls_mk_path, "r") as f:
                file_content = f.read()

            # Filter lines that appear to contain paths
            path_lines = [line for line in file_content.splitlines() if '-include' in line]

            # If more than 8 paths are present or just one path, replace the file
            if len(path_lines) > 8 :
                with open(all_testsrvls_mk_path, "w") as f:
                    f.write(include_line)
                #print(f"File {all_testsrvls_mk_path} replaced with only one include.\n")
            else:
                if include_line not in file_content:
                    with open(all_testsrvls_mk_path, "a") as f:
                        f.write(include_line)
                    #print(f"Adding sub-makefile path to {all_testsrvls_mk_path}.\n")
                #else:
                    #print(f"Sub-makefile already present in {all_testsrvls_mk_path}.\n")
    except Exception as e:
        print(f"An error occurred: {e}")

makefile_include_testsrvls()
all_testsRvls()

#makefile_include_testsrvls()

#def all_testsRvls():
#    # Vérifie si le fichier all_testsRvls.mk existe dans le répertoire de tests
#    all_testsrvls_mk_path = os.path.join(tests_directory_abs, "all_testsRvls.mk")
#    print(f"All MK in one : {all_testsrvls_mk_path}\n")
#
#    if not os.path.exists(all_testsrvls_mk_path):
#        # If the file does not exist, create it
#        with open(all_testsrvls_mk_path, "w") as f:
#            f.write(f"\n-include {script_path_abs}/testsRvls.mk\n")
#        print(f"File {all_testsrvls_mk_path} created.\n")
#    else:
#        # If the file exists, check if the line already exists
#        with open(all_testsrvls_mk_path, "r") as f:
#            file_content = f.read()
#        if f"\n-include {script_path_abs}/testsRvls.mk\n" not in file_content:
#            # If the line doesn't exist, add it at the end
#            with open(all_testsrvls_mk_path, "a") as f:
#                f.write(f"-include {script_path_abs}/testsRvls.mk\n")
#            print(f"Adding sub-makefile path to {all_testsrvls_mk_path}.\n")
#        else:
#            print(f"Sub-makefile already present in {all_testsrvls_mk_path}.\n")

#all_testsRvls()

def get_simulation_command():
    # Build the execution command line 
    command_list = [
        "sbt",
        "\"runMain naxriscv.platform.tilelinkdemo.SocSim",
        "--xlen", str(xlen),
        "--nax-count", str(naxCount)
    ]
    if rvc:
        command_list.append("--withRvc")
    if rvf:
        command_list.append("--withFloat")
    if rvd:
        command_list.append("--withDouble")
    #if dualSim:
    #    command_list.append("--dual-sim")
    #if traceIt:
    #    command_list.append("--trace")
    if not withRvls:
        command_list.append("--no-rvls")
    if not withL2:
        command_list.append("--no-l2")
    command_list.extend([
        "--workspace-path", str(workspacePath),
        "--workspace-name", str(workspaceName),
        "--workspace-output-dir", str(workspaceOutputDir)
    ])
    # Use "${ARGS}" to add additional arguments when running the script
    command_list.append("${ARGS}")

    return command_list

with open('testsRvls.mk', 'w') as f:

    def rvTest(name, elf_list=None, elf=None, passs="pass", start="test_2", startAdd = 0):
        # Sim Workspace Directory
        #simWorkspaceDir = riscv_tests
        # Get the basic command to start the SoC simulation
        command_list = get_simulation_command()
        if dualSim:
            command_list.append("--dual-sim")
        if traceIt:
            command_list.append("--trace")
        # Build the execution command line 
        command_list += [
            "--workspace-output-sub-dir", "riscv-tests/"+name,
            "--start-symbol", start,
            "--pass-symbol", str(passs),
            "--start-add", str(startAdd)
        ]
        config_rule = name+"_"+config_name
        f.write(f"{config_rule}:\n")
        rules.append(config_rule)
        testsFast.append(config_rule)
        # Define output directory
        outputDir = f"{workspacePath}/{workspaceName}/{workspaceOutputDir}/riscv-tests/{name}/"
        # Load ELF files and write test output rules
        if not elf_list:
            outputDirectory = outputDir + elf
            ouputs.append(outputDirectory)
            ouputsFast.append(outputDirectory)
            #f.write(f"{outputDirectory}/PASS:\n")
            testsPass.append(outputDirectory +"/PASS")
            testsFail.append(outputDirectory +"/FAIL")
            # Add ELF list to command line
            command_list.append("--load-elf")
            command_list.append(f" {naxriscv_software}/riscv-tests/{elf}")
        else:
            for index, elf in enumerate(elf_list):
                outputDirectory = outputDir + elf
                ouputs.append(outputDirectory)
                ouputsFast.append(outputDirectory)
                #f.write(f"{outputDirectory}/PASS:\n")
                testsPass.append(outputDirectory +"/PASS")
                testsFail.append(outputDirectory +"/FAIL")
                # Add ELF list to command line
                command_list.append("--load-elf")
                command_list.append(f" {naxriscv_software}/riscv-tests/{elf}")
        # Join the list to create the chain
        command_string = "\t" + " ".join(command_list)
        # Writing string to file
        f.write(command_string)
        f.write(f"\"\n\n")

    def rvArch(name, elf_list=None, elf=None, xlen=xlen, naxCount = 1, passs="pass", dualSim=False, traceIt=False, withRvls=True, withL2=True):
        # Get the basic command to start the SoC simulation
        command_list = get_simulation_command()
        if dualSim:
            command_list.append("--dual-sim")
        if traceIt:
            command_list.append("--trace")
        # Build the execution command line 
        command_list += [
            "--workspace-output-sub-dir", "riscv-arch-test/"+name,
            "--pass-symbol", str(passs)
        ]
        config_rule = name+"_"+config_name
        f.write(f"{config_rule}:\n")
        rules.append(config_rule)
        testsFast.append(config_rule)
        # Define output directory
        #outputDir = "output/riscv_arch/" + name + "/"
        outputDir = f"{workspacePath}/{workspaceName}/{workspaceOutputDir}/riscv-arch-test/{name}/"

        # Load elf files and write test output rules
        if not elf_list:
            outputDirectory = outputDir + os.path.basename(elf)  # Use os.path.basename to get just the file name
            ouputs.append(outputDirectory)
            ouputsFast.append(outputDirectory)
            #f.write(f"{outputDirectory}/PASS:\n")
            testsPass.append(outputDirectory +"/PASS")
            testsFail.append(outputDirectory +"/FAIL")
            # Add ELF list to command line
            command_list.append("--load-elf")
            command_list.append(f" {naxriscv_software}/riscv-arch-test/{elf}.elf")
        else:
            for index, elf in enumerate(elf_list):
                outputDirectory = outputDir + os.path.basename(elf)  # Use os.path.basename to get just the file name
                ouputs.append(outputDirectory)
                ouputsFast.append(outputDirectory)
                #f.write(f"{outputDirectory}/PASS:\n")
                testsPass.append(outputDirectory +"/PASS")
                testsFail.append(outputDirectory +"/FAIL")
                # Add ELF list to command line
                command_list.append("--load-elf")
                command_list.append(f" {naxriscv_software}/riscv-arch-test/{elf}.elf")
        # Join the list to create the chain
        command_string = "\t" + " ".join(command_list)
        # Writing string to file
        f.write(command_string)
        f.write(f"\"\n\n")

    def fpuTest(testListName, name, vector, testId):
        # Get the basic command to start the SoC simulation
        command_list = get_simulation_command()
        if dualSim:
            command_list.append("--dual-sim")
        if traceIt:
            command_list.append("--trace")

        command_list += [
            "--workspace-output-sub-dir", "nax/fpu-test/"+testListName+"/"+name
        ]
        config_rule = name+"_"+config_name
        f.write(f"{config_rule}:\n")
        rules.append(config_rule)
        testsFpu.append(config_rule)
        # Define output directory
        #outputDir = "output/nax/fpu_test/" + name
        outputDir = f"{workspacePath}/{workspaceName}/{workspaceOutputDir}/nax/fpu-test/{testListName}/{name}/fpu_test"
        testsPass.append(outputDir +"/PASS")
        testsFail.append(outputDir +"/FAIL")
        # Write test output rules
        ouputs.append(outputDir)
        ouputsFpu.append(outputDir)
        testCount = int(0x50000*NAXRISCV_TEST_FPU_FACTOR)
        if "sqrt" in name:
            testCount //= 16
        if "div" in name:
            testCount //= 8
        # Build the execution command line 
        command_list += [
            "--load-bin", f"{naxriscv_software}/baremetal/fpu_test/vector/{vector}.bin,90000000",
            "--load-u32", f"{testCount},A0000000",
            "--load-u32", f"{testId},A0000004",
            "--load-elf", f"{naxriscv_software}/baremetal/fpu_test/build/{archLinux}/fpu_test.elf",
            "--pass-symbol", "pass"
        ]
        # Join the list to create the chain
        command_string = "\t" + " ".join(command_list)
        # Writing string to file
        f.write(command_string)
        f.write(f"\"\n\n")

    def fpuTest3():
        # Get the basic command to start the SoC simulation
        command_list = get_simulation_command()
        if dualSim:
            command_list.append("--dual-sim")
        if traceIt:
            command_list.append("--trace")
        # Define FPU test name
        name = "nax"
        command_list += [
            "--workspace-output-sub-dir", name
        ]
        config_rule = "fpu_test3"+"_"+config_name
        f.write(f"{config_rule}:\n")
        rules.append(config_rule)
        testsFpu.append(config_rule)
        
        # Define output directory
        outputDir = f"{workspacePath}/{workspaceName}/{workspaceOutputDir}/{name}/fpu_test3"
        # Define test output rules
        testsPass.append(outputDir +"/PASS")
        testsFail.append(outputDir +"/FAIL")
        ouputs.append(outputDir)
        ouputsFpu.append(outputDir)
        # Build the execution command line 
        command_list += [
            "--load-bin", f"{naxriscv_software}/baremetal/fpu_test/vector/f32.bin,90000000",
            "--load-elf", f"{naxriscv_software}/baremetal/fpu_test3/build/{archLinux}/fpu_test3.elf",
            "--pass-symbol", "pass"
            #"--fail-symbol", "fail"
        ]
        # Join the list to create the chain
        command_string = "\t" + " ".join(command_list)
        # Writing string to file
        #f.write(f"{outputDir}/PASS:\n")
        f.write(command_string)
        f.write(f"\"\n\n")

    def regularSoftware(name, path):
        # Get the basic command to start the SoC simulation
        command_list = get_simulation_command()
        if dualSim:
            command_list.append("--dual-sim")
        if traceIt:
            command_list.append("--trace")
        # Set workspace directory
        _name = "nax/"
        command_list += [
            "--workspace-output-sub-dir", _name
        ]
        config_rule = name+"_"+config_name
        f.write(f"{config_rule}:\n")
        rules.append(config_rule)
        if name != "fpu_test2":
            testsFast.append(config_rule) 
        else:  
            testsFpu.append(config_rule) 
            
        # Define output directory
        outputDir = f"{workspacePath}/{workspaceName}/{workspaceOutputDir}/{_name}/{name}"
        testsPass.append(outputDir +"/PASS")
        testsFail.append(outputDir +"/FAIL")
        # Define test output rules
        ouputs.append(outputDir)
        if name != "fpu_test2":
            ouputsFast.append(outputDir)
        else:
            ouputsFpu.append(outputDir)

        # Build the execution command line 
        command_list += [
            "--load-elf", f" {naxriscv_software}/{path}",
            "--start-symbol", "_start",
            "--pass-symbol", "pass",
            #"--fail-symbol", "fail"
            #"--no-putc-flush",
        ]
        # Join the list to create the chain
        command_string = "\t" + " ".join(command_list)
        # Writing string to file
        #f.write(f"{outputDir}/PASS:\n")
        f.write(command_string)
        f.write(f"\"\n\n")


    def regularBaremetal(name):
        regularSoftware(name, f"baremetal/{name}/build/{arch}/{name}.elf")
    
    if xlen == 64:
        
        rvTest("riscv64_tests", elf_list=riscv64_tests)
        
        rvTest("riscv64TestMemory", elf_list=riscv64TestMemory)
        rvTest("riscv64TestMul", elf_list=riscv64TestMul)
        rvTest("riscv64TestDiv", elf_list=riscv64TestDiv)
        rvTest("riscv64TestAmo", elf_list=riscv64TestAmo)
        
        if rvc:
            rvTest("riscv64TestRvc", elf_list=riscv64TestRvc, startAdd=-8)
        
        if rvf:
            rvTest("riscv64TestFloat", elf_list=riscv64TestFloat ,start = "_start")
        
        
        if rvd:
            rvTest("riscv64TestDouble", elf_list=riscv64TestDouble ,start = "_start")
        
        rvArch("riscvArch64i", elf_list=riscvArch64i, traceIt=traceIt)
        rvArch("riscvArch64M", elf_list=riscvArch64M, traceIt=traceIt)
        rvArch("riscvArch64Zifencei", elf_list=riscvArch64Zifencei, traceIt=traceIt)

        if rvc:
            rvArch("riscvArch64C", elf_list=riscvArch64C, traceIt=traceIt)

        if rvf:
            regularBaremetal("fpu_test2")
            fpuTest3()
            for e in fpuTestRvf32:
                fpuTest("fpuTestRvf32", e[1], e[2], e[0])
            if xlen == 64:
                for e in fpuTestRvf64:
                    fpuTest("fpuTestRvf64", e[1], e[2], e[0])

        if rvd:
            for e in fpuTestRvd32:
                fpuTest("fpuTestRvd32", e[1], e[2], e[0])
            if xlen == 64:
                for e in fpuTestRvd64:
                    fpuTest("fpuTestRvd64", e[1], e[2], e[0])


        for spec in nax64Software:
            regularSoftware(spec[0], spec[1])
        
    if xlen == 32:
        
        rvTest("riscv32_tests", elf_list=riscv32_tests)
        
        rvTest("riscvTestMemory", elf_list=riscvTestMemory)
        
        rvTest("riscvTestMul", elf_list=riscvTestMul)
        rvTest("riscvTestDiv", elf_list=riscvTestDiv)
        rvTest("riscvTestAmo", elf_list=riscvTestAmo)

        if rvc:
            rvTest("riscv32TestRvc", elf_list=riscv32TestRvc, startAdd=-8)
        
        if rvf:
            rvTest("riscv32TestFloat", elf_list=riscv32TestFloat, start = "_start")
        
        if rvd:
            rvTest("riscv32TestDouble", elf_list=riscv32TestDouble, start = "_start")
        
        rvTest("rv32ua-p-lrsc_1234", elf="rv32ua-p-lrsc", passs="test_5")
        rvTest("rv32ua-p-lrsc_6", elf="rv32ua-p-lrsc", start="test_6")
        
        if rvf:
            regularBaremetal("fpu_test2")
            fpuTest3()
            for e in fpuTestRvf32:
                fpuTest("fpuTestRvf32", e[1], e[2], e[0])
        
        if rvd:
            for e in fpuTestRvd32:
                fpuTest("fpuTestRvd32", e[1], e[2], e[0])
        
        rvArch("riscvArch32i", elf_list=riscvArch32i, traceIt=traceIt)
        rvArch("riscvArch32M", elf_list=riscvArch32M, traceIt=traceIt)
        rvArch("riscvArch32Zifencei", elf_list=riscvArch32Zifencei, traceIt=traceIt)
        
        if rvc:
            rvArch("riscvArch32C", elf_list=riscvArch32C, traceIt=traceIt)
        
        for spec in naxSoftware:
            regularSoftware(spec[0], spec[1])
        
    # Run buildroot
    for i in range(linux_count):
        # Get the basic command to start the SoC simulation
        command_list = get_simulation_command()
        #if dualSim:
        #    command_list.append("--dual-sim")
        #if traceIt:
        #    command_list.append("--trace")
        # Set workspace directory
        name = "nax/buildroot/run" + str(i)
        command_list += [
            "--workspace-output-sub-dir", name
        ]
        config_rule = "buildroot_run"+ str(i)+"_"+config_name
        f.write(f"{config_rule}:\n")
        rules.append(config_rule)
        testsBuildroot.append(config_rule)
        # Define output directory
        outputDir = f"{workspacePath}/{workspaceName}/{workspaceOutputDir}/{name}"
        # outputDir = "output/nax/buildroot/run" + str(i)
        testsPass.append(outputDir +"/PASS")
        testsFail.append(outputDir +"/FAIL")
        # Define test output rules
        ouputs.append(outputDir)
        ouputsBuildroot.append(outputDir)

        imagePath = f"{naxriscv_software}/buildroot/images/" + archLinux

        # Build the execution command line 
        command_list += [
            #"--seed {i}"                  ,
            #"--name buildroot_run{i}"    ,
            "--load-bin", f"{imagePath}/fw_jump.bin,80000000",
            "--load-bin", f"{imagePath}/linux.dtb,80F80000" ,
            "--load-bin", f"{imagePath}/Image,80400000" ,
            "--load-bin", f"{imagePath}/rootfs.cpio,81000000", 
            #"--no-stdin"                  ,
            #"--no-putc-flush"          ,
            #"--seed={str(random.randint(0, 100000000))}" ,
            "--getc", "buildroot,login" ,
            "--putc", "root",
            "--getc", "#" ,
            "--putc", "cat,/proc/cpuinfo",
            "--getc", "#" ,
            "--putc", "echo,1+2+3*4,|,bc" ,
            "--getc", "#",
            "--putc", "micropython" ,
            "--getc", ">>>," ,
            "--putc", "import,math" ,
            "--getc", ">>>," ,
            "--putc", "math.sin(math.pi/4)" ,
            "--getc", ">>>," ,
            "--putc", "from,sys,import,exit" ,
            "--getc", ">>>," ,
            "--putc", "exit()" ,
            "--getc", "#" ,
            "--putc", "ls,/" ,
            "--getc", "#",
            "--putc", "echo" ,
            "--success"
        ]
        # Join the list to create the chain
        command_string = "\t" + " ".join(command_list)
        # Writing string to file
        #f.write(f"{outputDir}/PASS:\n")
        f.write(command_string)
        f.write(f"\"\n\n")      

    #f.write("OUTPUT_DIRS := \\\n")
    #for outputDir in ouputs:
    #    f.write(f"\t{outputDir} \\\n")

    # Start writing the test report rule
    f.write(f"""test-{config_name}-report:\n""")
    # Add the command to echo that testing has started
    f.write(f"""\t@echo "Checking tests..."\n""")

    # Write the TESTS_COUNT variable based on the number of tests in the 'tests' list
    f.write(f"""\t@TESTS_COUNT={len(ouputs)} && \\\n""")

    # Find the PASS files and store them in PASS_FILES, then count them
    f.write(f"""\tPASSED_FILES=$$(find {workspacePath}/{workspaceName}/{workspaceOutputDir}/ -name PASS 2>/dev/null) && \\\n""")

    # Find the FAIL files and store them in FAIL_FILES, then count them
    f.write(f"""\tFAILED_FILES=$$(find {workspacePath}/{workspaceName}/{workspaceOutputDir}/ -name FAIL 2>/dev/null) && \\\n""")

    # Count the number of PASS files found
    f.write(f"""\tPASSED=$$(echo "$$PASSED_FILES" | grep -v '^$$' | wc -l) && \\\n""")

    # Count the number of FAIL files found
    f.write(f"""\tFAILED=$$(echo "$$FAILED_FILES" | grep -v '^$$' | wc -l) && \\\n""")
    
    # Calculation of unexecuted tests
    f.write(f"""\tNOT_EXECUTED=$$((TESTS_COUNT - PASSED - FAILED)) && \\\n""")

    # Display the number of passed tests out of the total count
    f.write(f"""\techo "$$PASSED/$$TESTS_COUNT tests passed" && \\\n""")

    # Only display the "tests failed" and "tests not executed" if FAILED > 0 or NOT_EXECUTED > 0
    f.write(f"""\tif [ $$FAILED -gt 0 ] || [ $$NOT_EXECUTED -gt 0 ]; then \\\n""")
    #f.write(f"""\t\tif [ $$FAILED -gt 0 ]; then \\\n""")
    f.write(f"""\t\techo "$$FAILED/$$TESTS_COUNT tests failed"; \\\n""")
    #f.write(f"""\t\tfi; \\\n""")
    #f.write(f"""\t\tif [ $$NOT_EXECUTED -gt 0 ]; then \\\n""")
    f.write(f"""\t\techo "$$NOT_EXECUTED/$$TESTS_COUNT tests not executed"; \\\n""")
    #f.write(f"""\t\tfi; \\\n""")
    f.write(f"""\tfi\n""")

    # Add a couple of line breaks for clarity in the Makefile
    f.write(f"\n\n")


    f.write(f"""test-{config_name}-all:\n""") 
    for rule in rules:
        f.write(f"\t$(MAKE) {rule}\n")
    f.write(f"\n\n")
    
    f.write(f"""test-{config_name}-fast:\n""")
    for rule in testsFast:
        f.write(f"\t$(MAKE) {rule}\n")
    f.write(f"\n\n")
    
    if testsFpu:
        f.write(f"""test-{config_name}-fpu:\n""")
        for rule in testsFpu:
            f.write(f"\t$(MAKE) {rule}\n")
        f.write(f"\n\n")

    if testsBuildroot:
        f.write(f"""test-{config_name}-buildroot:\n""")
        for rule in testsBuildroot:
            f.write(f"\t$(MAKE) {rule}\n")
        f.write(f"\n\n")

    
    cleanPath = os.path.join(workspacePath, workspaceName)
    keep_files = ["Makefile", "testsRvls.mk", "testsGenRvls.py"]
    # Generate the clean rule
    f.write(f"""test-{config_name}-clean-all:\n""")
    f.write(f"\trm -rf {os.path.join(cleanPath, 'logs')}\n")
    f.write(f"\trm -rf {os.path.join(cleanPath, 'rtl')}\n")
    f.write(f"\trm -rf {os.path.join(cleanPath, 'verilator')}\n")
    f.write(f"\trm -rf {os.path.join(cleanPath, 'waves')}\n")
    f.write(f"\trm -f {os.path.join(cleanPath, 'verilatorScript.sh')}\n")

    f.write(f"\n\n")

    f.write(f"""test-{config_name}-clean:\n""")
    f.write(f"""\trm -rf {" ".join(ouputs)}\n""")

    f.write(f"\n\n")

    f.write(f"""test-{config_name}-fast-clean:\n""")
    f.write(f"""\trm -rf {" ".join(ouputsFast)}\n""")

    f.write(f"\n\n")

    if testsFpu:
        f.write(f"""test-{config_name}-fpu-clean:\n""")
        f.write(f"""\trm -rf {" ".join(ouputsFpu)}\n""")

    f.write(f"\n\n")

    if testsBuildroot:
        f.write(f"""test-{config_name}-buildroot-clean:\n""")
        f.write(f"""\trm -rf {" ".join(ouputsBuildroot)}\n""")

    f.write(f"\n\n")

    
# print(tests)


# for test in tests:
#     subprocess.run(test)