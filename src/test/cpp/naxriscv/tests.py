#!/usr/bin/env python3

import subprocess

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


riscvTestFloat = [
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


riscvTestDouble = [
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

tests = []

for name in riscv_tests + riscvTestMemory:
    tests.append([
        "obj_dir/VNaxRiscv",
        "--name", name,
        "--output_dir", "output/" + name,
        "--load_elf", f"../../../../ext/NaxSoftware/riscv-tests/{name}",
        "--start_symbol", "test_2",
        "--pass_symbol", "pass",
        "--fail_symbol", "fail",
        "--timeout", "10000"
    ])

print(tests)

for test in tests:
    subprocess.run(test)