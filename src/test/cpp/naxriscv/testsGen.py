#!/usr/bin/env python3

import subprocess

# class Test:
#     def __init__(self):
#         pass


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

naxSoftware = [
	["lsu", "baremetal/lsu/build/lsu.elf"],
	["dhrystone", "baremetal/dhrystone/build/dhrystone.elf"],
]


tests = []

with open('tests.mk', 'w') as f:
    for name in riscv_tests + riscvTestMemory:
        outputDir = "output/riscv_tests/" + name
        rule = outputDir +"/PASS"
        tests.append(rule)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", name,
            "--output_dir", outputDir,
            "--load_elf", f"../../../../ext/NaxSoftware/riscv-tests/{name}",
            "--start_symbol", "test_2",
            "--pass_symbol", "pass",
            "--fail_symbol", "fail",
            "--timeout", "10000"
        ]))
        f.write(f"\n\n")

    for spec in naxSoftware:
        outputDir = "output/nax/" + spec[0]
        rule = outputDir +"/PASS"
        tests.append(rule)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", spec[0],
            "--output_dir", outputDir,
            "--load_elf", f"../../../../ext/NaxSoftware/{spec[1]}",
            "--start_symbol", "_start",
            "--pass_symbol", "pass",
            "--fail_symbol", "fail"
        ]))
        f.write(f"\n\n")


    f.write(f"""TESTS_COUNT={len(tests)}\n""")

    f.write(f"""test_report:\n""")
    for test in tests:
        f.write(f"""\t@[ ! -f {test} ] && echo "{test} didn't passed"  || true\n""")
    f.write(f"""\t@PASSED=$(shell find {" ".join(tests)} | wc -l) && echo $$PASSED/$(TESTS_COUNT) passed\n""")  #; [ $PASSED $(TESTS_COUNT) ] && true || false
    f.write(f"\n\n")

# 	@PASSED=$(shell find output/riscv_tests/rv32ui-p-lui/PASS output/riscv_tests/rv32ui-p-auipc/PASS output/riscv_tests/rv32ui-p-jal/PASS output/riscv_tests/rv32ui-p-jalr/PASS output/riscv_tests/rv32ui-p-beq/PASS output/riscv_tests/rv32ui-p-bge/PASS output/riscv_tests/rv32ui-p-bgeu/PASS output/riscv_tests/rv32ui-p-blt/PASS output/riscv_tests/rv32ui-p-bltu/PASS output/riscv_tests/rv32ui-p-bne/PASS output/riscv_tests/rv32ui-p-add/PASS output/riscv_tests/rv32ui-p-addi/PASS output/riscv_tests/rv32ui-p-and/PASS output/riscv_tests/rv32ui-p-andi/PASS output/riscv_tests/rv32ui-p-or/PASS output/riscv_tests/rv32ui-p-ori/PASS output/riscv_tests/rv32ui-p-sll/PASS output/riscv_tests/rv32ui-p-slli/PASS output/riscv_tests/rv32ui-p-slt/PASS output/riscv_tests/rv32ui-p-slti/PASS output/riscv_tests/rv32ui-p-sra/PASS output/riscv_tests/rv32ui-p-srai/PASS output/riscv_tests/rv32ui-p-srl/PASS output/riscv_tests/rv32ui-p-srli/PASS output/riscv_tests/rv32ui-p-sub/PASS output/riscv_tests/rv32ui-p-xor/PASS output/riscv_tests/rv32ui-p-xori/PASS output/riscv_tests/rv32ui-p-lb/PASS output/riscv_tests/rv32ui-p-lbu/PASS output/riscv_tests/rv32ui-p-lh/PASS output/riscv_tests/rv32ui-p-lhu/PASS output/riscv_tests/rv32ui-p-lw/PASS output/riscv_tests/rv32ui-p-sb/PASS output/riscv_tests/rv32ui-p-sh/PASS output/riscv_tests/rv32ui-p-sw/PASS output/nax/load/PASS | wc -l) && \
# 	echo $$PASSED/$(TESTS_COUNT) passed


    f.write(f"""test_all: {" ".join(tests)}\n""")
    f.write(f"\n\n")

    f.write(f"""test_clean:\n""")
    f.write(f"""\trm -f {" ".join(tests)}\n""")

    f.write(f"\n\n")



# print(tests)


# for test in tests:
#     subprocess.run(test)