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

arch="rv32im"

naxSoftware = [
	["lsu", "baremetal/lsu/build/lsu.elf"],
]

naxSoftwareRegular = [
    "machine", "supervisor", "dhrystone"
]


freertos = ["blocktim", "countsem", "EventGroupsDemo", "flop", "integer", "QPeek",
            "QueueSet", "recmutex", "semtest", "TaskNotify", "dynamic",
            "GenQTest", "PollQ", "QueueOverwrite", "QueueSetPolling", "sp_flop", "test1"]

for e in naxSoftwareRegular:
    naxSoftware.append([e, f"baremetal/{e}/build/{arch}/{e}.elf"])

for e in freertos:
    naxSoftware.append(["freertos/" + e, f"baremetal/freertosDemo/build/{e}/{arch}/freertosDemo.elf"])


naxSoftware.append(["coremark", f"baremetal/coremark/coremark_{arch}.elf"])

tests = []
testsFast = []
ouputs = []

with open('tests.mk', 'w') as f:

    def rvTest(name, elf=None, timeout=10000, passs="pass", start="test_2"):
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
            "--load-elf", f"../../../../ext/NaxSoftware/riscv-tests/{elf}",
            "--start-symbol", start,
            "--pass-symbol", passs,
            "--fail-symbol", "fail",
            "--timeout", str(timeout),
           "${ARGS}"
        ]))
        f.write(f"\n\n")



    for name in riscv_tests + riscvTestMemory + riscvTestMul + riscvTestDiv + riscvTestAmo:
        rvTest(name)


    rvTest("rv32ua-p-lrsc_1234", elf="rv32ua-p-lrsc", timeout=300000, passs="test_5")
    rvTest("rv32ua-p-lrsc_6", elf="rv32ua-p-lrsc", timeout=100000, start="test_6")


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
            "--load-elf", f"../../../../ext/NaxSoftware/riscv-arch-test/{elf}.elf",
            "--pass-symbol", passs,
            "--timeout", str(timeout),
           "${ARGS}"
        ]))
        f.write(f"\n\n")

    for name in riscvArch32i + riscvArch32M + riscvArch32Zifencei:
        rvArch(name)

    for spec in naxSoftware:
        outputDir = "output/nax/" + spec[0]
        rule = outputDir +"/PASS"
        tests.append(rule)
        ouputs.append(outputDir)
        f.write(f"{outputDir}/PASS:\n")
        f.write("\t" + " ".join([
            "obj_dir/VNaxRiscv",
            "--name", spec[0],
            "--output-dir", outputDir,
            "--load-elf", f"../../../../ext/NaxSoftware/{spec[1]}",
            "--start-symbol", "_start",
            "--pass-symbol", "pass",
            "--fail-symbol", "fail",
           "${ARGS}"
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


    f.write(f"""test-clean:\n""")
    f.write(f"""\trm -rf {" ".join(ouputs)}\n""")

    f.write(f"\n\n")



# print(tests)


# for test in tests:
#     subprocess.run(test)