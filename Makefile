# Constants
PRJ_NAX=NaxRiscv
PROJECT?=$(PRJ_NAX)
# Dirs
MKFILE_PATH=$(abspath $(firstword $(MAKEFILE_LIST)))
CORE_DIR=$(dir $(MKFILE_PATH))
TOOLCHAIN_DIR=$(CORE_DIR)/toolchain
# Tools
SPIKE_DIR=$(CORE_DIR)/ext/riscv-isa-sim
RVLS_DIR=$(CORE_DIR)/ext/rvls
RISCV_TEST_DIR=$(CORE_DIR)/ext/NaxSoftware/riscv-tests
# Riscv gcc
RISCV_HOST=riscv64-unknown-elf
RISCV=$(TOOLCHAIN_DIR)/riscv-gnu-toolchain
RISCV_VERSION=409b951ba6621f2f115aebddfb15ce2dd78ec24f
RISCV_GCC=$(RISCV)/bin/$(RISCV_HOST)-gcc
RISCV_OBJCOPY=$(RISCV)/bin/$(RISCV_HOST)-objcopy
RISCV_OBJDUMP=$(RISCV)/bin/$(RISCV_HOST)-objdump
# Verilator
VERILATOR_VERSION_NAX=v4.216
VERILATOR_ROOT_NAX=$(TOOLCHAIN_DIR)/verilator-$(VERILATOR_VERSION_NAX)
# ELFIO
ELFIO_VERSION=d251da09a07dff40af0b63b8f6c8ae71d2d1938d # Avoid C++17
# SDL
LIBSDL_VERSION=60d1944e463da73f753661190d783961a9c5b764
# SBT
SBT_VERSION=1.7.1
# OpenJDK
OPENJDK_VERSION=11.0.15+10
OPENJDK_HOME=$(TOOLCHAIN_DIR)/openjdk
# Git links
GIT_URL_NAXRISCV=https://github.com/SpinalHDL/NaxRiscv
GIT_URL_SPINALHDL=https://github.com/SpinalHDL/SpinalHDL.git

# install toolchain ####################################

# install core
install-core:
	./ci/clone-submodules.sh $(CORE_DIR)

# install initial
install-toolchain-initial: install-core
	mkdir -p $(TOOLCHAIN_DIR)
	./ci/install-sbt.sh $(SBT_VERSION) $(TOOLCHAIN_DIR)
	./ci/install-openjdk.sh $(OPENJDK_VERSION) $(TOOLCHAIN_DIR)

# install toolchain
install-toolchain: install-toolchain-initial
	./ci/install-verilator.sh $(VERILATOR_VERSION_NAX) $(TOOLCHAIN_DIR)
	./ci/install-libsdl-elfio-spikespinalhdl.sh $(SPIKE_DIR) $(ELFIO_VERSION) $(LIBSDL_VERSION)
	./ci/install-rvls.sh $(RVLS_DIR)		

# build spike & rvls after modification
build-spike-rvls:
	./ci/build_spike_rvls.sh $(CORE_DIR)
	
# build regress simulator after modification
build-simulator:
	./ci/build_regress_simulator.sh $(CORE_DIR)

# All NaxRiscv targets implements machine, supervisor and user mode
TARGETS_NAX=("rv32imasu" "rv32imacsu" "rv32imafcsu" "rv32imafdcsu" "rv64imasu" "rv64imacsu" "rv64imafcsu" "rv64imafdcsu")

# Set default target if not specified
TARGET_NAX ?= rv64imafdcsu

# Define parameters for each target
# RV32
rv32imasu_PARAMS :=
rv32imacsu_PARAMS := --withRvc
rv32imafcsu_PARAMS := --withRvc --withFloat
rv32imafdcsu_PARAMS := --withRvc --withFloat --withDouble
# RV64
rv64imasu_PARAMS :=
rv64imacsu_PARAMS := --withRvc
rv64imafcsu_PARAMS := --withRvc --withFloat
rv64imafdcsu_PARAMS := --withRvc --withFloat --withDouble

# Get the parameters for the selected target
PARAMS := $($(TARGET_NAX)_PARAMS)

# Determine the command based on the target architecture (32-bit or 64-bit)
ifneq (,$(findstring 32,$(TARGET_NAX)))
    GEN_CMD := $(TOOLCHAIN_DIR)/sbt/bin/sbt "runMain naxriscv.Gen $(PARAMS)"
    XLEN := 32
else
    GEN_CMD := $(TOOLCHAIN_DIR)/sbt/bin/sbt "runMain naxriscv.Gen64 $(PARAMS)"
    XLEN := 64
endif

# RTL of NaxRiscv
.PHONY: $(PRJ_NAX).v
$(PRJ_NAX).v:
	echo " ";\
	echo "Selected target: $(TARGET_NAX)";\
	echo "Parameters: $(PARAMS)";\
	echo "Generation command: $(GEN_CMD)";\
	echo " ";\
	export JAVA_HOME=$(OPENJDK_HOME); \
		export NAXRISCV=$(CORE_DIR); \
		export PATH=$(OPENJDK_HOME)/bin:$(PATH); \
		$(MAKE) clean-gen; \
		cd $(CORE_DIR); \
		$(GEN_CMD); \

# Verilator model of NaxRiscv
.PHONY: src/test/cpp/naxriscv/obj_dir/VNaxRiscv
src/test/cpp/naxriscv/obj_dir/VNaxRiscv:$(PRJ_NAX).v
	make -C $(CORE_DIR)/src/test/cpp/naxriscv compile
		VERILATOR_ROOT=$(VERILATOR_ROOT_NAX)
		SPIKE=$(SPIKE_DIR)
		PATH=$(VERILATOR_ROOT_NAX)/bin:$(PATH)

verilate-$(PRJ_NAX): src/test/cpp/naxriscv/obj_dir/VNaxRiscv

# install
install:clean-install clean-toolchain install-toolchain
	@echo " "
	@echo "[SUCCESS] The entire toolchain is built with Success."
	@echo " "

# test execute #########################################
test-regression : 
	@echo "Testing with NaxRiscvRegression...."
	sbt "testOnly *.NaxRiscvRegression"

test-rvls : 
	@echo "Testing with NaxRiscvRvls...."
	sbt "testOnly *.NaxRiscvRvls"
# clean ################################################

clean-submodules:
	rm -rf $(CORE_DIR)/ext/*
	
clean-install:
	rm -rf $(CORE_DIR)/ext/*
	rm -rf $(CORE_DIR)/tmp
	rm -rf $(CORE_DIR)/.venv

clean-sim:
	rm -rf $(CORE_DIR)/.venv

clean-workspace:
	rm -rf $(CORE_DIR)/simWorkspace

clean-exec:
	rm -rf *.tmp
	rm -rf *.xml
	rm -rf *.tar.gz

clean-toolchain:
	rm -rf $(TOOLCHAIN_DIR)/verilator-$(VERILATOR_VERSION_NAX)
	rm -rf $(SPIKE_DIR)
	rm -rf $(RVLS_DIR)
	rm -rf $(CORE_DIR)/ext/SpinalHDL
	rm -rf $(CORE_DIR)/ext/NaxSoftware

clean-toolchain-all:
	rm -rf $(TOOLCHAIN_DIR)

clean-gen:
	rm -rf $(CORE_DIR)/src/test/cpp/naxriscv/obj_dir
	rm -f $(PRJ_NAX).v
	rm -f nax.h
	rm -f NaxRiscvSynt.v

# include ################################################
-include testsRvls.mk
-include src/test/python/naxriscv/all_testsRvls.mk