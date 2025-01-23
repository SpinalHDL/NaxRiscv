# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

NAXRISCV_REPO?=$(realpath ../../../..)
NAXRISCV_GEN_FOLDER?=${NAXRISCV_REPO}
NAXRISCV_VERILOG?=${NAXRISCV_GEN_FOLDER}/NaxRiscv.v
NAXRISCV_HEADER?=${NAXRISCV_GEN_FOLDER}/nax.h

SPIKE?=${NAXRISCV_REPO}/ext/riscv-isa-sim
SPIKE_BUILD=$(realpath ${SPIKE}/build)
SPIKE_INCLUDE=$(realpath ${SPIKE}/include)
SPIKE_LIB=$(realpath ${SPIKE}/lib)

TRACE?=yes
#LOCKSTEP?=yes
ALLOCATOR_CHECKS?=no
ADDCFLAGS += -CFLAGS -Iobj_dir
ADDCFLAGS += -CFLAGS -I$(realpath ${SPIKE}/riscv)
ADDCFLAGS += -CFLAGS -I$(realpath ${SPIKE}/fesvr)
ADDCFLAGS += -CFLAGS -I$(realpath ${SPIKE}/softfloat)
ADDCFLAGS += -CFLAGS -I$(realpath ${SPIKE_BUILD})
ADDCFLAGS += -CFLAGS -I$(realpath ${SPIKE_INCLUDE}) # to include SDL2 & elfio

ADDCFLAGS += -LDFLAGS -L$(realpath ${SPIKE_LIB})
ADDCFLAGS += -LDFLAGS -lSDL2

ADDCFLAGS += -CFLAGS -g
ADDCFLAGS += -CFLAGS -rdynamic

FLAGS += --autoflush
FLAGS += --output-split 5000
FLAGS += --output-split-cfuncs 500
FLAGS += --output-split-ctrace 500
FLAGS +=  --x-assign unique

ifeq ($(PROF_GEN),yes)
FLAGS += -CFLAGS -fprofile-generate -LDFLAGS -fprofile-generate
endif

ifeq ($(PROF_USE),yes)
FLAGS +=  -CFLAGS "-fprofile-use -fprofile-correction"
endif


SPIKE_OBJS:= libspike_main.a  libriscv.a  libdisasm.a  libsoftfloat.a  libfesvr.a  libfdt.a
SPIKE_OBJS:=$(addprefix ${SPIKE_BUILD}/,${SPIKE_OBJS})

SRCS=src/main.cpp
SRCS+=${SPIKE_OBJS} # g++ -shared -L. -Wl,--export-dynamic -L/usr/lib/x86_64-linux-gnu  -Wl,-rpath,/opt/riscv/lib  -o package.so spike.o  libspike_main.a  libriscv.a  libdisasm.a  libsoftfloat.a  libfesvr.a  libfdt.a -lpthread -ldl -lboost_regex -lboost_system -lpthread  -lboost_system -lboost_regex


#SRCS+=${SPIKE_BUILD}/libspike_main.a
#SRCS+=${SPIKE_BUILD}/libriscv.a
#SRCS+=${SPIKE_BUILD}/libdisasm.a
#SRCS+=${SPIKE_BUILD}/libsoftfloat.a
#SRCS+=${SPIKE_BUILD}/libfesvr.a
#SRCS+=${SPIKE_BUILD}/libfdt.a

ifeq ($(DEBUG),yes)
	ADDCFLAGS += -O0 -CFLAGS -O0 -CFLAGS -g
else
	ADDCFLAGS += -O3  -CFLAGS -O3  -O3
endif

ifeq ($(TRACE),yes)
	VERILATOR_ARGS += --trace-fst
	ADDCFLAGS += -CFLAGS -DTRACE
endif


ifeq ($(ALLOCATOR_CHECKS),yes)
	ADDCFLAGS += -CFLAGS -DALLOCATOR_CHECKS
endif

#ifeq ($(LOCKSTEP),yes)
#	ADDCFLAGS += -CFLAGS -DLOCKSTEP
#endif


LIBS +="-lpthread -ldl -lboost_regex -lboost_system  -lpthread  -lboost_system -lboost_regex"

all: clean run

run: compile
	./obj_dir/VNaxRiscv

verilate: ${NAXRISCV_VERILOG}
	verilator -cc  ${NAXRISCV_VERILOG}  -CFLAGS -std=c++14 -LDFLAGS -pthread  ${ADDCFLAGS}  ${FLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH --x-assign unique --exe ${SRCS}

src/nax.h: ${NAXRISCV_HEADER}
	cp ${NAXRISCV_HEADER} src/nax.h

compile: verilate src/nax.h
	make  -j${THREAD_COUNT} -C obj_dir/ -f VNaxRiscv.mk VNaxRiscv LIBS=${LIBS}
 	
clean:
	rm -rf obj_dir
 	
-include tests.mk