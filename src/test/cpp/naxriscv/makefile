#make clean all TOPLEVEL_FILE=../../../../NaxRiscv.v TRACE=yes

GEN_FOLDER?=../../../..
NAX_VERILOG?=${GEN_FOLDER}/NaxRiscv.v
NAX_HEADER?=${GEN_FOLDER}/nax.h

TRACE?=no
LOCKSTEP?=yes
ADDCFLAGS += -CFLAGS -Iobj_dir

ifeq ($(DEBUG),yes)
	ADDCFLAGS += -CFLAGS -O0 -CFLAGS -g
else
	ADDCFLAGS += -CFLAGS -O3  -O3
endif

ifeq ($(TRACE),yes)
	VERILATOR_ARGS += --trace-fst
	ADDCFLAGS += -CFLAGS -DTRACE
endif


ifeq ($(LOCKSTEP),yes)
	ADDCFLAGS += -CFLAGS -DLOCKSTEP
endif


all: clean run

run: compile
	./obj_dir/VNaxRiscv

verilate: ${NAX_VERILOG}
	verilator -cc  ${NAX_VERILOG}  -O3 -CFLAGS -std=c++11 -LDFLAGS -pthread  ${ADDCFLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH --x-assign unique --exe main.cpp

nax.h: ${NAX_HEADER}
	cp ${NAX_HEADER} nax.h

compile: verilate nax.h
	make  -j${THREAD_COUNT} -C obj_dir/ -f VNaxRiscv.mk VNaxRiscv
 	
clean:
	rm -rf obj_dir
 	
