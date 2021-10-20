#make clean all TOPLEVEL_FILE=../../../../NaxRiscv.v TRACE=yes


TRACE?=no

ifeq ($(TRACE),yes)
	VERILATOR_ARGS += --trace-fst
	ADDCFLAGS += -CFLAGS -DTRACE
endif


all: clean run

run: compile
	./obj_dir/VNaxRiscv

verilate: ${TOPLEVEL_FILE}
	verilator -cc  ${TOPLEVEL_FILE}  -O3 -CFLAGS -std=c++11 -LDFLAGS -pthread  ${ADDCFLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH --x-assign unique --exe main.cpp
 	
compile: verilate
	make  -j${THREAD_COUNT} -C obj_dir/ -f VNaxRiscv.mk VNaxRiscv
 	
clean:
	rm -rf obj_dir
 	
