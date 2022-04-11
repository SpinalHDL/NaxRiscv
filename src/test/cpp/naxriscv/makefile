#make clean all TOPLEVEL_FILE=../../../../NaxRiscv.v TRACE=yes
NAXRISCV_REPO?=$(realpath ../../../..)
GEN_FOLDER?=${NAXRISCV_REPO}
NAX_VERILOG?=${GEN_FOLDER}/NaxRiscv.v
NAX_HEADER?=${GEN_FOLDER}/nax.h

SPIKE=${NAXRISCV_REPO}/ext/riscv-isa-sim
SPIKE_BUILD=${SPIKE}/build


TRACE?=yes
#LOCKSTEP?=yes
ALLOCATOR_CHECKS?=yes
ADDCFLAGS += -CFLAGS -Iobj_dir
ADDCFLAGS += -CFLAGS -I${SPIKE}/riscv
ADDCFLAGS += -CFLAGS -I${SPIKE}/fesvr
ADDCFLAGS += -CFLAGS -I${SPIKE}/softfloat
ADDCFLAGS += -CFLAGS -I${SPIKE_BUILD}


FLAGS += --autoflush
FLAGS += --output-split 5000
FLAGS += --output-split-cfuncs 500
FLAGS += --output-split-ctrace 500
FLAGS +=  --x-assign unique

SRCS=src/main.cpp
SRCS+=${SPIKE_BUILD}/package.so # g++ -shared -L. -Wl,--export-dynamic -L/usr/lib/x86_64-linux-gnu  -Wl,-rpath,/opt/riscv/lib  -o package.so spike.o  libspike_main.a  libriscv.a  libdisasm.a  libsoftfloat.a  libfesvr.a  libfdt.a -lpthread -ldl -lboost_regex -lboost_system -lpthread  -lboost_system -lboost_regex


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

verilate: ${NAX_VERILOG}
	verilator -cc  ${NAX_VERILOG}  -CFLAGS -std=c++14 -LDFLAGS -pthread  ${ADDCFLAGS}  ${FLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH --x-assign unique --exe ${SRCS}

nax.h: ${NAX_HEADER}
	cp ${NAX_HEADER} src/nax.h

compile: verilate nax.h
	make  -j${THREAD_COUNT} -C obj_dir/ -f VNaxRiscv.mk VNaxRiscv LIBS=${LIBS}
 	
clean:
	rm -rf obj_dir
 	
-include tests.mk