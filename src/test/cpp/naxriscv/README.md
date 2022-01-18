# Setup the repos :

```shell
git clone https://github.com/SpinalHDL/SpinalHDL.git --recursive
git clone https://github.com/SpinalHDL/NaxRiscv.git --recursive
cd NaxRiscv
export NAXRISCV=${PWD}
```

# Building riscv-isa-sim (spike)

```shell
cd $NAXRISCV/ext/riscv-isa-sim
mkdir build
cd build
../configure --prefix=$RISCV --enable-commitlog 
make -j$(nproc)
g++ --shared -L. -Wl,--export-dynamic -L/usr/lib/x86_64-linux-gnu  -Wl,-rpath,/lib  -o package.so spike.o  libspike_main.a  libriscv.a  libdisasm.a  libsoftfloat.a  libfesvr.a  libfdt.a -lpthread -ldl -lboost_regex -lboost_system -lpthread  -lboost_system -lboost_regex
```

# Install ELFIO

```
git clone https://github.com/serge1/ELFIO.git
cd ELFIO
sudo cp -R elfio /usr/include
```

# Generate NaxRiscv

```shell
cd $NAXRISCV
sbt "runMain naxriscv.Gen"
```

# Compile the simulator

```shell
cd $NAXRISCV/src/test/cpp/naxriscv
make compile TRACE=yes
```

# How to use the simulator 

```shell
obj_dir/VNaxRiscv --help
--help                  : Print this
--load-hex              : Load a hex file in the simulation memory
--load-elf              : Load a elf file in the simulation memory
--start-symbol=SYMBOL   : Force the CPU to boot at the given elf symbol
--pass-symbol=SYMBOL    : The simulation will pass when the given elf symbol execute
--fail-symbol=SYMBOL    : The simulation will fail when the given elf symbol execute
--output-dir=DIR        : Path to where every traces will be written
--name=STRING           : Test name reported when on exit (not very useful XD)
--timeout=INT           : Simulation time before failure (~number of cycles x 2)
--progress=PERIOD       : Will print the simulation speed each period seconds
--seed=INT              : Seed used to initialize randomizers
--trace                 : Enable FST wave capture
--trace-start-time=INT  : Add a time to which the FST should start capturing
--trace-stop-time=INT   : Add a time to which the FST should stop capturng
--trace-sporadic=RATIO  : Specify that periodically the FST capture a bit of the wave
--trace-ref             : Store the spike execution traces in a file
--stats-print           : Print some stats about the CPU execution at the end of the sim
--stats-print-all       : Print all the stats possible (including which branches had miss)
--stats-start-symbol=SY : Specify at which elf symbol the stats should start capturing
--stats-stop-symbol=SYM : Specify at which elf symbol the stats should stop capturing
--stats-toggle-symbol=S : Specify at which elf symbol the stats should change its capture state
--trace-gem5            : Enable capture of the pipeline timings as a gem5 trace, readable with github konata
```

Here is a konata visualisation of the trace produced by --trace-gem5 when running a simple memory copy loop hitting a cache miss on a dual issue pipeline: 

![alt text](assets/konata.png "Konata")

# Run the simulation

```shell
cd $NAXRISCV/src/test/cpp/naxriscv
./obj_dir/VNaxRiscv --timeout 100000 --load-elf ../../../../ext/NaxSoftware/baremetal/play/build/play.elf --pass-symbol=pass
```

# Run a riscv-test

```shell
cd $NAXRISCV/src/test/cpp/naxriscv
./obj_dir/VNaxRiscv \
--load-elf ../../../../ext/NaxSoftware/riscv-tests/rv32ui-p-addi \
--start-symbol test_2 \
--pass-symbol pass \
--fail-symbol fail \
--timeout 10000 
```

# Run all tests

```shell
cd $NAXRISCV/src/test/cpp/naxriscv
./testsGen.py
make compile test-clean
make test-all -j$(nproc); make test-report
```