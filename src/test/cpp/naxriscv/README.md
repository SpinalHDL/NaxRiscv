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
git clone git clone https://github.com/serge1/ELFIO.git
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
cd $NAXRISCV/src/cpp/test/naxriscv
make compile TRACE=yes
```

# Run the simulation

```shell
cd $NAXRISCV/src/cpp/test/naxriscv
./obj_dir/VNaxRiscv --timeout 1000 --load_elf ../../../../ext/NaxSoftware/baremetal/play/build/play.elf
```

# Run a riscv-test

```shell
cd $NAXRISCV/src/cpp/test/naxriscv
./obj_dir/VNaxRiscv \
--load_elf ../../../../ext/NaxSoftware/riscv-tests/rv32ui-p-addi \
--start_symbol test_2 \
--pass_symbol pass \
--fail_symbol fail \
--timeout 10000 
```

# Run all tests

```shell
cd $NAXRISCV/src/cpp/test/naxriscv
./testsGen.py
make compile test_clean test_all test_report
```