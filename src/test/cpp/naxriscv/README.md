# Building riscv-isa-sim (spike)

```shell
cd ext/riscv-isa-sim
mkdir build
cd build
../configure --prefix=$RISCV --enable-commitlog 
make -j$(nproc)
g++ --shared -L. -Wl,--export-dynamic -L/usr/lib/x86_64-linux-gnu  -Wl,-rpath,/lib  -o package.so spike.o  libspike_main.a  libriscv.a  libdisasm.a  libsoftfloat.a  libfesvr.a  libfdt.a -lpthread -ldl -lboost_regex -lboost_system -lpthread  -lboost_system -lboost_regex
```

# Generate NaxRiscv

```shell
sbt "runMain naxriscv.Gen"
```

# Compile the simulator

```shell
make compile TRACE=yes
```

# Run the simulation

```shell
./obj_dir/VNaxRiscv --timeout 1000 --mem_hex ../../../../ext/NaxSoftware/baremetal/play/build/play.hex
```