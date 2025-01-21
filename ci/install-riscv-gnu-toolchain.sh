# usage: ./install-riscv-gnu-toolchain.sh <version> <output-dir> <extra-path>

#!/bin/bash

export PATH=$PATH:$3

if [ ! -e "$2/riscv-gnu-toolchain/bin/riscv64-unknown-elf-gcc"  ]; then
    echo "Installing RISC-V GNU GCC..."
    mkdir -p tmp ; rm -rf tmp/* ; cd tmp
    git clone https://github.com/riscv/riscv-gnu-toolchain 
    cd riscv-gnu-toolchain
    git checkout $1
    ./configure --prefix="$2/riscv-gnu-toolchain"
    make -j `nproc`
else
    echo "Using RISC-V GNU GCC from cached directory."
fi

