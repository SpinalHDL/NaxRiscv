#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Check if the correct number of arguments is provided (should be exactly 1)
if [ "$#" -ne 1 ]; then
    echo "Error: You must provide exactly one argument, which is the base directory path."
    echo "Tip: Set the base directory by running the following command:"
    echo "      export NAXRISCV=\$(pwd)"
    echo "Then, run the script from the NaxRiscv directory with:"
    echo "      $0 \$NAXRISCV"
    exit 1
fi

# Check if the 'build' directory exists, create it if it doesn't
if [ ! -d "ext/riscv-isa-sim/build/" ]; then
    mkdir -p ext/riscv-isa-sim/build/
fi

# Run the configuration script without Boost libraries
cd $1/ext/riscv-isa-sim/build/
if [ -d "ext/riscv-isa-sim/build/" ]; then
    make clean
fi
../configure --prefix=${RISCV} --without-boost --without-boost-asio --without-boost-regex
 

# Compile the project using all available cores
make -j$(nproc)

# Clean previous build artifacts for 'rvls'
if [ ! -e "$1/ext/rvls/build/apps/rvls"  ]; then
    echo "Installing RVLS..."
    cd $1/ext/rvls/
else
	cd $1/ext/rvls/
    make clean
fi

# Compile the project using all available cores
make -j$(nproc)