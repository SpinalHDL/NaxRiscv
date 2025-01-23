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

# Base directory
BASE_DIR=$1

# Check if RISCV environment variable is set
if [ -z "$RISCV" ]; then
    echo "Error: RISCV environment variable is not set."
    exit 1
fi

# Install ELFIO if not already installed
if [ ! -e "$BASE_DIR/ext/riscv-isa-sim/include/elfio/elfio.hpp" ]; then
    echo "Installing ELFIO..."
    mkdir -p $BASE_DIR/tmp
    cd $BASE_DIR/tmp
    git clone https://github.com/serge1/ELFIO.git
    cd ELFIO
    git checkout master # or specify a specific version/tag
    mkdir -p $BASE_DIR/ext/riscv-isa-sim/include
    cp -R elfio $BASE_DIR/ext/riscv-isa-sim/include
    cd $BASE_DIR
    rm -rf $BASE_DIR/tmp
else
    echo "ELFIO is already installed."
fi

# Install SDL if not already installed
if [ ! -e "$BASE_DIR/ext/riscv-isa-sim/include/SDL2/SDL.h" ]; then
    echo "Installing SDL..."
    mkdir -p $BASE_DIR/tmp
    cd $BASE_DIR/tmp
    git clone https://github.com/libsdl-org/SDL
    cd SDL
    git checkout release-2.28.5 # or specify a specific version/tag
    mkdir -p build
    cd build
    ../configure --prefix="$BASE_DIR/ext/riscv-isa-sim/"
    make -j$(nproc)
    make install
    cd $BASE_DIR
    rm -rf $BASE_DIR/tmp
else
    echo "SDL is already installed."
fi

# Check if the 'build' directory exists, create it if it doesn't
if [ ! -d "$BASE_DIR/ext/riscv-isa-sim/build/" ]; then
    mkdir -p $BASE_DIR/ext/riscv-isa-sim/build/
fi

# Run the configuration script without Boost libraries
cd $BASE_DIR/ext/riscv-isa-sim/build/
if [ -d "$BASE_DIR/ext/riscv-isa-sim/build/" ]; then
    make clean
fi
../configure --prefix=${RISCV} --without-boost --without-boost-asio --without-boost-regex

# Compile the project using all available cores
make -j$(nproc)

# Clean previous build artifacts for 'rvls'
if [ ! -e "$BASE_DIR/ext/rvls/build/apps/rvls" ]; then
    echo "Installing RVLS..."
    cd $BASE_DIR/ext/rvls/
else
    cd $BASE_DIR/ext/rvls/
    make clean
fi

# Compile the project using all available cores
make -j$(nproc)

echo "Installation and build of ELFIO, SDL, Spike, and RVLS completed."