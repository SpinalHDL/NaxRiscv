#!/bin/bash

# Global installation script for ELFIO, SDL, and Spike
# Usage: ./install-libsdl-elfio-spikespinalhdl.sh <spike_nax_home> <elfio_version> <libsdl_version>

# Check if the correct number of arguments is provided
if [ $# -ne 3 ]; then
    echo "Usage: $0 <spike_nax_home> <elfio_version> <libsdl_version>"
    exit 1
fi

SPIKE_NAX_HOME=$1
ELFIO_VERSION=$2
LIBSDL_VERSION=$3


# Create a temporary directory for downloads and builds
TMP_DIR=$(mktemp -d)
trap "rm -rf $TMP_DIR" EXIT

# Install ELFIO
if [ ! -e "$SPIKE_NAX_HOME/include/elfio/elfio.hpp" ]; then
    echo "Installing ELFIO..."
    cd $TMP_DIR
    git clone https://github.com/serge1/ELFIO.git
    cd ELFIO
    git checkout $ELFIO_VERSION
    mkdir -p $SPIKE_NAX_HOME/include
    cp -R elfio $SPIKE_NAX_HOME/include
else
    echo "Using ELFIO from cached directory."
fi

# Install SDL
if [ ! -e "$SPIKE_NAX_HOME/include/SDL2/SDL.h" ]; then
    echo "Installing SDL..."
    cd $TMP_DIR
    git clone https://github.com/libsdl-org/SDL
    cd SDL
    git checkout $LIBSDL_VERSION
    mkdir -p build
    cd build
    ../configure --prefix="$SPIKE_NAX_HOME/"
    make -j $(nproc)
    make install
else
    echo "Using SDL from cached directory."
fi

# Install Spike
if [ ! -e "$SPIKE_NAX_HOME/build/spike" ]; then
    echo "Installing SpinalHDL/spike..."
    cd $SPIKE_NAX_HOME
    mkdir -p build
    cd build
    ../configure --prefix=$RISCV --without-boost --without-boost-asio --without-boost-regex
    make -j$(nproc)
else
    echo "Spike is already built and exists in $SPIKE_NAX_HOME/build."
fi

echo "Installation of ELFIO, SDL, and Spike completed."