#!/bin/bash

# Checks if no argument is given
if [ $# -eq 0 ]; then
    echo "Usage: ./install-libsdl.sh <version> <rvls-path>"
    exit 1
fi

if [ ! -e "$2/include/SDL2/SDL.h" ]; then
    echo "Installing SDL..."
    mkdir -p tmp ; rm -rf tmp/* ; cd tmp
    git clone https://github.com/libsdl-org/SDL
    cd SDL
    git checkout $1
    mkdir -p build
    cd build
    ../configure --prefix="$2/"
    make -j `nproc`
    make install
else
    echo "Using SDL from cached directory."
fi
