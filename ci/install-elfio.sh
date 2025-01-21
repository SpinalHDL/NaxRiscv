#!/bin/bash

# Checks if no argument is given
if [ $# -eq 0 ]; then
    echo "Usage: ./install-elfio.sh <version> <rvls-path>"
    exit 1
fi

if [ ! -e "$2/include/elfio/elfio.hpp" ]; then
    echo "Installing ELFIO..."
    mkdir -p tmp ; rm -rf tmp/* ; cd tmp
    git clone https://github.com/serge1/ELFIO.git
    cd ELFIO
    git checkout $1
    mkdir -p $2/include
    cp -R elfio $2/include
else
    echo "Using ELFIO from cached directory."
fi
