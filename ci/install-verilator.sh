# usage: ./install-verilator.sh <version> <output-dir>

#!/bin/bash

if [ ! -e "$2/verilator-$1/bin/verilator" ]; then
    echo "Installing Verilator..."
    cd $2
    git clone https://github.com/verilator/verilator.git
    unset VERILATOR_ROOT
    mv verilator/ verilator-$1/
    git checkout $1
    # copy scripts
    autoconf
    ./configure --prefix="$2/verilator-$1"
    make -j$(nproc)
    cd ..
else
    echo "Using Verilator from cached directory."
fi
