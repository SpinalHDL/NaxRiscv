# usage: ./install-verilator.sh <version> <output-dir>

#!/bin/bash

if [ ! -e "$2/verilator-$1/bin/verilator" ]; then
    echo "Installing Verilator..."
    mkdir -p tmp ; rm -rf tmp/* ; cd tmp
    git clone https://github.com/verilator/verilator.git
    unset VERILATOR_ROOT
    cd verilator
    git checkout $1
    mkdir -p $2/verilator-$1
    # copy scripts
    autoconf && ./configure --prefix="$2/verilator-$1" && make -j `nproc` 
    rm -rf ~/tools/verilator
    cp -r "$2/verilator-$1" ~/tools/verilator
else
    echo "Using Verilator from cached directory."
    # Vérification que ~/tools/verilator est à jour
    rm -rf ~/tools/verilator
    cp -r "$2/verilator-$1" ~/tools/verilator
fi
