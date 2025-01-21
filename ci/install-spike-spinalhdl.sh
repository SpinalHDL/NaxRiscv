# usage   : ./install-spike-spinalhdl.sh <core_dir> <spike_nax_home>

#!/bin/bash

export PATH=$PATH:$2

if [ ! -e "$1/build/spike"  ]; then
	echo "Installing SpinalHDL/spike..."
	cd $1/
	mkdir -p build
	cd build
	../configure --prefix=$(RISCV)  --without-boost --without-boost-asio --without-boost-regex
	make -j$(nproc)
else
	echo "Spike is already built and exists in $1/build."
fi
