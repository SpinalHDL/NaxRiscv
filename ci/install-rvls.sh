#!/bin/bash

# Checks if no argument is given
if [ $# -eq 0 ]; then
    echo "Usage: ./install-rvls.sh <rvls-path>"
    exit 1
fi

if [ ! -e "$1/build/apps/rvls"  ]; then
    echo "Installing RVLS..."
    cd $1
	make -j$(nproc)
else
	echo "RVLS is already built and exists in $1/build/apps/."
fi
