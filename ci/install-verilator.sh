# usage: ./install-verilator.sh <version> <output-dir>

#!/bin/bash
set -e  # Stop script on error

VERILATOR_VERSION=$1
INSTALL_DIR=$2

# full path of the installation directory
VERILATOR_DIR="$INSTALL_DIR/verilator-$VERILATOR_VERSION"

if [ ! -e "$VERILATOR_DIR/bin/verilator" ]; then
    echo "Installing Verilator $VERILATOR_VERSION..."
    mkdir -p $INSTALL_DIR
    cd $INSTALL_DIR

    # Clone and build in the verilator toolchain directory
    git clone https://github.com/verilator/verilator.git $VERILATOR_DIR
    cd $VERILATOR_DIR
    git checkout $VERILATOR_VERSION

    # Generate the configuration script
    autoconf
    ./configure --prefix="$VERILATOR_DIR"
    make -j$(nproc)
    make install

    echo "Verilator successfully installed in $VERILATOR_DIR"
else
    echo "Verilator $VERILATOR_VERSION is already installed in $VERILATOR_DIR"
fi
