#!/bin/bash

# Script to compile the NaxRiscv simulator

# Exit immediately if a command exits with a non-zero status
set -e

# Check if the correct number of arguments is provided (should be exactly 1)
if [ "$#" -ne 1 ]; then
    echo "Error: You must provide exactly one argument, which is the base root directory path."
    echo "Tip: Set the base directory by running the following command:"
    echo "      export NAXRISCV=\$(pwd)"
    echo "Then, run the script from the NaxRiscv directory with:"
    echo "      $0 \$NAXRISCV"
    exit 1
fi

# Navigate to the correct directory
cd $1/src/test/cpp/naxriscv || {
    echo "Error: Directory $1/src/test/cpp/naxriscv does not exist."
    exit 1
}

# Check if the obj_dir directory exists
if [ -d "$1/src/test/cpp/naxriscv/obj_dir" ]; then
    echo "Cleaning previous build..."
    make clean
fi

# Compile the simulator
echo "Compiling the simulator..."
make compile

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo "Compilation completed successfully."
else
    echo "Error: Compilation failed."
    exit 1
fi
