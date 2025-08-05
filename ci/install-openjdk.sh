#!/bin/bash
# usage: ./install-openjdk.sh <version> <output-dir>

if [ ! -e "$2/openjdk/bin/java"  ]; then
    echo "Installing OpenJDK..."
    mkdir -p tmp ; rm -rf tmp/* ; cd tmp
    mkdir -p $2/openjdk
    wget https://builds.openlogic.com/downloadJDK/openlogic-openjdk/$1/openlogic-openjdk-$1-linux-x64.tar.gz
    tar -xvzf openlogic-openjdk-$1-linux-x64.tar.gz -C .
    mv openlogic-openjdk-$1-linux-x64/* $2/openjdk
else
    echo "Using OpenJDK from cached directory."
fi
