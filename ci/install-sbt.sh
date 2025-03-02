# usage: ./install-sbt.sh <version> <output-dir> 

#!/bin/bash

if [ ! -e "$2/sbt/bin/sbt"  ]; then
    echo "Installing SBT..."
    mkdir -p tmp ; rm -rf tmp/* ; cd tmp
    wget https://github.com/sbt/sbt/releases/download/v$1/sbt-$1.tgz 
    tar -xvzf sbt-$1.tgz -C $2/ 
else
    echo "Using SBT from cached directory."
fi
