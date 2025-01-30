#!/bin/bash

# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

install_verilator(){
  sudo apt-get update
  sudo apt install -y git make autoconf g++ flex libfl-dev bison  # First time prerequisites
  git clone https://github.com/verilator/verilator.git   # Only first time
  unset VERILATOR_ROOT  # For bash
  cd verilator
  git pull        # Make sure we're up-to-date
  git checkout v4.216
  autoconf        # Create ./configure script
  ./configure --prefix ~/tools
  make -j4
  make install
  cd ..
}



install_NaxSoftware(){
  (cd $NAXRISCV/ext/NaxSoftware  && ./init.sh)
}

install_spike(){
  cd $NAXRISCV/ext/riscv-isa-sim
  mkdir build
  cd build
  ../configure --prefix=$RISCV --enable-commitlog --without-boost --without-boost-asio --without-boost-regex
  make -j4
  g++ --shared -L. -Wl,--export-dynamic -L/usr/lib/x86_64-linux-gnu  -Wl,-rpath,/lib  -o package.so spike.o  libspike_main.a  libriscv.a  libdisasm.a  libsoftfloat.a  libfesvr.a  libfdt.a -lpthread -ldl -lboost_regex -lboost_system -lpthread  -lboost_system -lboost_regex
  cp -f package.so ~/tools/spike.so
  cp -f config.h ~/tools/spike.h
  cp -f libspike_main.a ~/tools
  cp -f libriscv.a ~/tools
  cp -f libdisasm.a ~/tools
  cp -f libsoftfloat.a ~/tools
  cp -f libfesvr.a ~/tools
  cp -f libfdt.a ~/tools
}

install_elfio(){
  git clone https://github.com/serge1/ELFIO.git
  cd ELFIO
  git checkout d251da09a07dff40af0b63b8f6c8ae71d2d1938d # Avoid C++17
  sudo cp -R elfio /usr/include
  #export C_INCLUDE_PATH=${PWD}/elfio
  cd ..
}

install_packages(){
  sudo apt-get update
  sudo apt install -y zlib1g-dev libboost-all-dev libboost-dev libasio-dev device-tree-compiler libsdl2-2.0-0 libsdl2-dev
  sudo apt install -y git make autoconf build-essential flex libfl-dev bison help2man # First time prerequisites
}

install_uncached(){
  export NAXRISCV=${PWD}
  install_elfio
  install_NaxSoftware

  mkdir -p $NAXRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/spike.so $NAXRISCV/ext/riscv-isa-sim/build/package.so
  cp -f ~/tools/spike.h $NAXRISCV/ext/riscv-isa-sim/build/config.h
  cp -f ~/tools/libspike_main.a $NAXRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/libriscv.a $NAXRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/libdisasm.a $NAXRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/libsoftfloat.a $NAXRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/libfesvr.a $NAXRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/libfdt.a $NAXRISCV/ext/riscv-isa-sim/build
}

install_cached(){
  export NAXRISCV=${PWD}
  mkdir -p ~/tools
  (install_spike)
  (install_verilator)
}

install_all(){
  export NAXRISCV=${PWD}
  make install
}
