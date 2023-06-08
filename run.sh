#!/bin/bash
eval $(opam env)
PROJECT_HOME=$(pwd)
cd ${PROJECT_HOME}/backend/
mkdir -p build
cd build
export CXX=/usr/bin/clang++
export CC=/usr/bin/clang
cmake ../ && make
cd ${PROJECT_HOME}
dune exec vilangu
