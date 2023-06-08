#!/bin/bash
eval $(opam env)
PROJECT_HOME=$(pwd)
cd ${PROJECT_HOME}/backend/
mkdir -p build
cd build
cmake ../ && make
cd ${PROJECT_HOME}
dune exec vilangu
