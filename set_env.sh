#!/bin/sh

# paths
export SRC_ROOT=$(pwd)/src
export TEST_ROOT=$(pwd)/test
export BIN_ROOT=$(pwd)/bin

# tools
export CC=clang
export MAKE=make
export VALGRIND=valgrind
export CMP=cmp
export DOT=dot

# flags
export CFLAGS="-g -O0 -I$SRC_ROOT"
export VALGRIND_FLAGS="--error-exitcode=1 --leak-check=full -q"
export CMP_FLAGS="-s"
export DOT_FLAGS="-Tpng"
