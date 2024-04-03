#!/bin/sh

# paths
export SRC_ROOT=$(pwd)/src
export TEST_ROOT=$(pwd)/test
export BIN_ROOT=$(pwd)/bin

# tools
export CC=gcc
export MAKE=make
export VALGRIND=valgrind
export CMP=cmp
export DOT=dot
export BISON=bison
export GPROF=gprof

# flags
export CFLAGS="-g -O0 -Wall -Wpedantic -Wno-gnu -I$SRC_ROOT -I$TEST_ROOT"
export PROFILE_CFLAGS="$CFLAGS -pg"
export VALGRIND_FLAGS="--error-exitcode=1 --leak-check=full -q"
export CMP_FLAGS="-s"
export DOT_FLAGS="-Tsvg"
