# Tools
CC := gcc
VALGRIND := valgrind
CMP := cmp
DOT := dot
BISON := bison
GPROF := gprof

# Paths
ROOT := .
SRC_ROOT := $(ROOT)/src
TEST_ROOT := $(ROOT)/test
BIN_ROOT := $(ROOT)/bin

# Flags
CFLAGS := -g -O0 -Wall -Wpedantic -Wno-gnu -I$(SRC_ROOT) -I$(TEST_ROOT)
PROFILE_CFLAGS := $(CFLAGS) -pg
VALGRIND_FLAGS := --error-exitcode=1 --leak-check=full -q
VALGRIND_REDIRECT := --log-fd=9 9>&2
CMP_FLAGS := -s
DOT_FLAGS := -Tsvg
