BIN=$(BIN_ROOT)/util/source

# dependencies
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o
SRC_DEPS += $(SRC_ROOT)/util/source
LIB_DEPS += $(BIN_ROOT)/util/source/source.o

# modify valgrind flags to redirect program output
VALGRIND_FLAGS += --log-fd=9

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_source $(CFLAGS) $(LIB_DEPS) test_source.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_source 9>&2 2>1.out
	$(CMP) $(CMP_FLAGS) 1.out 1.ref

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_source

%.clean:
	$(MAKE) -C $* clean
