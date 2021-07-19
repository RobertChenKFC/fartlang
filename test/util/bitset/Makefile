BIN=$(BIN_ROOT)/util/bitset

# dependencies
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o
SRC_DEPS += $(SRC_ROOT)/util/bitset
LIB_DEPS += $(BIN_ROOT)/util/bitset/bitset.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_bitset $(CFLAGS) $(LIB_DEPS) test_bitset.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_bitset

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_bitset

%.clean:
	$(MAKE) -C $* clean
