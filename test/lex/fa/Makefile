BIN=$(BIN_ROOT)/lex/fa

# dependencies
SRC_DEPS += $(SRC_ROOT)/lex/fa
LIB_DEPS += $(BIN_ROOT)/lex/fa/fa.o
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_fa $(CFLAGS) $(LIB_DEPS) test_fa.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_fa
	$(CMP) $(CMP_FLAGS) 1.out 1.ref
	$(CMP) $(CMP_FLAGS) 2.out 2.ref
	$(CMP) $(CMP_FLAGS) 3.out 3.ref

visualize: all
	$(BIN)/test_fa
	$(DOT) $(DOT_FLAGS) -o 1.svg 1.out
	$(DOT) $(DOT_FLAGS) -o 2.svg 2.out
	$(DOT) $(DOT_FLAGS) -o 3.svg 3.out

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_fa

%.clean:
	$(MAKE) -C $* clean
