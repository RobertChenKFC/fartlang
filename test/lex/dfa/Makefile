BIN=$(BIN_ROOT)/lex/dfa

# dependencies
SRC_DEPS += $(SRC_ROOT)/lex/regex
LIB_DEPS += $(BIN_ROOT)/lex/regex/regex.o
SRC_DEPS += $(SRC_ROOT)/lex/fa
LIB_DEPS += $(BIN_ROOT)/lex/fa/fa.o
SRC_DEPS += $(SRC_ROOT)/lex/nfa
LIB_DEPS += $(BIN_ROOT)/lex/nfa/nfa.o
SRC_DEPS += $(SRC_ROOT)/lex/dfa
LIB_DEPS += $(BIN_ROOT)/lex/dfa/dfa.o
SRC_DEPS += $(SRC_ROOT)/util/bitset
LIB_DEPS += $(BIN_ROOT)/util/bitset/bitset.o
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_dfa $(CFLAGS) $(LIB_DEPS) test_dfa.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_dfa
	$(CMP) $(CMP_FLAGS) 1.out 1.ref
	$(CMP) $(CMP_FLAGS) 2.out 2.ref
	$(CMP) $(CMP_FLAGS) 3.out 3.ref
	$(CMP) $(CMP_FLAGS) 4.out 4.ref
	$(CMP) $(CMP_FLAGS) 5.out 5.ref
	$(CMP) $(CMP_FLAGS) 6.out 6.ref
	$(CMP) $(CMP_FLAGS) 7.out 7.ref
	$(CMP) $(CMP_FLAGS) 8.out 8.ref
	$(CMP) $(CMP_FLAGS) 9.out 9.ref
	$(CMP) $(CMP_FLAGS) 10.out 10.ref
	$(CMP) $(CMP_FLAGS) 11.out 11.ref
	$(CMP) $(CMP_FLAGS) 12.out 12.ref
	$(CMP) $(CMP_FLAGS) 13.out 13.ref
	$(CMP) $(CMP_FLAGS) 14.out 14.ref
	$(CMP) $(CMP_FLAGS) 15.out 15.ref
	$(CMP) $(CMP_FLAGS) 15.out 15.ref
	$(CMP) $(CMP_FLAGS) 16.out 16.ref

visualize: all
	$(BIN)/test_dfa
	$(DOT) $(DOT_FLAGS) -o 1.svg 1.out
	$(DOT) $(DOT_FLAGS) -o 2.svg 2.out
	$(DOT) $(DOT_FLAGS) -o 3.svg 3.out
	$(DOT) $(DOT_FLAGS) -o 4.svg 4.out
	$(DOT) $(DOT_FLAGS) -o 5.svg 5.out
	$(DOT) $(DOT_FLAGS) -o 6.svg 6.out
	$(DOT) $(DOT_FLAGS) -o 7.svg 7.out
	$(DOT) $(DOT_FLAGS) -o 8.svg 8.out
	$(DOT) $(DOT_FLAGS) -o 9.svg 9.out
	$(DOT) $(DOT_FLAGS) -o 10.svg 10.out
	$(DOT) $(DOT_FLAGS) -o 11.svg 11.out
	$(DOT) $(DOT_FLAGS) -o 12.svg 12.out
	$(DOT) $(DOT_FLAGS) -o 13.svg 13.out
	$(DOT) $(DOT_FLAGS) -o 14.svg 14.out
	$(DOT) $(DOT_FLAGS) -o 15.svg 15.out
	$(DOT) $(DOT_FLAGS) -o 16.svg 16.out

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_dfa

%.clean:
	$(MAKE) -C $* clean
