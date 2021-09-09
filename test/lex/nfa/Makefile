BIN=$(BIN_ROOT)/lex/dfa

# dependencies
SRC_DEPS += $(SRC_ROOT)/lex/regex
LIB_DEPS += $(BIN_ROOT)/lex/regex/regex.o
SRC_DEPS += $(SRC_ROOT)/lex/fa
LIB_DEPS += $(BIN_ROOT)/lex/fa/fa.o
SRC_DEPS += $(SRC_ROOT)/lex/nfa
LIB_DEPS += $(BIN_ROOT)/lex/nfa/nfa.o
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_nfa $(CFLAGS) $(LIB_DEPS) test_nfa.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_nfa
	$(CMP) $(CMP_FLAGS) 1.out 1.ref
	$(CMP) $(CMP_FLAGS) 2.out 2.ref
	$(CMP) $(CMP_FLAGS) 3.out 3.ref
	$(CMP) $(CMP_FLAGS) 4.out 4.ref
	$(CMP) $(CMP_FLAGS) 5.out 5.ref
	$(CMP) $(CMP_FLAGS) 6.out 6.ref
	$(CMP) $(CMP_FLAGS) 7.out 7.ref
	$(CMP) $(CMP_FLAGS) 8.out 8.ref

visualize: all
	$(BIN)/test_nfa
	$(DOT) $(DOT_FLAGS) -o 1.svg 1.out
	$(DOT) $(DOT_FLAGS) -o 2.svg 2.out
	$(DOT) $(DOT_FLAGS) -o 3.svg 3.out
	$(DOT) $(DOT_FLAGS) -o 4.svg 4.out
	$(DOT) $(DOT_FLAGS) -o 5.svg 5.out
	$(DOT) $(DOT_FLAGS) -o 6.svg 6.out
	$(DOT) $(DOT_FLAGS) -o 7.svg 7.out
	$(DOT) $(DOT_FLAGS) -o 8.svg 8.out

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_nfa

%.clean:
	$(MAKE) -C $* clean
