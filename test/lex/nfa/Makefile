SRC=$(SRC_ROOT)/lex/nfa
BIN=$(BIN_ROOT)/lex/nfa

# dependencies
REGEX_SRC=$(SRC_ROOT)/lex/regex
LIBS += $(BIN_ROOT)/lex/regex/regex.o
HASHTABLE_SRC=$(SRC_ROOT)/util/hashtable
LIBS += $(BIN_ROOT)/util/hashtable/hashtable.o
VECTOR_SRC=$(SRC_ROOT)/util/vector
LIBS += $(BIN_ROOT)/util/vector/vector.o

all: test_nfa
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_nfa
	# DEBUG
	# $(CMP) $(CMP_FLAGS) 1.out 1.ref

test_nfa: test_nfa.c
	$(MAKE) -C $(SRC)
	$(MAKE) -C $(REGEX_SRC)
	$(MAKE) -C $(HASHTABLE_SRC)
	$(MAKE) -C $(VECTOR_SRC)
	$(CC) -o $(BIN)/test_nfa $(CFLAGS) $(BIN)/nfa.o $(LIBS) test_nfa.c

visualize: test_nfa
	$(BIN)/test_nfa
	$(DOT) $(DOT_FLAGS) -o 1.png 1.out
	$(DOT) $(DOT_FLAGS) -o 2.png 2.out
	$(DOT) $(DOT_FLAGS) -o 3.png 3.out
	$(DOT) $(DOT_FLAGS) -o 4.png 4.out
	$(DOT) $(DOT_FLAGS) -o 5.png 5.out
	$(DOT) $(DOT_FLAGS) -o 6.png 6.out
	$(DOT) $(DOT_FLAGS) -o 7.png 7.out

clean:
	$(MAKE) -C $(SRC) clean
	rm -rf $(BIN)/test_nfa
