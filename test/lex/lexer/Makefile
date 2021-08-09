BIN=$(BIN_ROOT)/lex/lexer

# dependencies
SRC_DEPS += $(SRC_ROOT)/lex/regex
LIB_DEPS += $(BIN_ROOT)/lex/regex/regex.o
SRC_DEPS += $(SRC_ROOT)/lex/fa
LIB_DEPS += $(BIN_ROOT)/lex/fa/fa.o
SRC_DEPS += $(SRC_ROOT)/lex/nfa
LIB_DEPS += $(BIN_ROOT)/lex/nfa/nfa.o
SRC_DEPS += $(SRC_ROOT)/lex/dfa
LIB_DEPS += $(BIN_ROOT)/lex/dfa/dfa.o
SRC_DEPS += $(SRC_ROOT)/lex/lexer
LIB_DEPS += $(BIN_ROOT)/lex/lexer/lexer.o
SRC_DEPS += $(SRC_ROOT)/util/bitset
LIB_DEPS += $(BIN_ROOT)/util/bitset/bitset.o
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o
SRC_DEPS += $(SRC_ROOT)/util/source
LIB_DEPS += $(BIN_ROOT)/util/source/source.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

# modify valgrind flags to redirect program output
VALGRIND_FLAGS += --log-fd=9

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_lexer $(CFLAGS) $(LIB_DEPS) test_lexer.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_lexer 9>&2 2>8.out
	$(CMP) $(CMP_FLAGS) 1.out 1.ref
	$(CMP) $(CMP_FLAGS) 2.out 2.ref
	$(CMP) $(CMP_FLAGS) 3.out 3.ref
	$(CMP) $(CMP_FLAGS) 4.out 4.ref
	$(CMP) $(CMP_FLAGS) 5.out 5.ref
	$(CMP) $(CMP_FLAGS) 6.out 6.ref
	$(CMP) $(CMP_FLAGS) 7.out 7.ref
	$(CMP) $(CMP_FLAGS) 8.out 8.ref

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_lexer

%.clean:
	$(MAKE) -C $* clean
