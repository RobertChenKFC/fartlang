BIN=$(BIN_ROOT)/lex/regex

# dependencies
SRC_DEPS += $(SRC_ROOT)/lex/regex
LIB_DEPS += $(BIN_ROOT)/lex/regex/regex.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_regex $(CFLAGS) $(LIB_DEPS) test_regex.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_regex > 1.out
	$(CMP) $(CMP_FLAGS) 1.out 1.ref

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_regex

%.clean:
	$(MAKE) -C $* clean
