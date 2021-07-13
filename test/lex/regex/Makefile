SRC=$(SRC_ROOT)/lex/regex
BIN=$(BIN_ROOT)/lex/regex

all: test_regex
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_regex > 1.out
	$(CMP) $(CMP_FLAGS) 1.out 1.ref

test_regex: test_regex.c
	$(MAKE) -C $(SRC)
	$(CC) -o $(BIN)/test_regex $(CFLAGS) $(BIN)/regex.o test_regex.c

clean:
	$(MAKE) -C $(SRC) clean
	rm -rf $(BIN)/test_regex
