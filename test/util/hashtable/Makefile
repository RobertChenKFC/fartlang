SRC=$(SRC_ROOT)/util/hashtable
BIN=$(BIN_ROOT)/util/hashtable

all: test_hashtable
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_hashtable

test_hashtable: test_hashtable.c
	$(MAKE) -C $(SRC)
	$(CC) -o $(BIN)/test_hashtable $(CFLAGS) $(BIN)/hashtable.o test_hashtable.c

clean:
	$(MAKE) -C $(SRC) clean
	rm -rf $(BIN)/test_hashtable
