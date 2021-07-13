SRC=$(SRC_ROOT)/util/vector
BIN=$(BIN_ROOT)/util/vector

all: test_vector
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_vector

test_vector: test_vector.c
	$(MAKE) -C $(SRC)
	$(CC) -o $(BIN)/test_vector $(CFLAGS) $(BIN)/vector.o test_vector.c

clean:
	$(MAKE) -C $(SRC) clean
	rm -rf $(BIN)/test_vector
