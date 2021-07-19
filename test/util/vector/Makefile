BIN=$(BIN_ROOT)/util/vector

# dependencies
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_vector $(CFLAGS) $(LIB_DEPS) test_vector.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_vector

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_vector

%.clean:
	$(MAKE) -C $* clean
