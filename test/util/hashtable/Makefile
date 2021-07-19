BIN=$(BIN_ROOT)/util/hashtable

# dependencies
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_hashtable $(CFLAGS) $(LIB_DEPS) test_hashtable.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_hashtable

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_hashtable

%.clean:
	$(MAKE) -C $* clean
