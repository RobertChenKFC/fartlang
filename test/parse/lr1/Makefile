BIN=$(BIN_ROOT)/parse/lr1

# dependencies
SRC_DEPS += $(SRC_ROOT)/parse/cfg
LIB_DEPS += $(BIN_ROOT)/parse/cfg/cfg.o
SRC_DEPS += $(SRC_ROOT)/parse/lr1
LIB_DEPS += $(BIN_ROOT)/parse/lr1/lr1.o
SRC_DEPS += $(SRC_ROOT)/util/hashtable
LIB_DEPS += $(BIN_ROOT)/util/hashtable/hashtable.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_lr1 $(CFLAGS) $(LIB_DEPS) test_lr1.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_lr1

visualize: all
	$(BIN)/test_lr1
	$(DOT) $(DOT_FLAGS) -o 1.svg 1.out
	$(DOT) $(DOT_FLAGS) -o 2.svg 2.out
	$(DOT) $(DOT_FLAGS) -o 3.svg 3.out
	$(DOT) $(DOT_FLAGS) -o 4.svg 4.out
	$(DOT) $(DOT_FLAGS) -o 5.svg 5.out
	$(DOT) $(DOT_FLAGS) -o 6.svg 6.out

profile_all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_lr1 $(PROFILE_CFLAGS) $(LIB_DEPS) test_lr1.c

profile: profile_all
	$(BIN)/test_lr1

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_lr1

%.clean:
	$(MAKE) -C $* clean
