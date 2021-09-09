BIN=$(BIN_ROOT)/parse/cfg

# dependencies
SRC_DEPS += $(SRC_ROOT)/parse/cfg
LIB_DEPS += $(BIN_ROOT)/parse/cfg/cfg.o
SRC_DEPS += $(SRC_ROOT)/util/vector
LIB_DEPS += $(BIN_ROOT)/util/vector/vector.o

all: $(addsuffix .all,$(SRC_DEPS))
	$(CC) -o $(BIN)/test_cfg $(CFLAGS) $(LIB_DEPS) test_cfg.c

%.all:
	$(MAKE) -C $*

test: all
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)/test_cfg
	$(CMP) $(CMP_FLAGS) 1.out 1.ref

clean: $(addsuffix .clean,$(SRC_DEPS))
	rm -rf $(BIN)/test_cfg

%.clean:
	$(MAKE) -C $* clean
