BIN = $(BIN_ROOT)/util/source/test_source

# dependencies
DEPS += /util/vector
DEPS += /util/source
SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

TESTS = $(shell seq 1)
TEST_DEPS = $(addsuffix .test, $(TESTS))
OUT_FILES = $(addsuffix .out, $(TESTS))

.PHONY: $(ALL_DEPS) test $(TEST_DEPS) clean $(CLEAN_DEPS)

# modify valgrind flags to redirect program output
VALGRIND_FLAGS += --log-fd=9

$(BIN): test_source.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(TEST_DEPS)

$(TEST_DEPS): %.test: $(OUT_FILES)
	$(CMP) $(CMP_FLAGS) $*.out $*.ref

$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN) 9>&2 2>1.out

clean: $(CLEAN_DEPS)
	rm -rf $(BIN)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
