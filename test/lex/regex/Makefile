BIN = $(BIN_ROOT)/lex/regex/test_regex

# dependencies
DEPS += /lex/regex
DEPS += /util/vector
SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

TESTS = $(shell seq 1)
TEST_DEPS = $(addsuffix .test, $(TESTS))
OUT_FILES = $(addsuffix .out, $(TESTS))

.PHONY: $(ALL_DEPS) clean $(CLEAN_DEPS) test $(TEST_DEPS)

$(BIN): test_regex.c $(ALL_DEPS)
	$(CC) -o $@ $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(TEST_DEPS)

$(TEST_DEPS): %.test: $(OUT_FILES)
	$(CMP) $(CMP_FLAGS) $*.out $*.ref

$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN) > 1.out

clean: $(CLEAN_DEPS)
	rm -rf $(BIN) $(OUT_FILES)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
