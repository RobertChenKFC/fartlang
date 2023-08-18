BIN = $(BIN_ROOT)/lex/nfa/test_nfa

# dependencies
DEPS += /lex/regex
DEPS += /lex/fa
DEPS += /lex/nfa
DEPS += /util/hashtable
DEPS += /util/vector
SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

TESTS = $(shell seq 8)
TEST_DEPS = $(addsuffix .test, $(TESTS))
OUT_FILES = $(addsuffix .out, $(TESTS))
SVG_FILES = $(addsuffix .svg, $(TESTS))

.PHONY: $(ALL_DEPS) test $(TEST_DEPS) visualize clean $(CLEAN_DEPS)

$(BIN): test_nfa.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(TEST_DEPS)

$(TEST_DEPS): %.test: $(OUT_FILES)
	$(CMP) $(CMP_FLAGS) $*.out $*.ref

visualize: $(SVG_FILES)

$(SVG_FILES): %.svg: $(OUT_FILES)
	$(DOT) $(DOT_FLAGS) -o $*.svg $*.out

$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)

clean: $(CLEAN_DEPS)
	rm -rf $(BIN) $(OUT_FILES) $(SVG_FILES)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
