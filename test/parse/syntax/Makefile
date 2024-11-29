BIN = $(BIN_ROOT)/parse/syntax/test_syntax

# dependencies
DEPS += /lex/fa
DEPS += /lex/nfa
DEPS += /lex/dfa
DEPS += /lex/lexer
DEPS += /lex/regex
DEPS += /parse/cfg
DEPS += /parse/lr1
DEPS += /parse/parser
DEPS += /parse/syntax
DEPS += /util/bigint
DEPS += /util/bitset
DEPS += /util/hashtable
DEPS += /util/source
DEPS += /util/vector

SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

SAVE_FILES = $(dir $(BIN))lexer.txt $(dir $(BIN))parser.txt
REF_FILES = $(wildcard *.ref)
OUT_FILES = $(REF_FILES:.ref=.out)
.PRECIOUS: $(OUT_FILES)
TESTS = $(REF_FILES:.ref=.test)
OUTPUT_FILES = $(SAVE_FILES) $(OUT_FILES)

$(BIN): test_syntax.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<
	rm -rf $(SAVE_FILES)

.PHONY: test
test: $(TESTS)

$(TESTS): %.test: %.out %.ref
	$(CMP) $(CMP_FLAGS) $*.out $*.ref

$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)

.PHONY: $(ALL_DEPS)
$(ALL_DEPS): %.all:
	$(MAKE) -C $*

.PHONY: clean
clean: $(CLEAN_DEPS)
	rm -rf $(BIN) $(OUTPUT_FILES)

.PHONY: $(CLEAN_DEPS)
$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
