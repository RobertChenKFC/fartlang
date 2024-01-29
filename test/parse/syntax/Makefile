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
DEPS += /util/bitset
DEPS += /util/hashtable
DEPS += /util/source
DEPS += /util/vector

SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

.PHONY: $(ALL_DEPS) test clean $(CLEAN_DEPS)

OUTPUT_FILES = $(dir $(BIN))lexer.txt

test: $(BIN)
	$(BIN)

$(BIN): test_syntax.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

clean: $(CLEAN_DEPS)
	rm -rf $(BIN) $(OUTPUT_FILES)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
