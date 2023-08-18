BIN = $(BIN_ROOT)/parse/parser/test_parser

# dependencies
DEPS += /lex/fa
DEPS += /lex/nfa
DEPS += /lex/dfa
DEPS += /lex/lexer
DEPS += /lex/regex
DEPS += /parse/cfg
DEPS += /parse/lr1
DEPS += /parse/parser
DEPS += /util/bitset
DEPS += /util/hashtable
DEPS += /util/source
DEPS += /util/vector

SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

LR_TYPES = canonical-lr lalr
GRAMMAR_FILES = $(addprefix 7-,$(LR_TYPES))
BISON_FILES = $(addsuffix .yy,$(GRAMMAR_FILES))
BISON_CPP_FILES = $(addsuffix .tab.cc,$(GRAMMAR_FILES))
BISON_OUTPUT_FILES = $(addsuffix .output,$(GRAMMAR_FILES))
REF_FILES = $(addsuffix .ref,$(GRAMMAR_FILES))
PARSER_FILES = $(addsuffix .txt,$(addprefix parser3-,$(LR_TYPES)))

TESTS = 3 $(GRAMMAR_FILES)
TEST_DEPS = $(addsuffix .test,$(TESTS))
OUT_FILES = $(addsuffix .out,$(TESTS))

OUTPUT_FILES = $(BISON_FILES) $(BISON_CPP_FILES) $(BISON_OUTPUT_FILES) $(REF_FILES) parser2.txt $(PARSER_FILES)

.PHONY: $(ALL_DEPS) test clean $(CLEAN_DEPS)

$(BIN): test_parser.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(TEST_DEPS)

$(TEST_DEPS): %.test: %.out %.ref
	$(CMP) $(CMP_FLAGS) $*.out $*.ref

$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)

$(REF_FILES): 7-%.ref: 7-%.output
	./get_num_states.py 7-$*.output 7-$*.ref

$(BISON_OUTPUT_FILES): 7-%.output: 7-%.yy
	$(BISON) --report=state 7-$*.yy

$(BISON_FILES): 7-%.yy: 7.yy
	echo "%define lr.type $*" | cat - 7.yy > 7-$*.yy

clean: $(CLEAN_DEPS)
	rm -rf $(BIN) $(OUTPUT_FILES)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
