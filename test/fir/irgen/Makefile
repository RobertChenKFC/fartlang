NAME := irgen
include $(MK)/push.mk

DEPS := $(DEPS) fir/ir
DEPS := $(DEPS) fir/irgen
DEPS := $(DEPS) fir/interpreter
DEPS := $(DEPS) lex/fa
DEPS := $(DEPS) lex/nfa
DEPS := $(DEPS) lex/dfa
DEPS := $(DEPS) lex/lexer
DEPS := $(DEPS) lex/regex
DEPS := $(DEPS) parse/cfg
DEPS := $(DEPS) parse/lr1
DEPS := $(DEPS) parse/parser
DEPS := $(DEPS) parse/syntax
DEPS := $(DEPS) sema
DEPS := $(DEPS) util/bigint
DEPS := $(DEPS) util/bitset
DEPS := $(DEPS) util/hashtable
DEPS := $(DEPS) util/source
DEPS := $(DEPS) util/vector
include $(MK)/c_cmp_test.mk

COMPILER_FILES := $(BIN_DIR)/lexer.txt
COMPILER_FILES := $(COMPILER_FILES) $(BIN_DIR)/parser.txt
$(COMPILER_FILES)&: $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $<

$(OUT_FILES): %.out: $(BIN) %.fart $(COMPILER_FILES) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(VALGRIND_REDIRECT) $(abspath $<) $(notdir $*.fart) --debug 2>$(abspath $@)

BIN_DIR := $(dir $(BIN))
CLEAN_FILES := $(CLEAN_FILES) $(BIN_DIR)/lexer.txt
CLEAN_FILES := $(CLEAN_FILES) $(BIN_DIR)/parser.txt

include $(MK)/pop.mk
