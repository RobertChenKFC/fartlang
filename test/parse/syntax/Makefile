NAME := syntax
include $(MK)/push.mk

DEPS := $(DEPS) lex/dfa
DEPS := $(DEPS) lex/fa
DEPS := $(DEPS) lex/nfa
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
$(OUT_FILES): $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<)

BIN_DIR := $(dir $(BIN))
CLEAN_FILES := $(CLEAN_FILES) $(BIN_DIR)/lexer.txt
CLEAN_FILES := $(CLEAN_FILES) $(BIN_DIR)/parser.txt

include $(MK)/pop.mk
