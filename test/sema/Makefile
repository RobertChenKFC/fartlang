NAME := sema
include $(MK)/push.mk

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

COMPILER_FILES := $(COMPILER_FILES) $(BIN_DIR)/lexer.txt
COMPILER_FILES := $(COMPILER_FILES) $(BIN_DIR)/parser.txt
$(CMPS): | $(COMPILER_FILES)
$(COMPILER_FILES): $(BIN) | $(DIR)
$(OUT_FILES): %.out: $(BIN) %.fart | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(VALGRIND_REDIRECT) $(abspath $<) $(notdir $*.fart) 2>$(abspath $@)

BIN_DIR := $(dir $(BIN))
CLEAN_FILES := $(CLEAN_FILES) 
CLEAN_FILES := $(CLEAN_FILES) $(BIN_DIR)/parser.txt

include $(MK)/pop.mk
