NAME := lexer
include $(MK)/push.mk

DEPS := $(DEPS) lex/dfa
DEPS := $(DEPS) lex/fa
DEPS := $(DEPS) lex/lexer
DEPS := $(DEPS) lex/nfa
DEPS := $(DEPS) lex/regex
DEPS := $(DEPS) util/bitset
DEPS := $(DEPS) util/hashtable
DEPS := $(DEPS) util/source
DEPS := $(DEPS) util/vector
include $(MK)/c_cmp_test.mk
$(OUT_FILES)&: $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<) 2>$(abspath $(addsuffix error.out,$(dir $@)))

include $(MK)/pop.mk
