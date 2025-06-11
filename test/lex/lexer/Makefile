NAME := lexer
include $(MK)/push.mk

DEPS := lex/dfa
DEPS += lex/fa
DEPS += lex/lexer
DEPS += lex/nfa
DEPS += lex/regex
DEPS += util/bitset
DEPS += util/hashtable
DEPS += util/source
DEPS += util/vector
include $(MK)/c_cmp_test.mk
$(OUT_FILES)&: $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<) 2>$(abspath $(addsuffix error.out,$(dir $@)))

include $(MK)/pop.mk
