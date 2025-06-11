NAME := dfa
include $(MK)/push.mk

DEPS := lex/dfa
DEPS += lex/fa
DEPS += lex/nfa
DEPS += lex/regex
DEPS += util/bitset
DEPS += util/hashtable
DEPS += util/vector
include $(MK)/c_cmp_test.mk
$(OUT_FILES): $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<)

SVG_FILES := $(addsuffix .svg, $(basename $(OUT_FILES)))
.PHONY: $(DIR).visualize
$(DIR).visualize: $(SVG_FILES)
$(SVG_FILES): %.svg: $(OUT_FILES)
	$(DOT) $(DOT_FLAGS) -o $*.svg $*.out

include $(MK)/pop.mk
