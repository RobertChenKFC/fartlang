NAME := parser
include $(MK)/push.mk

DEPS := $(DEPS) lex/dfa
DEPS := $(DEPS) lex/fa
DEPS := $(DEPS) lex/nfa
DEPS := $(DEPS) lex/lexer
DEPS := $(DEPS) lex/regex
DEPS := $(DEPS) parse/cfg
DEPS := $(DEPS) parse/lr1
DEPS := $(DEPS) parse/parser
DEPS := $(DEPS) util/bitset
DEPS := $(DEPS) util/hashtable
DEPS := $(DEPS) util/source
DEPS := $(DEPS) util/vector
include $(MK)/c_test.mk

# DEBUG
.PHONY: debug
GET_NUM_STATES := $(DIR)/get_num_states.py
BISON_CMP_NAMES := $(DIR)/7-canonical-lr $(DIR)/7-lalr
BISON_BASE_FILE := $(DIR)/7.yy
BISON_FILES := $(addsuffix .yy,$(BISON_CMP_NAMES))
BISON_OUTPUT_FILES := $(addsuffix .output,$(BISON_CMP_NAMES))
BISON_CC_FILES := $(addsuffix .tab.cc,$(BISON_CMP_NAMES))
BISON_REF_FILES := $(addsuffix .ref,$(BISON_CMP_NAMES))
CMP_NAMES := $(DIR)/3 $(BISON_CMP_NAMES)
OUT_FILES := $(addsuffix .out,$(CMP_NAMES))
REF_FILES := $(addsuffix .ref,$(CMP_NAMES))
CMPS := $(addsuffix .cmp,$(CMP_NAMES))
$(BISON_FILES): $(DIR)/7-%.yy: $(BISON_BASE_FILE)
	echo "%define lr.type $*" | cat - $< >$@
$(BISON_OUTPUT_FILES): %.output: %.yy | $(DIR)
	cd $| && $(BISON) --report=state $(abspath $<)
$(BISON_REF_FILES): %.ref: %.output
	$(GET_NUM_STATES) $< $@
$(OUT_FILES): $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<)
$(CMPS): %.cmp: %.out %.ref
	$(CMP) $(CMP_FLAGS) $^
.PHONY: $(DIR).test
$(DIR).test: $(CMPS)

CLEAN_FILES := $(CLEAN_FILES) $(BISON_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(BISON_OUTPUT_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(BISON_CC_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(BISON_REF_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(OUT_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(DIR)/parser2.txt
CLEAN_FILES := $(CLEAN_FILES) $(DIR)/parser3-canonical-lr.txt
CLEAN_FILES := $(CLEAN_FILES) $(DIR)/parser3-lalr.txt

include $(MK)/pop.mk
