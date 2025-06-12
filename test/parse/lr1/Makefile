NAME := lr1
include $(MK)/push.mk

BISON_FILES := $(filter-out %_opt.yy,$(wildcard $(DIR)/*.yy))
BISON_BASENAMES := $(basename $(BISON_FILES))
BISON_OPT_FILES := $(addsuffix _opt.yy,$(BISON_BASENAMES))
BISON_XML_FILES := $(addsuffix _opt.xml,$(BISON_BASENAMES))
BISON_CC_FILES := $(addsuffix _opt.tab.cc,$(BISON_BASENAMES))
TEST_HDRS := $(addsuffix .h,$(BISON_BASENAMES))
TEST_XML_FILES := $(addsuffix .xml,$(BISON_BASENAMES))
CFG_CONVERT := $(DIR)/cfg_convert.py
CHECKER := $(DIR)/check_isomorphism.py
CHECKS := $(addsuffix .check,$(BISON_BASENAMES))

.PHONY: debug
debug:
	@echo $(BISON_FILES)

DEPS := $(DEPS) parse/cfg
DEPS := $(DEPS) parse/lr1
DEPS := $(DEPS) util/hashtable
DEPS := $(DEPS) util/vector
RAW_C_DEPS := $(RAW_C_DEPS) $(TEST_HDRS)
include $(MK)/c_test.mk

$(BISON_OPT_FILES): %_opt.yy: $(CFG_CONVERT) %.yy
	$< $*.yy $(notdir $*) --bison >$@
$(BISON_XML_FILES): %_opt.xml: %_opt.yy | $(DIR)
	cd $| && $(BISON) -x $(abspath $<)
$(TEST_HDRS): %.h: $(CFG_CONVERT) %.yy
	$< $*.yy $(notdir $*) --fart >$@
$(TEST_XML_FILES): $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<)
.PHONY: $(CHECKS)
$(CHECKS): %.check: %.xml %_opt.xml
	$(CHECKER) $^
.PHONY: $(DIR).test
TEST := $(TEST) $(DIR).test
$(DIR).test: $(CHECKS)

CLEAN_FILES := $(CLEAN_FILES) $(BISON_OPT_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(BISON_XML_FILES)
CLEAN_FILES := $(CLEAN_FILES) $(TEST_HDRS)
CLEAN_FILES := $(CLEAN_FILES) $(TEST_XML_FILES)

include $(MK)/pop.mk
