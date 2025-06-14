NAME := cfg
include $(MK)/push.mk

DEPS := $(DEPS) parse/cfg
DEPS := $(DEPS) util/vector
include $(MK)/c_cmp_test.mk
$(OUT_FILES): $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<)

include $(MK)/pop.mk
