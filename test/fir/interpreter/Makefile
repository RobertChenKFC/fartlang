NAME := interpreter
include $(MK)/push.mk

DEPS := $(DEPS) util/hashtable
DEPS := $(DEPS) util/vector
DEPS := $(DEPS) fir/examples
DEPS := $(DEPS) fir/ir
DEPS := $(DEPS) fir/interpreter

include $(MK)/c_cmp_test.mk
$(OUT_FILES)&: $(BIN) | $(DIR)
	cd $| && $(VALGRIND) $(VALGRIND_FLAGS) $(abspath $<)

include $(MK)/pop.mk
