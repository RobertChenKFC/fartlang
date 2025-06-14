NAME := source
include $(MK)/push.mk

DEPS := $(DEPS) util/source
DEPS := $(DEPS) util/vector
include $(MK)/c_cmp_test.mk
$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(VALGRIND_REDIRECT) $< 2>$@

include $(MK)/pop.mk
