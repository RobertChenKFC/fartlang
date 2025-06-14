NAME := bigint
include $(MK)/push.mk

DEPS := $(DEPS) util/bigint
include $(MK)/c_cmp_test.mk
$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(VALGRIND_REDIRECT) $< >$@

include $(MK)/pop.mk
