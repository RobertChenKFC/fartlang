NAME := regex
include $(MK)/push.mk

DEPS += lex/regex
DEPS += util/vector
include $(MK)/c_cmp_test.mk
$(OUT_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $< >$@

include $(MK)/pop.mk
