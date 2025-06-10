include $(MK)/c_test.mk

.PHONY: $(DIR).test
TEST := $(TEST) $(DIR).test
$(DIR).test: $(BIN)
		$(VALGRIND) $(VALGRIND_FLAGS) $<
