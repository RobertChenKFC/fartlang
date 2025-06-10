MK := ./mk
include $(MK)/defs.mk

DIR := .
include $(MK)/internal.mk

.PHONY: test
test: $(TEST)
.PHONY: test_targets
test_targets:
	./gen_test_targets.py $(GITHUB_OUTPUT) $(TEST)

include $(MK)/clean.mk
.PHONY: clean
clean: $(CLEAN)

include $(MK)/mkdir.mk
