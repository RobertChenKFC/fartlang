include $(MK)/c_test.mk

REF_FILES := $(wildcard $(DIR)/*.ref)
OUT_FILES := $(REF_FILES:.ref=.out)
CMPS := $(REF_FILES:.ref=.cmp)
.PHONY: $(CMPS)
$(CMPS): %.cmp: %.ref %.out
	$(CMP) $(CMP_FLAGS) $^

TEST := $(TEST) $(DIR).test
$(DIR).test: $(CMPS)

CLEAN_FILES := $(CLEAN_FILES) $(OUT_FILES)
