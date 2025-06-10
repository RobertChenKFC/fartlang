CLEAN := $(CLEAN) $(DIR).clean
CLEAN_TARGETS := $(addsuffix .clean,$(sort $(CLEAN_FILES)))

.PHONY: $(DIR).clean
$(DIR).clean: $(CLEAN_TARGETS)
$(CLEAN_TARGETS): %.clean:
	rm -f $*
