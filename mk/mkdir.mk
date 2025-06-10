MKDIRS := $(sort $(MKDIRS))
$(MKDIRS): %:
	mkdir -p $*
