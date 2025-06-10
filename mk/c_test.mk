SRC_PREFIX := $(TEST_DIR)

include $(MK)/c.mk

NAMES := $(notdir $(DEPS))
DEPS := $(addprefix $(BIN_ROOT)/,$(DEPS))
DEPS := $(addsuffix /,$(DEPS))
DEPS := $(join $(DEPS),$(NAMES))
DEPS := $(addsuffix .o,$(DEPS))
OBJS += $(DEPS)
BIN := $(BIN_DIR)/test_$(NAME)
$(BIN): $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS)

CLEAN_FILES := $(CLEAN_FILES) $(BIN)
