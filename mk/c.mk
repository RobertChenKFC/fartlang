SRCS := $(wildcard $(DIR)/*.c)
OBJS := $(patsubst $(SRC_PREFIX)/%.c,$(BIN_DIR)/%.o,$(SRCS))
$(OBJS): $(BIN_DIR)/%.o: $(SRC_PREFIX)/%.c
		$(CC) -c -o $@ $< $(CFLAGS)

CLEAN_FILES := $(CLEAN_FILES) $(OBJS)
