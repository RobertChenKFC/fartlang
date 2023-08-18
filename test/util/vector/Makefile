BIN = $(BIN_ROOT)/util/vector/test_vector

# dependencies
DEPS += /util/vector
SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

.PHONY: $(ALL_DEPS) test clean $(CLEAN_DEPS)

$(BIN): test_vector.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)

clean: $(CLEAN_DEPS)
	rm -rf $(BIN)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
