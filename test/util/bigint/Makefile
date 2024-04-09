BIN = $(BIN_ROOT)/util/vector/test_bigint

# dependencies
DEPS += /util/bigint
SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

REF_FILES = $(wildcard *.ref)
OUT_FILES = $(REF_FILES:.ref=.out)
TESTS = $(REF_FILES:.ref=.test)

.PHONY: $(ALL_DEPS) test clean $(CLEAN_DEPS)

$(BIN): test_bigint.c $(ALL_DEPS)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(TESTS)

$(TESTS): %.test: %.out %.ref
	$(CMP) $(CMP_FLAGS) $*.out $*.ref

$(OUT_FILES): $(BIN)
	# DEBUG
	# $(VALGRIND) $(VALGRIND_FLAGS) $(BIN)
	$(BIN) > $(OUT_FILES)

clean: $(CLEAN_DEPS)
	rm -rf $(BIN)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
