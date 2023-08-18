SRC = $(TEST_ROOT)/util

# dependencies
SRC_DEPS = $(filter-out $(SRC)/Makefile, $(wildcard $(SRC)/*))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
TEST_DEPS = $(addsuffix .test, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

.PHONY: all $(ALL_DEPS) test $(TEST_DEPS) clean $(CLEAN_DEPS)

all: $(ALL_DEPS)

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

test: $(TEST_DEPS)

$(TEST_DEPS): %.test:
	$(MAKE) -C $* test

clean: $(CLEAN_DEPS)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
