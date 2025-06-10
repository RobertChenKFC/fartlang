SRC = $(SRC_ROOT)/parse

# dependencies
SRC_DEPS = $(filter-out $(SRC)/Makefile, $(wildcard $(SRC)/*))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

.PHONY: all $(ALL_DEPS) clean $(CLEAN_DEPS)

all: $(ALL_DEPS)

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

clean: $(CLEAN_DEPS)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
