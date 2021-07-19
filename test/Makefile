SRC=$(TEST_ROOT)

# dependencies
SRC_DEPS=$(filter-out $(SRC)/Makefile,$(wildcard $(SRC)/*))

all: $(addsuffix .all,$(SRC_DEPS))

%.all:
	$(MAKE) -C $*

test: $(addsuffix .test,$(SRC_DEPS))

%.test:
	$(MAKE) -C $* test

clean: $(addsuffix .clean,$(SRC_DEPS))

%.clean:
	$(MAKE) -C $* clean
