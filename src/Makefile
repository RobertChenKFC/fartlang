SRC=$(SRC_ROOT)

# dependencies
SRC_DEPS=$(filter-out $(SRC)/Makefile,$(wildcard $(SRC)/*))

all: $(addsuffix .all,$(SRC_DEPS))

%.all:
	$(MAKE) -C $*

clean: $(addsuffix .clean,$(SRC_DEPS))

%.clean:
	$(MAKE) -C $* clean
