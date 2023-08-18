BIN = $(BIN_ROOT)/parse/lr1/test_lr1

# dependencies
DEPS += /parse/cfg
DEPS += /parse/lr1
DEPS += /util/hashtable
DEPS += /util/vector

SRC_DEPS = $(addprefix $(SRC_ROOT), $(DEPS))
LIB_DEPS = $(addprefix $(BIN_ROOT), $(join $(DEPS), $(addprefix /, $(addsuffix .o, $(notdir $(DEPS))))))

ALL_DEPS = $(addsuffix .all, $(SRC_DEPS))
CLEAN_DEPS = $(addsuffix .clean, $(SRC_DEPS))

TESTS = $(shell seq 6)
TEST_DEPS = $(addsuffix .test, $(TESTS))
BISON_OPT_FILES = $(addsuffix _opt.yy, $(TESTS))
BISON_FLAGS = -x
BISON_XML_FILES = $(addsuffix _opt.xml, $(TESTS))
BISON_CC_FILES = $(addsuffix _opt.tab.cc, $(TESTS))
TEST_FILES = $(addsuffix .h, $(TESTS))
XML_FILES = $(addsuffix .xml, $(TESTS))
SVG_FILES = $(addsuffix .svg, $(TESTS))
PROFILE_FILES = gmon.out

.PHONY: $(ALL_DEPS) test $(TEST_DEPS) visualize profile clean $(CLEAN_DEPS)

$(BIN): test_lr1.c $(ALL_DEPS) $(BISON_XML_FILES) $(TEST_FILES)
	$(CC) -o $(BIN) $(CFLAGS) $(LIB_DEPS) $<

$(ALL_DEPS): %.all:
	$(MAKE) -C $*

$(BISON_XML_FILES): %_opt.xml: %_opt.yy
	$(BISON) $(BISON_FLAGS) $<

$(BISON_OPT_FILES): %_opt.yy: cfg_convert.py %.yy
	$(CURDIR)/$< $*.yy $* --bison > $*_opt.yy

$(TEST_FILES): %.h: cfg_convert.py %.yy
	$(CURDIR)/$< $*.yy $* --fart > $*.h

test: $(TEST_DEPS)

$(TEST_DEPS): %.test: $(BISON_XML_FILES) $(XML_FILES)
	./check_isomorphism.py $*.xml $*_opt.xml

visualize: $(SVG_FILES)

$(SVG_FILES): %.svg: %.out
	$(DOT) $(DOT_FLAGS) -o $*.svg $*.out

$(XML_FILES): $(BIN)
	$(VALGRIND) $(VALGRIND_FLAGS) $(BIN)

profile: $(PROFILE_FILES)
	$(GPROF) $(BIN) $(PROFILE_FILES)

$(PROFILE_FILES): test_lr1.c $(ALL_DEPS) $(TEST_FILES)
	$(CC) -o $(BIN) $(PROFILE_CFLAGS) $(LIB_DEPS) $<
	$(BIN)

clean: $(CLEAN_DEPS)
	rm -rf $(BIN) $(BISON_OPT_FILES) $(XML_FILES) $(BISON_XML_FILES) $(BISON_CC_FILES) $(TEST_FILES) $(SVG_FILES) $(PROFILE_FILES)

$(CLEAN_DEPS): %.clean:
	$(MAKE) -C $* clean
