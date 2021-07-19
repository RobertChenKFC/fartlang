SRC=$(SRC_ROOT)
TEST=$(TEST_ROOT)

.PHONY: all test clean

all:
	$(MAKE) -C $(SRC)

test:
	$(MAKE) -C $(TEST) test

clean:
	$(MAKE) -C $(TEST) clean

