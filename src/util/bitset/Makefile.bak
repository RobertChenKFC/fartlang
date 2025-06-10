BIN = $(BIN_ROOT)/util/bitset/bitset.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): bitset.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
