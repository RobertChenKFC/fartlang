BIN = $(BIN_ROOT)/util/bigint/bigint.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): bigint.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
