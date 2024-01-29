BIN = $(BIN_ROOT)/parse/syntax/syntax.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): syntax.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
