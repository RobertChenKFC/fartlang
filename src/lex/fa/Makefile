BIN = $(BIN_ROOT)/lex/fa/fa.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): fa.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
