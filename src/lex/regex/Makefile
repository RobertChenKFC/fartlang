BIN = $(BIN_ROOT)/lex/regex/regex.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): regex.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
