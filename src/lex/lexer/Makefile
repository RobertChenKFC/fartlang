BIN = $(BIN_ROOT)/lex/lexer/lexer.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): lexer.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
