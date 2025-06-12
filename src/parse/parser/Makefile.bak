BIN = $(BIN_ROOT)/parse/parser/parser.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): parser.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
