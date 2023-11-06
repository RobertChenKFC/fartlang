BIN = $(BIN_ROOT)/parse/lr1/lr1.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): lr1.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
