BIN = $(BIN_ROOT)/util/hashtable/hashtable.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): hashtable.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
