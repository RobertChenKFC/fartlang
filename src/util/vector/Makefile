BIN = $(BIN_ROOT)/util/vector/vector.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): vector.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
