BIN = $(BIN_ROOT)/util/source/source.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): source.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
