BIN = $(BIN_ROOT)/sema/sema.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): sema.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
