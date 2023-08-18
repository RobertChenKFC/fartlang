BIN = $(BIN_ROOT)/util/bitset/bitset.o

.PHONY: clean

$(BIN): bitset.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
