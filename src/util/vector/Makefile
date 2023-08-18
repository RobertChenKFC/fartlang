BIN = $(BIN_ROOT)/util/vector/vector.o

.PHONY: clean

$(BIN): vector.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
