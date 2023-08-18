BIN = $(BIN_ROOT)/util/source/source.o

.PHONY: clean

$(BIN): source.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
