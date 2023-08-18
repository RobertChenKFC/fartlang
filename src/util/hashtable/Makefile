BIN = $(BIN_ROOT)/util/hashtable/hashtable.o

.PHONY: clean

$(BIN): hashtable.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
