BIN = $(BIN_ROOT)/lex/fa/fa.o

.PHONY: clean

$(BIN): fa.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
