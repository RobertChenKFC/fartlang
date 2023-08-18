BIN = $(BIN_ROOT)/lex/regex/regex.o

.PHONY: clean

$(BIN): regex.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
