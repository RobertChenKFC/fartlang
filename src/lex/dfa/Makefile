BIN = $(BIN_ROOT)/lex/dfa/dfa.o

.PHONY: clean

$(BIN): dfa.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
