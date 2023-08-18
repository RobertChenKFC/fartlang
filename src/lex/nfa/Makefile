BIN = $(BIN_ROOT)/lex/nfa/nfa.o

.PHONY: clean

$(BIN): nfa.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
