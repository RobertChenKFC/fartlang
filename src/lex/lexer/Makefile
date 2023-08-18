BIN = $(BIN_ROOT)/lex/lexer/lexer.o

.PHONY: clean

$(BIN): lexer.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
