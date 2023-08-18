BIN = $(BIN_ROOT)/parse/parser/parser.o

.PHONY: clean

$(BIN): parser.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
