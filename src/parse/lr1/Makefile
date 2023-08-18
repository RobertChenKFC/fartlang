BIN = $(BIN_ROOT)/parse/lr1/lr1.o

.PHONY: clean

$(BIN): lr1.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
