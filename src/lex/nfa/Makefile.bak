BIN = $(BIN_ROOT)/lex/nfa/nfa.o
BIN_DIR = $(dir $(BIN))

.PHONY: clean

$(BIN): nfa.c | $(BIN_DIR)
	$(CC) -c -o $@ $(CFLAGS) $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN)
