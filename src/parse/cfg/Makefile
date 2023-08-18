BIN = $(BIN_ROOT)/parse/cfg/cfg.o

.PHONY: clean

$(BIN): cfg.c
	$(CC) -c -o $@ $(CFLAGS) $<

clean:
	rm -rf $(BIN)
