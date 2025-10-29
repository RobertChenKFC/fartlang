#include "fir/irgen/irgen.h"
#include "fir/interpreter/interpreter.h"
#include "parse/syntax/syntax.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char *GetOutFilename(const char *path) {
  char *out = strdup(path);
  int len = strlen(path);
  int i;
  for (i = len; i >= 0 && path[i] != '.'; --i);
  // We expect ".fart" at the end of the path
  assert(i >= 0 && len - i == 5);
  strcpy(out + i + 1, "out");
  return out;
}

uint64_t WrappedWrite(Vector *args) {
  assert(args->size == 3);
  int fd = (int)(int64_t)args->arr[0];
  void *buf = args->arr[1];
  size_t count = (size_t)args->arr[2];
  return (uint64_t)write(fd, buf, count);
}

int main(int argc, char **argv) {
  if (argc == 1) {
    SyntaxSetupParser();
  } else {
    assert(argc == 2 || argc == 3);
    const char *path = argv[1];
    bool enableDebug = false;
    if (argc == 3) {
      const char *debug = argv[2];
      assert(strcmp(debug, "--debug") == 0);
      enableDebug = true;
    }
    IrProgram *program = IrgenFromFile(path);
    assert(program);
    if (enableDebug) {
      IrProgramPrint(stdout, program);
      fflush(stdout);
    }

    Interpreter interpreter;
    InterpreterInit(&interpreter);
    InterpreterRegisterCFunc(&interpreter, WrappedWrite, "write");
    char *out = GetOutFilename(path);
    freopen(out, "w", stdout);
    InterpreterRun(&interpreter, program);
    InterpreterDelete(&interpreter);
    IrProgramDelete(program);
    free(out);
  }
}

