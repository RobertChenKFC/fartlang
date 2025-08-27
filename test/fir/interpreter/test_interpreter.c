#include "fir/examples/examples.h"
#include "fir/interpreter/interpreter.h"
#include "util/vector/vector.h"
#include <assert.h>
#include <stdio.h>
#include <unistd.h>

uint64_t WrappedWrite(Vector *args) {
  assert(args->size == 3);
  int fd = (int)(int64_t)args->arr[0];
  void *buf = args->arr[1];
  size_t count = (size_t)args->arr[2];
  return (uint64_t)write(fd, buf, count);
}

int main(void) {
  // 1. A straightline program with simple operations
  IrProgram *program = IrExamplesHello();
  Interpreter interpreter;
  InterpreterInit(&interpreter);
  InterpreterRegisterCFunc(&interpreter, WrappedWrite, "write");
  freopen("1.out", "w", stdout);
  InterpreterRun(&interpreter, program);
  InterpreterDelete(&interpreter);
  IrProgramDelete(program);
}
