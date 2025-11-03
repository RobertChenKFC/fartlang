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
  Interpreter interpreter;
  InterpreterInit(&interpreter);

  // 1. A straightline program with simple operations
  IrProgram *program = IrExamplesHello();
  InterpreterRegisterCFunc(&interpreter, WrappedWrite, "write");
  freopen("1.out", "w", stdout);
  InterpreterRun(&interpreter, program);
  IrProgramDelete(program);

  // 2. A program with simple conditionals
  program = IrExamplesCond();
  freopen("2.out", "w", stdout);
  InterpreterRun(&interpreter, program);
  IrProgramDelete(program);

  // 3. A program with empty basic blocks
  program = IrExamplesEmptyBasicBlock(/*emptyBasicBlock=*/true);
  freopen("3.out", "w", stdout);
  InterpreterRun(&interpreter, program);
  IrProgramDelete(program);

  // 4. A program with empty basic blocks but non-empty entry block
  program = IrExamplesEmptyBasicBlock(/*emptyBasicBlock=*/false);
  freopen("4.out", "w", stdout);
  InterpreterRun(&interpreter, program);
  IrProgramDelete(program);

  // 5. A program that tests return values
  program = IrExamplesReturnValue();
  freopen("5.out", "w", stdout);
  InterpreterRun(&interpreter, program);
  IrProgramDelete(program);

  InterpreterDelete(&interpreter);
}
