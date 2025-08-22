#include "fir/examples/examples.h"
#include "fir/ir/ir.h"
#include <stdio.h>

int main(void) {
  // 1. A straightline program with simple operations
  IrProgram *program = IrExamplesHello();
  FILE *file = fopen("1.out", "w");
  IrProgramPrint(file, program);
  fclose(file);
  IrProgramDelete(program);
}
