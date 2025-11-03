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

  // 2. A program with simple conditionals
  program = IrExamplesCond();
  file = fopen("2.out", "w");
  IrProgramPrint(file, program);
  fclose(file);
  IrProgramDelete(program);

  // 3. A program with empty basic blocks
  program = IrExamplesEmptyBasicBlock(/*emptyEntryBlock=*/true);
  file = fopen("3.out", "w");
  IrProgramPrint(file, program);
  fclose(file);
  IrProgramDelete(program);

  // 4. A program with empty basic blocks
  program = IrExamplesEmptyBasicBlock(/*emptyEntryBlock=*/false);
  file = fopen("4.out", "w");
  IrProgramPrint(file, program);
  fclose(file);
  IrProgramDelete(program);

  // 5. A program that tests return values
  program = IrExamplesReturnValue();
  file = fopen("5.out", "w");
  IrProgramPrint(file, program);
  fclose(file);
  IrProgramDelete(program);
}
