#ifndef EXAMPLES_H
#define EXAMPLES_H

#include "fir/ir/ir.h"

// Creates a program that prints "Hello world!\n" to stdout
IrProgram *IrExamplesHello(void);
// Creates a program that tests conditional blocks
IrProgram *IrExamplesCond(void);
// Creates a program with empty basic blocks. If "emptyEntryBlock" is false,
// the entry block is populated, otherwise it is also empty
IrProgram *IrExamplesEmptyBasicBlock(bool emptyEntryBlock);
// Creates a program that tests return value
IrProgram *IrExamplesReturnValue();

#endif // EXAMPLES_H
