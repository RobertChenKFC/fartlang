#include "fir/examples/examples.h"
#include <stdlib.h>
#include <string.h>

// Generate the IR to print "str" using the external "write" function
void IrExamplesPrintStr(IrBasicBlock *block, const char *str);
// Generate a function in "module" that returns "val" of type "type"
IrFunc *IrExamplesConstructFunc(IrModule *module, IrType type, uint64_t val);
// Generate the IR to print "trueMsg" or "falseMsg" based on whether "cond"
// is true or false
IrBasicBlock *IrExamplesPrintCondMsg(
    IrBasicBlock *block, IrVar *cond, const char *trueMsg,
    const char *falseMsg);

IrProgram *IrExamplesHello(void) {
  IrProgram *program = IrProgramNew();
  IrModule *module = IrModuleAdd(program);
  IrFunc *entryFunc = IrFuncAdd(module);
  IrProgramSetEntryFunc(program, entryFunc);
  IrBasicBlock *entryBlock = IrBasicBlockAdd(entryFunc);
  IrFuncSetEntryBlock(entryFunc, entryBlock);
  IrExamplesPrintStr(entryBlock, "Hello world!\n");
  return program;
}

void IrExamplesPrintStr(IrBasicBlock *block, const char *str) {
  IrFunc *func = IrBasicBlockGetParentFunc(block);
  IrVar *fd = IrFuncAddVar(func, IR_TYPE_I32);
  IrVar *hello = IrFuncAddVar(func, IR_TYPE_ADDR);
  IrVar *len = IrFuncAddVar(func, IR_TYPE_U64);
  IrVar *write = IrFuncAddVar(func, IR_TYPE_CFN);
  // stdout is fd 1
  IrOpAppend(block, IrOpNewConst(fd, 1));
  int lenVal = strlen(str);
  IrOpAppend(block, IrOpNewConstAddr(
        hello, (uint8_t*)strdup(str), lenVal));
  IrOpAppend(block, IrOpNewConst(len, lenVal));
  IrOpAppend(block, IrOpNewConstCfn(write, strdup("write")));
  int numWriteArgs = 3;
  IrVar **writeArgs = malloc(sizeof(IrVar*) * numWriteArgs);
  writeArgs[0] = fd;
  writeArgs[1] = hello;
  writeArgs[2] = len;
  IrOpAppend(block, IrOpNewCall(
        /*dst=*/NULL, write, numWriteArgs, writeArgs));
}

IrProgram *IrExamplesCond(void) {
  IrProgram *program = IrProgramNew();
  IrModule *module = IrModuleAdd(program);
  IrFunc *entryFunc = IrFuncAdd(module);
  IrProgramSetEntryFunc(program, entryFunc);
  IrBasicBlock *entryBlock = IrBasicBlockAdd(entryFunc);
  IrFuncSetEntryBlock(entryFunc, entryBlock);


  IrVar *trueCond = IrFuncAddVar(entryFunc, IR_TYPE_U8);
  IrOpAppend(entryBlock, IrOpNewConst(trueCond, true));
  IrBasicBlock *block = IrExamplesPrintCondMsg(
      entryBlock, trueCond, /*trueMsg=*/"1. This line should be printed\n",
      /*falseMsg=*/"1. This line should NOT be printed\n");

  IrVar *falseCond = IrFuncAddVar(entryFunc, IR_TYPE_U8);
  IrOpAppend(block, IrOpNewConst(falseCond, false));
  IrExamplesPrintCondMsg(
      block, falseCond, /*trueMsg=*/"2. This line should NOT be printed\n",
      /*falseMsg=*/"2. This line should be printed\n");

  return program;
}

IrProgram *IrExamplesEmptyBasicBlock(bool emptyEntryBlock) {
  IrProgram *program = IrProgramNew();
  IrModule *module = IrModuleAdd(program);
  IrFunc *entryFunc = IrFuncAdd(module);
  IrProgramSetEntryFunc(program, entryFunc);
  IrBasicBlock *entryBlock = IrBasicBlockAdd(entryFunc);
  IrFuncSetEntryBlock(entryFunc, entryBlock);
  if (!emptyEntryBlock) {
    IrExamplesPrintStr(entryBlock, "Entry block!\n");
  }

  IrBasicBlock *lastBlock = entryBlock;
  for (int i = 0; i < 3; ++i) {
    IrBasicBlock *emptyBlock = IrBasicBlockAdd(entryFunc);
    IrBasicBlockSetTrueBlock(lastBlock, emptyBlock);
    lastBlock = emptyBlock;
  }
  IrExamplesPrintStr(lastBlock, "After a few empty basic blocks...\n");
  return program;
}

IrProgram *IrExamplesReturnValue() {
  IrProgram *program = IrProgramNew();
  IrModule *module = IrModuleAdd(program);
  IrFunc *entryFunc = IrFuncAdd(module);
  IrProgramSetEntryFunc(program, entryFunc);
  IrBasicBlock *entryBlock = IrBasicBlockAdd(entryFunc);
  IrFuncSetEntryBlock(entryFunc, entryBlock);

  IrFunc *trueFunc = IrExamplesConstructFunc(module, IR_TYPE_U8, true);
  IrVar *trueFuncVar = IrFuncAddVar(entryFunc, IR_TYPE_FN);
  IrBasicBlock *block = entryBlock;
  IrOpAppend(block, IrOpNewConstFn(trueFuncVar, trueFunc));
  IrVar *cond = IrFuncAddVar(entryFunc, IR_TYPE_U8);
  IrOpAppend(block, IrOpNewCall(
      cond, trueFuncVar, /*numArgs=*/0, /*args=*/NULL));
  block = IrExamplesPrintCondMsg(
      block, cond, /*trueMsg=*/"1. Correct value from true function\n",
      /*falseMsg=*/"1. INCORRECT value from true function\n");

  IrFunc *falseFunc = IrExamplesConstructFunc(module, IR_TYPE_U8, false);
  IrVar *falseFuncVar = IrFuncAddVar(entryFunc, IR_TYPE_FN);
  IrOpAppend(block, IrOpNewConstFn(falseFuncVar, falseFunc));
  cond = IrFuncAddVar(entryFunc, IR_TYPE_U8);
  IrOpAppend(block, IrOpNewCall(
      cond, falseFuncVar, /*numArgs=*/0, /*args=*/NULL));
  IrExamplesPrintCondMsg(
      block, cond, /*trueMsg=*/"2. INCORRECT value from false function\n",
      /*falseMsg=*/"2. Correct value from false function\n");

  return program;
}

IrFunc *IrExamplesConstructFunc(IrModule *module, IrType type, uint64_t val) {
  IrFunc *func = IrFuncAdd(module);
  IrVar *var = IrFuncAddVar(func, type);
  IrBasicBlock *block = IrBasicBlockAdd(func);
  IrFuncSetEntryBlock(func, block);
  IrOpAppend(block, IrOpNewConst(var, val));
  IrBasicBlockSetRet(block, var);
  return func;
}

IrBasicBlock *IrExamplesPrintCondMsg(
    IrBasicBlock *block, IrVar *cond, const char *trueMsg,
    const char *falseMsg) {
  IrFunc *func = IrBasicBlockGetParentFunc(block);
  IrBasicBlockSetCond(block, cond);
  IrBasicBlock *trueBlock = IrBasicBlockAdd(func);
  IrBasicBlock *falseBlock = IrBasicBlockAdd(func);
  IrBasicBlockSetTrueBlock(block, trueBlock);
  IrBasicBlockSetFalseBlock(block, falseBlock);
  IrExamplesPrintStr(trueBlock, trueMsg);
  IrExamplesPrintStr(falseBlock, falseMsg);
  block = IrBasicBlockAdd(func);
  IrBasicBlockSetTrueBlock(trueBlock, block);
  IrBasicBlockSetTrueBlock(falseBlock, block);
  return block;
}
