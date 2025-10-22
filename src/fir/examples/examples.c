#include "fir/examples/examples.h"
#include <stdlib.h>
#include <string.h>

IrProgram *IrExamplesHello(void) {
  IrProgram *program = IrProgramNew();
  IrModule *module = IrModuleAdd(program);
  IrFunc *entryFunc = IrFuncAdd(module);
  IrProgramSetEntryFunc(program, entryFunc);
  IrBasicBlock *entryBlock = IrBasicBlockAdd(entryFunc);
  IrFuncSetEntryBlock(entryFunc, entryBlock);
  IrFuncSetExitBlock(entryFunc, entryBlock);
  IrVar *fd = IrFuncAddVar(entryFunc, IR_TYPE_I32);
  IrVar *hello = IrFuncAddVar(entryFunc, IR_TYPE_ADDR);
  IrVar *len = IrFuncAddVar(entryFunc, IR_TYPE_U64);
  IrVar *write = IrFuncAddVar(entryFunc, IR_TYPE_CFN);
  // stdout is fd 1
  IrOpAppend(entryBlock, IrOpNewConst(fd, 1));
  const char *helloStr = "Hello world!\n";
  int helloStrLen = strlen(helloStr);
  IrOpAppend(entryBlock, IrOpNewConstAddr(
        hello, (uint8_t*)strdup(helloStr), helloStrLen));
  IrOpAppend(entryBlock, IrOpNewConst(len, helloStrLen));
  IrOpAppend(entryBlock, IrOpNewConstCfn(write, strdup("write")));
  int numWriteArgs = 3;
  IrVar **writeArgs = malloc(sizeof(IrVar*) * numWriteArgs);
  writeArgs[0] = fd;
  writeArgs[1] = hello;
  writeArgs[2] = len;
  IrOpAppend(entryBlock, IrOpNewCall(
        /*dst=*/NULL, write, numWriteArgs, writeArgs));
  return program;
}
