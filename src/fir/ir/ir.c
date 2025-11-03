#include "fir/ir/ir.h"
#include "util/hashtable/hashtable.h"
#include <assert.h>
#include <endian.h>
#include <stdbool.h>
#include <stdlib.h>

// Forward declarations
typedef struct IrPrinter IrPrinter;

struct IrPrinter {
  // The file to print the IR to
  FILE *file;
  // A mapping from IrModule* to integer id
  HashTable *moduleIdMap;
  // A mapping from IrFunc* to integer id
  HashTable *funcIdMap;
  // A mapping from IrBasicBlock* to integer id. This mapping is cleared on
  // every new function
  HashTable *blockIdMap;
  // A mapping from IrVar* to integer id. This mapping is cleared on every new
  // function
  HashTable *varIdMap;
};

// Initialize a "printer" to "file" and initialize empty maps
void IrPrinterInit(IrPrinter *printer, FILE *file);
// Delete a "printer" initialized with IrPrinterInit
void IrPrinterDelete(IrPrinter *printer);
// Retrieves the id for "irObj" from the mapping "idMap". If the mapping does
// not exist for "irObj", create a new id for "irObj". Special case: if "irObj"
// is NULL, then returns -1
int IrPrinterId(HashTable *idMap, void *irObj);
// Specific id function for modules
int IrPrinterModuleId(IrPrinter *printer, IrModule *module);
// Specific id function for functions
int IrPrinterFuncId(IrPrinter *printer, IrFunc *func);
// Specific id function for basic blocks
int IrPrinterBasicBlockId(IrPrinter *printer, IrBasicBlock *block);
// Specific id function for variables
int IrPrinterVarId(IrPrinter *printer, IrVar *var);
// Implementation of IrProgramPrint, which uses an internal "printer"
void IrProgramPrintImpl(IrPrinter *printer, IrProgram *prog);
// Implementation of IrModulePrint, which uses an internal "printer"
void IrModulePrintImpl(IrPrinter *printer, IrModule *module);
// Implementation of IrFuncPrint, which uses an internal "printer"
void IrFuncPrintImpl(IrPrinter *printer, IrFunc *func);
// Implementation of IrBasicBlockPrint, which uses an internal "printer"
void IrBasicBlockPrintImpl(IrPrinter *printer, IrBasicBlock *block);
// Implementation of IrVarPrint, which uses an internal "printer"
void IrVarPrintImpl(IrPrinter *printer, IrVar *var, bool printType);
// Implementation of IrTypePrint, which uses an internal "printer"
void IrTypePrintImpl(IrPrinter *printer, IrType type);
// Implementation of IrOpPrint, which uses an internal "printer"
void IrOpPrintImpl(IrPrinter *printer, IrOp *op);
// Implementation of IrOpConstPrint, which uses an internal "printer"
void IrOpConstPrintImpl(IrPrinter *printer, IrOp *op);
// Implementation of IrOpCallPrint, which uses an internal "printer"
void IrOpCallPrintImpl(IrPrinter *printer, IrOp *op);
// Delete an operation "op" created with IrOpNewConst, IrOpNewConstAddr,
// IrOpNewConstFn or IrOpNewConstCfn
void IrOpDeleteConst(IrOp *op);
// Delete an operation "op" created with IrOpNewCall
void IrOpDeleteCall(IrOp *op);
// Get the byte size of the IR "type"
int IrTypeGetSize(IrType type);

// Macros
// Given the pointer variables to the "firstNode" and "lastNode" of the list,
// the pointer to the "node" to append, and the identifier "nextEntry" for
// accessing the next pointer of a node (eg. "firstNode"->"nextEntry" should
// give me second node in the list), add the "node" to the end of the list
#define IrLinkedListAppend(firstNode, lastNode, node, nextEntry) \
  do { \
    if (lastNode) { \
      (lastNode)->nextEntry = (node); \
    } else { \
      (firstNode) = (node); \
    } \
    (node)->nextEntry = NULL; \
    (lastNode) = (node); \
  } while (0)
// Similar to IrLinkedListAppend, but for double linked list. Has additional
// identifier "prevEntry" that is used to access the previous pointer of a node
#define IrDoubleLinkedListAppend( \
    firstNode, lastNode, node, prevEntry, nextEntry) \
  do { \
    if (lastNode) { \
      (lastNode)->nextEntry = (node); \
      (node)->prevEntry = (lastNode); \
    } else { \
      (firstNode) = (node); \
    } \
    (node)->nextEntry = NULL; \
    (lastNode) = node; \
  } while (0)
// Similar to IrLinkedListAppend, but adds the node to the front of the list
#define IrLinkedListPrepend(firstNode, node, nextEntry) \
  do { \
    (node)->nextEntry = (firstNode); \
    (firstNode) = (node); \
  } while (0)

IrProgram *IrProgramNew(void) {
  IrProgram *prog = malloc(sizeof(IrProgram));
  prog->firstModule = prog->lastModule = NULL;
  prog->entryFunc = NULL;
  return prog;
}

void IrProgramDelete(IrProgram *prog) {
  IrForModule(prog, module) {
    IrModuleDelete(module);
  }
  free(prog);
}

void IrProgramSetEntryFunc(IrProgram *prog, IrFunc *func) {
  prog->entryFunc = func;
}

IrFunc *IrProgramGetEntryFunc(IrProgram *prog) {
  return prog->entryFunc;
}

void IrProgramPrint(FILE *file, IrProgram *prog) {
  IrPrinter printer;
  IrPrinterInit(&printer, file);
  IrProgramPrintImpl(&printer, prog);
  IrPrinterDelete(&printer);
}

IrModule *IrModuleAdd(IrProgram *prog) {
  IrModule *module = malloc(sizeof(IrModule));
  module->firstFunc = module->lastFunc = NULL;
  IrLinkedListAppend(prog->firstModule, prog->lastModule, module, nextModule);
  return module;
}

void IrModuleDelete(IrModule *module) {
  IrForFunc(module, func) {
    IrFuncDelete(func);
  }
  free(module);
}

IrFunc *IrFuncAdd(IrModule *module) {
  IrFunc *func = malloc(sizeof(IrFunc));
  func->params = VectorNew();
  func->firstBlock = func->lastBlock = NULL;
  func->entryBlock = NULL;
  func->firstVar = NULL;
  func->freeVars = VectorNew();
  IrLinkedListAppend(module->firstFunc, module->lastFunc, func, nextFunc);
  func->module = module;
  return func;
}

void IrFuncDelete(IrFunc *func) {
  VectorDelete(func->params);
  IrForBasicBlock(func, block) {
    IrBasicBlockDelete(block);
  }
  IrForVar(func, var) {
    IrVarDelete(var);
  }
  for (int i = 0; i < func->freeVars->size; ++i) {
    IrVarDelete(func->freeVars->arr[i]);
  }
  VectorDelete(func->freeVars);
  free(func);
}

IrBasicBlock *IrBasicBlockAdd(IrFunc *func) {
  IrBasicBlock *block = malloc(sizeof(IrBasicBlock));
  block->firstOp = block->lastOp = NULL;
  block->cond = NULL;
  block->trueBlock = block->falseBlock = NULL;
  IrDoubleLinkedListAppend(
      func->firstBlock, func->lastBlock, block, prevBlock, nextBlock);
  block->func = func;
  return block;
}

IrOp *IrBasicBlockGetFirstOp(IrBasicBlock *block) {
  return block->firstOp;
}

bool IrBasicBlockIsCond(IrBasicBlock *block) {
  return block->falseBlock != NULL;
}

void IrBasicBlockSetCond(IrBasicBlock *block, IrVar *cond) {
  block->cond = cond;
}

IrVar *IrBasicBlockGetCond(IrBasicBlock *block) {
  return IrBasicBlockIsCond(block) ? block->cond : NULL;
}

bool IrBasicBlockIsExit(IrBasicBlock *block) {
  if (!block->trueBlock) {
    assert(!block->falseBlock);
    return true;
  }
  return false;
}

void IrBasicBlockSetRet(IrBasicBlock *block, IrVar *ret) {
  assert(IrBasicBlockIsExit(block));
  block->ret = ret;
}

IrVar *IrBasicBlockGetRet(IrBasicBlock *block) {
  return IrBasicBlockIsExit(block) ? block->ret : NULL;
}

void IrBasicBlockSetTrueBlock(IrBasicBlock *block, IrBasicBlock *trueBlock) {
  block->trueBlock = trueBlock;
}

IrBasicBlock *IrBasicBlockGetTrueBlock(IrBasicBlock *block) {
  return block->trueBlock;
}

void IrBasicBlockSetFalseBlock(IrBasicBlock *block, IrBasicBlock *falseBlock) {
  block->falseBlock = falseBlock;
}

IrBasicBlock *IrBasicBlockGetFalseBlock(IrBasicBlock *block) {
  return block->falseBlock;
}

IrFunc *IrBasicBlockGetParentFunc(IrBasicBlock *block) {
  return block->func;
}

void IrBasicBlockDelete(IrBasicBlock *block) {
  IrForOp(block, op) {
    IrOpDelete(op);
  }
  free(block);
}

void IrFuncSetEntryBlock(IrFunc *func, IrBasicBlock *block) {
  func->entryBlock = block;
}

IrBasicBlock *IrFuncGetEntryBlock(IrFunc *func) {
  return func->entryBlock;
}

IrVar *IrFuncAddVar(IrFunc *func, IrType type) {
  IrVar *var = malloc(sizeof(IrVar));
  var->type = type;
  IrLinkedListPrepend(func->firstVar, var, nextVar);
  return var;
}

Vector* IrFuncGetParams(IrFunc *func) {
  return func->params;
}

void IrVarDelete(IrVar *var) {
  free(var);
}

void IrOpAppend(IrBasicBlock *block, IrOp *op) {
  op->block = block;
  IrDoubleLinkedListAppend(block->firstOp, block->lastOp, op, prevOp, nextOp);
}

IrOp *IrOpGetNextOp(IrOp *op) {
  return op->nextOp;
}

IrBasicBlock *IrOpGetParentBasicBlock(IrOp *op) {
  return op->block;
}

IrOp *IrOpNewConstAddr(IrVar *dst, uint8_t *addr, int len) {
  IrOp *op = malloc(sizeof(IrOp));
  op->kind = IR_OP_KIND_CONST_ADDR;
  op->constant.dst = dst;
  op->constant.addr = addr;
  op->constant.len = len;
  return op;
}

IrOp *IrOpNewConst(IrVar *dst, uint64_t val) {
  IrOp *op = malloc(sizeof(IrOp));
  op->kind = IR_OP_KIND_CONST;
  op->constant.dst = dst;
  op->constant.val = htole64(val);
  return op;
}

IrOp *IrOpNewConstFn(IrVar *dst, IrFunc *func) {
  IrOp *op = malloc(sizeof(IrOp));
  op->kind = IR_OP_KIND_CONST_FN;
  op->constant.dst = dst;
  op->constant.func = func;
  return op;
}

IrOp *IrOpNewConstCfn(IrVar *dst, char *symbol) {
  IrOp *op = malloc(sizeof(IrOp));
  op->kind = IR_OP_KIND_CONST_CFN;
  op->constant.dst = dst;
  op->constant.symbol = symbol;
  return op;
}

IrOp *IrOpNewCall(IrVar *dst, IrVar *func, int numArgs, IrVar **args) {
  IrOp *op = malloc(sizeof(IrOp));
  op->kind = IR_OP_KIND_CALL;
  op->call.dst = dst;
  op->call.func = func;
  op->call.args = args;
  op->call.numArgs = numArgs;
  return op;
}

void IrOpDelete(IrOp *op) {
  switch (op->kind) {
    case IR_OP_KIND_CONST:
    case IR_OP_KIND_CONST_ADDR:
    case IR_OP_KIND_CONST_FN:
    case IR_OP_KIND_CONST_CFN:
      IrOpDeleteConst(op);
      break;
    case IR_OP_KIND_CALL:
      IrOpDeleteCall(op);
      break;
    default:
      printf("Op kind: %d\n", op->kind);
      assert(false);
  }
}

void IrPrinterInit(IrPrinter *printer, FILE *file) {
  printer->file = file;
  printer->moduleIdMap = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  printer->funcIdMap = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  printer->blockIdMap = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  printer->varIdMap = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
}

void IrPrinterDelete(IrPrinter *printer) {
  HashTableDelete(printer->moduleIdMap);
  HashTableDelete(printer->funcIdMap);
  HashTableDelete(printer->blockIdMap);
  HashTableDelete(printer->varIdMap);
}

int IrPrinterId(HashTable *idMap, void *irObj) {
  if (!irObj) {
    return -1;
  }
  HashTableEntry *entry = HashTableEntryRetrieve(idMap, irObj);
  int id;
  if (entry) {
    id = (int)(int64_t)entry->value;
  } else {
    HashTableEntryAdd(idMap, irObj, (void*)(int64_t)(id = idMap->size));
  }
  return id;
}

int IrPrinterModuleId(IrPrinter *printer, IrModule *module) {
  return IrPrinterId(printer->moduleIdMap, module);
}

int IrPrinterFuncId(IrPrinter *printer, IrFunc *func) {
  return IrPrinterId(printer->funcIdMap, func);
}

int IrPrinterBasicBlockId(IrPrinter *printer, IrBasicBlock *block) {
  return IrPrinterId(printer->blockIdMap, block);
}

int IrPrinterVarId(IrPrinter *printer, IrVar *var) {
  return IrPrinterId(printer->varIdMap, var);
}

void IrProgramPrintImpl(IrPrinter *printer, IrProgram *prog) {
  assert(prog->entryFunc);
  fprintf(printer->file, "entry: f%d\n",
          IrPrinterFuncId(printer, prog->entryFunc));
  IrForModule(prog, module) {
    IrModulePrintImpl(printer, module);
  }
}

void IrModulePrintImpl(IrPrinter *printer, IrModule *module) {
  fprintf(printer->file, "m%d:\n", IrPrinterModuleId(printer, module));
  IrForFunc(module, func) {
    IrFuncPrintImpl(printer, func);
  }
}

void IrFuncPrintImpl(IrPrinter *printer, IrFunc *func) {
  fprintf(printer->file, "\nf%d(", IrPrinterFuncId(printer, func));
  bool isFirstParam = true;
  IrForParam(func, param) {
    if (isFirstParam) {
      isFirstParam = false;
    } else {
      fprintf(printer->file, ", ");
    }
    IrVarPrintImpl(printer, param, /*printType=*/true);
  }
  fprintf(printer->file, "):\n");
  assert(func->entryBlock);
  fprintf(printer->file, "entry: b%d\n",
          IrPrinterBasicBlockId(printer, func->entryBlock));
  IrForBasicBlock(func, block) {
    IrBasicBlockPrintImpl(printer, block);
  }
}

void IrBasicBlockPrintImpl(IrPrinter *printer, IrBasicBlock *block) {
  fprintf(printer->file, "b%d:\n", IrPrinterBasicBlockId(printer, block));
  IrForOp(block, op) {
    IrOpPrintImpl(printer, op);
  }
  fprintf(printer->file, "next: ");
  if (block->trueBlock) {
    if (block->cond) {
      assert(block->falseBlock);
      IrVarPrintImpl(printer, block->cond, /*printType=*/false);
      int trueBlockId = IrPrinterBasicBlockId(printer, block->trueBlock);
      int falseBlockId = IrPrinterBasicBlockId(printer, block->falseBlock);
      fprintf(printer->file, " ? b%d : b%d", trueBlockId, falseBlockId);
    } else {
      assert(!block->falseBlock);
      fprintf(printer->file, "b%d",
              IrPrinterBasicBlockId(printer, block->trueBlock));
    }
  } else {
    assert(!block->falseBlock);
    if (block->ret) {
      fprintf(printer->file, "return ");
      IrVarPrintImpl(printer, block->ret, /*printType=*/false);
    }
  }
  fprintf(printer->file, "\n");
}

void IrVarPrintImpl(IrPrinter *printer, IrVar *var, bool printType) {
  fprintf(printer->file, "v%d", IrPrinterVarId(printer, var));
  if (printType) {
    fprintf(printer->file, " : ");
    IrTypePrintImpl(printer, var->type);
  }
}

void IrTypePrintImpl(IrPrinter *printer, IrType type) {
  switch (type) {
    case IR_TYPE_U64:
      fprintf(printer->file, "u64");
      break;
    case IR_TYPE_I32:
      fprintf(printer->file, "i32");
      break;
    case IR_TYPE_U8:
      fprintf(printer->file, "u8");
      break;
    case IR_TYPE_FN:
      fprintf(printer->file, "fn");
      break;
    case IR_TYPE_CFN:
      fprintf(printer->file, "cfn");
      break;
    default:
      printf("IR type: %d\n", type);
      assert(false);
  }
}

void IrOpPrintImpl(IrPrinter *printer, IrOp *op) {
  fprintf(printer->file, "  ");
  switch (op->kind) {
    case IR_OP_KIND_CONST:
    case IR_OP_KIND_CONST_ADDR:
    case IR_OP_KIND_CONST_FN:
    case IR_OP_KIND_CONST_CFN:
      IrOpConstPrintImpl(printer, op);
      break;
    case IR_OP_KIND_CALL:
      IrOpCallPrintImpl(printer, op);
      break;
    default:
      printf("Op kind: %d\n", op->kind);
      assert(false);
  }
}

void IrOpConstPrintImpl(IrPrinter *printer, IrOp *op) {
  IrVarPrintImpl(printer, op->constant.dst, /*printType=*/true);
  fprintf(printer->file, " = ");
  uint8_t *addr;
  int len;
  bool printVal = false;
  switch (op->kind) {
    case IR_OP_KIND_CONST:
      addr = (uint8_t*)&op->constant.val;
      len = IrTypeGetSize(op->constant.dst->type);
      printVal = true;
      break;
    case IR_OP_KIND_CONST_ADDR:
      fprintf(printer->file, "&");
      addr = op->constant.addr;
      len = op->constant.len;
      printVal = true;
      break;
    case IR_OP_KIND_CONST_FN:
      fprintf(
          printer->file, "m%d.f%d",
          IrPrinterModuleId(printer, op->constant.func->module),
          IrPrinterFuncId(printer, op->constant.func));
      break;
    case IR_OP_KIND_CONST_CFN:
      fprintf(printer->file, "\"%s\"", op->constant.symbol);
      break;
    default:
      printf("Op kind: %d\n", op->kind);
      assert(false);
  }
  if (printVal) {
    bool isAscii = true;
    for (int i = 0; i < len; ++i) {
      fprintf(printer->file, "%02x", addr[i]);
      if (i == len - 1) {
        isAscii &= addr[i] == (uint8_t)'\0';
      } else {
        isAscii &= 0 < addr[i] && addr[i] <= 127;
      }
    }
    if (op->kind == IR_OP_KIND_CONST_ADDR && isAscii) {
      fprintf(printer->file, " (\"");
      for (int i = 0; i < len; ++i) {
        char c = (char)addr[i];
        switch (c) {
          case '\n': fprintf(printer->file, "\\n"); break;
          case '\r': fprintf(printer->file, "\\r"); break;
          case '\t': fprintf(printer->file, "\\t"); break;
          default: fputc(c, printer->file);
        }
      }
      fprintf(printer->file, "\")");
    }
  }
  fprintf(printer->file, "\n");
}

void IrOpCallPrintImpl(IrPrinter *printer, IrOp *op) {
  if (op->call.dst) {
    IrVarPrintImpl(printer, op->call.dst, /*printType=*/false);
    fprintf(printer->file, " = ");
  }
  fprintf(printer->file, "call v%d(", IrPrinterVarId(printer, op->call.func));
  for (int i = 0; i < op->call.numArgs; ++i) {
    if (i != 0) {
      fprintf(printer->file, ", ");
    }
    IrVarPrintImpl(printer, op->call.args[i], /*printType=*/false);
  }
  fprintf(printer->file, ")\n");
}

void IrOpDeleteConst(IrOp *op) {
  switch (op->kind) {
    case IR_OP_KIND_CONST_ADDR:
      free(op->constant.addr);
      break;
    case IR_OP_KIND_CONST:
    case IR_OP_KIND_CONST_FN:
      break;
    case IR_OP_KIND_CONST_CFN:
      free(op->constant.symbol);
      break;
    default:
      printf("Op kind: %d\n", op->kind);
      assert(false);
  }
  free(op);
}

void IrOpDeleteCall(IrOp *op) {
  free(op->call.args);
  switch (op->kind) {
    case IR_OP_KIND_CALL:
      break;
    default:
      printf("Op kind: %d\n", op->kind);
      assert(false);
  }
  free(op);
}

IrOp *IrOpNewCopy(IrVar *dst, IrVar *src) {
  IrOp *op = malloc(sizeof(IrOp));
  op->kind = IR_OP_KIND_COPY;
  op->unary.dst = dst;
  op->unary.src = src;
  return op;
}

int IrTypeGetSize(IrType type) {
  switch (type) {
    case IR_TYPE_U64:
    case IR_TYPE_I64:
      return 8;
    case IR_TYPE_U32:
    case IR_TYPE_I32:
      return 4;
    case IR_TYPE_U16:
    case IR_TYPE_I16:
      return 2;
    case IR_TYPE_U8:
    case IR_TYPE_I8:
      return 1;
    default:
      printf("IR type: %d\n", type);
      assert(false);
  }
}

IrOpKind IrOpGetKind(IrOp *op) {
  return op->kind;
}
