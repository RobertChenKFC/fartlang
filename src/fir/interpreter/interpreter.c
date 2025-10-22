#include "fir/interpreter/interpreter.h"
#include <assert.h>
#include <endian.h>

// Run the current operation in "interpreter->pc", and update "interpreter->pc"
// to the next operation
void InterpreterStep(Interpreter *interpeter);
// Set the current value of variable "var" in "interpreter" to "val"
void InterpreterSetVarVal(Interpreter *interpeter, IrVar *var, uint64_t val);
// Get the current value of variable "var" in "interpreter"
uint64_t InterpreterGetVarVal(Interpreter *interpeter, IrVar *var);
// Run the operation "op" and update the state of the "interpreter"
void InterpreterRunOp(Interpreter *interpreter, IrOp *op);
// Run the constant operation "op"
void InterpreterRunOpConst(Interpreter *interpreter, IrOp *op);
// Run the call operation "op"
void InterpreterRunOpCall(Interpreter *interpreter, IrOp *op);
// Get the wrapped C function with symbol "name"
InterpreterCFunc InterpreterGetCFunc(
    Interpreter *interpreter, const char *name);
// Checks if the integer value "val" evaluates to true in fartlang semantics
bool InterpreterValIsTrue(uint64_t val);

void InterpreterInit(Interpreter *interpreter) {
  interpreter->pc = NULL;
  interpreter->cFuncs = HashTableNew(
      HashTableStringHash, HashTableStringEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  interpreter->vars = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  interpreter->stack = VectorNew();
  interpreter->args = VectorNew();
}

void InterpreterDelete(Interpreter *interpreter) {
  HashTableDelete(interpreter->cFuncs);
  HashTableDelete(interpreter->vars);
  VectorDelete(interpreter->stack);
  VectorDelete(interpreter->args);
}

void InterpreterRun(Interpreter *interpreter, IrProgram *program) {
  HashTableClear(interpreter->vars);
  VectorClear(interpreter->stack);
  // Push a NULL operation onto the stack. When the interpreter finishes
  // executing the entry function, it will pop the NULL operation from
  // the stack and successfully exit the run
  VectorAdd(interpreter->stack, NULL);

  IrFunc *entryFunc = IrProgramGetEntryFunc(program);
  assert(entryFunc);
  IrBasicBlock *entryBlock = IrFuncGetEntryBlock(entryFunc);
  assert(entryBlock);
  IrOp *op = IrBasicBlockGetFirstOp(entryBlock);
  assert(op);
  interpreter->pc = op;

  while (interpreter->pc) {
    InterpreterStep(interpreter);
  }
}

void InterpreterStep(Interpreter *interpreter) {
  IrOp *op = interpreter->pc;
  InterpreterRunOp(interpreter, op);
  IrOp *nextOp = IrOpGetNextOp(op);
  if (nextOp) {
    interpreter->pc = nextOp;
    return;
  }

  // If there is no next operation in the block, that means this is the last
  // operation in the block, so the next operation is in a different block
  IrBasicBlock *block = IrOpGetParentBasicBlock(op);
  if (IrBasicBlockIsExit(block)) {
    // This block is the exit block, so return to the caller
    IrVar *retVar = IrBasicBlockGetRet(block);
    if (retVar) {
      interpreter->ret = InterpreterGetVarVal(interpreter, retVar);
    }
    interpreter->pc = VectorPop(interpreter->stack);
  } else {
    IrBasicBlock *nextBlock;
    IrVar *condVar = IrBasicBlockGetCond(block);
    if (condVar) {
      // This block is a conditional block, so retrieve the condition value and
      // jump to the true/false block based on the condition
      assert(condVar);
      uint64_t condVal = InterpreterGetVarVal(interpreter, condVar);
      nextBlock =
          InterpreterValIsTrue(condVal) ? IrBasicBlockGetTrueBlock(block)
                                        : IrBasicBlockGetFalseBlock(block);
    } else {
      // This block is an unconditional block, so retrieve the block to jump to
      nextBlock = IrBasicBlockGetTrueBlock(block);
    }
    assert(nextBlock);
    interpreter->pc = IrBasicBlockGetFirstOp(nextBlock);
  }
}

void InterpreterSetVarVal(Interpreter *interpreter, IrVar *var, uint64_t val) {
  HashTableEntryAdd(interpreter->vars, var, (void*)val);
}

uint64_t InterpreterGetVarVal(Interpreter *interpreter, IrVar *var) {
  HashTableEntry *entry = HashTableEntryRetrieve(interpreter->vars, var);
  assert(entry);
  return (uint64_t)entry->value;
}

void InterpreterRunOp(Interpreter *interpreter, IrOp *op) {
  switch (op->kind) {
    case IR_OP_KIND_CONST:
    case IR_OP_KIND_CONST_ADDR:
    case IR_OP_KIND_CONST_FN:
    case IR_OP_KIND_CONST_CFN:
      InterpreterRunOpConst(interpreter, op);
      break;
    case IR_OP_KIND_CALL:
      InterpreterRunOpCall(interpreter, op);
      break;
    default:
      printf("Op kind: %d\n", op->kind);
      assert(false);
  }
}

void InterpreterRunOpConst(Interpreter *interpreter, IrOp *op) {
  uint64_t val;
  switch (op->kind) {
    case IR_OP_KIND_CONST:
      val = op->constant.val;
      break;
    case IR_OP_KIND_CONST_ADDR:
      val = (uint64_t)op->constant.addr;
      break;
    case IR_OP_KIND_CONST_FN:
      val = (uint64_t)op->constant.func;
      break;
    case IR_OP_KIND_CONST_CFN:
      val = (uint64_t)op->constant.symbol;
      break;
    default:
      assert(false);
  }
  InterpreterSetVarVal(interpreter, op->constant.dst, val);
}

void InterpreterRunOpCall(Interpreter *interpreter, IrOp *op) {
  IrVar **argVars = op->call.args;
  int numArgs = op->call.numArgs;
  uint64_t retVal;
  IrVar *func = op->call.func;
  switch (func->type) {
    case IR_TYPE_FN: {
      assert(false);
    } case IR_TYPE_CFN: {
      Vector *args = interpreter->args;
      VectorClear(args);
      for (int i = 0; i < numArgs; ++i) {
        uint64_t val = InterpreterGetVarVal(interpreter, argVars[i]);
        val = le64toh(val);
        VectorAdd(args, (void*)val);
      }
      char *symbol = (char*)InterpreterGetVarVal(interpreter, func);
      InterpreterCFunc func = InterpreterGetCFunc(interpreter, symbol);
      retVal = func(args);
      break;
    } default: {
      printf("Var type: %d\n", func->type);
      assert(false);
    }
  }
  IrVar *dst = op->call.dst;
  if (dst) {
    InterpreterSetVarVal(interpreter, dst, retVal);
  }
}

void InterpreterRegisterCFunc(
    Interpreter *interpreter, InterpreterCFunc func, const char *name) {
  HashTableEntryAdd(interpreter->cFuncs, (void*)name, (void*)func);
}

InterpreterCFunc InterpreterGetCFunc(
    Interpreter *interpreter, const char *name) {
  HashTableEntry *entry = HashTableEntryRetrieve(
      interpreter->cFuncs, (void*)name);
  assert(entry);
  return (InterpreterCFunc)entry->value;
}

bool InterpreterValIsTrue(uint64_t val) {
  return val != 0;
}
