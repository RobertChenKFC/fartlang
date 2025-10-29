#include "fir/interpreter/interpreter.h"
#include <alloca.h>
#include <assert.h>
#include <endian.h>

// Update "interpreter->pc" to the next operation
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
// Pushes all the variable values (uint64_t) of "func" onto the stack of
// "interpreter"
void InterpreterPushFuncVars(Interpreter *interpreter, IrFunc *func);
// The opposite operation of "InterpreterPushFuncVars"
void InterpreterPopFuncVars(Interpreter *interpreter, IrFunc *func);
// Initialize all the function variables to 0. This is used beforing calling
// a function to ensure that when calling "InterpreterPushFuncVars" for this
// function, all the function variables have values to be stored
void InterpreterSetupFuncVars(Interpreter *interpreter, IrFunc *func);

void InterpreterInit(Interpreter *interpreter) {
  interpreter->pc = NULL;
  interpreter->retVar = NULL;
  interpreter->cFuncs = HashTableNew(
      HashTableStringHash, HashTableStringEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  interpreter->vars = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, /*keyDelete=*/NULL,
      /*valDelete=*/NULL);
  interpreter->opStack = VectorNew();
  interpreter->varStack = VectorNew();
  interpreter->args = VectorNew();
}

void InterpreterDelete(Interpreter *interpreter) {
  HashTableDelete(interpreter->cFuncs);
  HashTableDelete(interpreter->vars);
  VectorDelete(interpreter->opStack);
  VectorDelete(interpreter->varStack);
  VectorDelete(interpreter->args);
}

void InterpreterRun(Interpreter *interpreter, IrProgram *program) {
  HashTableClear(interpreter->vars);
  VectorClear(interpreter->opStack);
  // Push a NULL operation onto the stack. When the interpreter finishes
  // executing the entry function, it will pop the NULL operation from
  // the stack and successfully exit the run
  VectorAdd(interpreter->opStack, NULL);
  VectorClear(interpreter->varStack);

  IrFunc *entryFunc = IrProgramGetEntryFunc(program);
  InterpreterSetupFuncVars(interpreter, entryFunc);
  assert(entryFunc);
  IrBasicBlock *entryBlock = IrFuncGetEntryBlock(entryFunc);
  assert(entryBlock);
  IrOp *op = IrBasicBlockGetFirstOp(entryBlock);
  assert(op);
  interpreter->pc = op;

  while (interpreter->pc) {
    InterpreterRunOp(interpreter, interpreter->pc);
  }
}

void InterpreterStep(Interpreter *interpreter) {
  IrOp *op = interpreter->pc;
  IrBasicBlock *block = IrOpGetParentBasicBlock(op);
  IrOp *nextOp;
  while (!(nextOp = IrOpGetNextOp(op)) && IrBasicBlockIsExit(block)) {
    // This operation is the last in its basic block, and the block is the exit
    // block, so return to the caller
    op = VectorPop(interpreter->opStack);
    if (!op) {
      // Initially in "InterpreterRun", a NULL is pushed to the bottom of the
      // stack to indicate the caller of the entry function. If we get this
      // NULL op, this means we have returned from the entry function, so we
      // have finished interpreting this program
      interpreter->pc = op;
      return;
    }
    block = IrOpGetParentBasicBlock(op);
    InterpreterPopFuncVars(interpreter, IrBasicBlockGetParentFunc(block));
    IrVar *retVar = IrBasicBlockGetRet(block);
    if (retVar) {
      uint64_t retVal = InterpreterGetVarVal(interpreter, retVar);
      if (interpreter->retVar) {
        InterpreterSetVarVal(interpreter, interpreter->retVar, retVal);
      }
    }
  }

  // This operation is not the last in its basic block, so jump to the next
  if (nextOp) {
    interpreter->pc = nextOp;
    return;
  }

  // The operation is the last in its basic block, and the basic block is not
  // an exit block, so go to the next basic block
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
  // The IR stores constant values in little endian, but all other types of
  // constants in host pointers and values. The interpreter stores everything
  // in little endian because it cannot differentiate between the two, thus
  // the pointer values are converted to little endian before being stored
  // in the variable
  uint64_t val;
  switch (op->kind) {
    case IR_OP_KIND_CONST:
      val = op->constant.val;
      break;
    case IR_OP_KIND_CONST_ADDR:
      val = htole64((uint64_t)op->constant.addr);
      break;
    case IR_OP_KIND_CONST_FN:
      val = htole64((uint64_t)op->constant.func);
      break;
    case IR_OP_KIND_CONST_CFN:
      val = htole64((uint64_t)op->constant.symbol);
      break;
    default:
      assert(false);
  }
  InterpreterSetVarVal(interpreter, op->constant.dst, val);
  InterpreterStep(interpreter);
}

void InterpreterRunOpCall(Interpreter *interpreter, IrOp *op) {
  IrVar **argVars = op->call.args;
  int numArgs = op->call.numArgs;
  IrVar *funcVar = op->call.func;
  Vector *args = interpreter->args;
  VectorClear(args);
  for (int i = 0; i < numArgs; ++i) {
    uint64_t val = InterpreterGetVarVal(interpreter, argVars[i]);
    VectorAdd(args, (void*)val);
  }

  IrVar *retVar = op->call.dst;
  switch (funcVar->type) {
    case IR_TYPE_FN: {
      IrFunc *func =
          (IrFunc*)le64toh(InterpreterGetVarVal(interpreter, funcVar));
      VectorAdd(interpreter->opStack, op);
      interpreter->retVar = retVar;
      interpreter->pc = IrBasicBlockGetFirstOp(IrFuncGetEntryBlock(func));
      InterpreterPushFuncVars(
          interpreter, IrBasicBlockGetParentFunc(IrOpGetParentBasicBlock(op)));
      InterpreterSetupFuncVars(interpreter, func);
      int i = 0;
      IrForParam(func, param) {
        InterpreterSetVarVal(interpreter, param, (uint64_t)args->arr[i++]);
      }
      break;
    } case IR_TYPE_CFN: {
      char *symbol = (char*)le64toh(InterpreterGetVarVal(interpreter, funcVar));
      // We're using the interpreter stored variables to call an actual C
      // function. Since the interpreter stores all variables in little endian,
      // we will have to convert them back to host order before calling
      for (int i = 0; i < numArgs; ++i) {
        args->arr[i] = (void*)le64toh((uint64_t)args->arr[i]);
      }
      InterpreterCFunc func = InterpreterGetCFunc(interpreter, symbol);
      uint64_t retVal = func(args);
      if (retVar) {
        InterpreterSetVarVal(interpreter, retVar, retVal);
      }
      InterpreterStep(interpreter);
      break;
    } default: {
      printf("Var type: %d\n", funcVar->type);
      assert(false);
    }
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

void InterpreterPushFuncVars(Interpreter *interpreter, IrFunc *func) {
  IrForVar(func, var) {
    VectorAdd(interpreter->varStack,
              (void*)InterpreterGetVarVal(interpreter, var));
  }
}

void InterpreterPopFuncVars(Interpreter *interpreter, IrFunc *func) {
  int numVars = 0;
  IrForVar(func, var) {
    ++numVars;
  }

  // Values are pushed onto the stack in the order of the function variables,
  // so when we pop them from the stack, we have to store them in a temporary
  // buffer so that we can assign them in reverse order
  uint64_t *valBuf = alloca(numVars * sizeof(uint64_t));
  int i = 0;
  Vector *stack = interpreter->varStack;
  IrForVar(func, var) {
    valBuf[i++] = (uint64_t)VectorPop(stack);
  }
  i = numVars - 1;
  IrForVar(func, var) {
    InterpreterSetVarVal(interpreter, var, valBuf[i--]);
  }
}

void InterpreterSetupFuncVars(Interpreter *interpreter, IrFunc *func) {
  IrForVar(func, var) {
    InterpreterSetVarVal(interpreter, var, 0);
  }
}
