#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "fir/ir/ir.h"
#include "util/hashtable/hashtable.h"
#include "util/vector/vector.h"

// Forward declarations
typedef struct Interpreter Interpreter;
typedef uint64_t (*InterpreterCFunc)(Vector *args);

// The interpreter main structure
struct Interpreter {
  // The current operation to execute
  IrOp *pc;
  // The return value from the latest function call
  uint64_t ret;
  // A map from symbol (char*) to a wrapper to a C function (InterpreterCFunc)
  HashTable *cFuncs;
  // A map from variables (IrVar*) to their values (uint64_t)
  HashTable *vars;
  // A stack of operations (IrOp*) to jump to whenever we return from a function
  Vector *stack;
  // An argument vector (of uint64_t) used whenever a wrapped C function is
  // called
  Vector *args;
};

// Initialize a new "interpreter" with no registered C functions and variables
void InterpreterInit(Interpreter *interpreter);
// Delete an "interpreter" created with InterpreterInit
void InterpreterDelete(Interpreter *interpreter);
// Run a IR "program" with the provided "interpreter"
void InterpreterRun(Interpreter *interpreter, IrProgram *program);
// Register a wrapped C "func" to the "interpreter" under the symbol "name".
// A wrapped C function has type InterpreterCFunc, which takes in a Vector
// of arguments (of type uint64_t; types that are shorter than 64 bits are
// padded with 0 in the higher bits), and should return a uint64_t value
// (it's okay to return anything if the C function does not have a return value)
void InterpreterRegisterCFunc(
    Interpreter *interpreter, InterpreterCFunc func, const char *name);

#endif // INTERPRETER_H
