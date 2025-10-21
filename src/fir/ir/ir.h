#ifndef IR_H
#define IR_H

#include "util/vector/vector.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

// Enums
enum IrType {
  IR_TYPE_U64,
  IR_TYPE_ADDR = IR_TYPE_U64,
  IR_TYPE_I64,
  IR_TYPE_U32,
  IR_TYPE_I32,
  IR_TYPE_U16,
  IR_TYPE_I16,
  IR_TYPE_U8,
  IR_TYPE_BOOL = IR_TYPE_U8,
  IR_TYPE_I8,
};

enum IrOpKind {
  IR_OP_KIND_ADD,
  IR_OP_KIND_SUB,
  IR_OP_KIND_MUL,
  IR_OP_KIND_DIV,
  IR_OP_KIND_MOD,
  IR_OP_KIND_NEG,
  IR_OP_KIND_AND,
  IR_OP_KIND_OR,
  IR_OP_KIND_XOR,
  IR_OP_KIND_NOT,
  IR_OP_KIND_LSHIFT,
  IR_OP_KIND_RSHIFT,
  IR_OP_KIND_LT,
  IR_OP_KIND_LE,
  IR_OP_KIND_EQ,
  IR_OP_KIND_NE,
  IR_OP_KIND_GE,
  IR_OP_KIND_GT,
  IR_OP_KIND_AS,
  IR_OP_KIND_INTO,
  IR_OP_KIND_CONST,
  IR_OP_KIND_CONST_ADDR,
  IR_OP_KIND_COPY,
  IR_OP_KIND_LOAD,
  IR_OP_KIND_STORE,
  IR_OP_KIND_ALLOC,
  IR_OP_KIND_FREE,
  IR_OP_KIND_CALL,
  IR_OP_KIND_CCALL,
};

// Forward declarations
typedef enum IrType IrType;
typedef enum IrOpKind IrOpKind;
typedef struct IrProgram IrProgram;
typedef struct IrModule IrModule;
typedef struct IrFunc IrFunc;
typedef struct IrVar IrVar;
typedef struct IrModule IrModule;
typedef struct IrBasicBlock IrBasicBlock;
typedef struct IrOp IrOp;

struct IrProgram {
  // An owning pointer to the first module in the program. The modules in a
  // program follow insertion order but does not have any semantic meaning
  IrModule *firstModule;
  // The last module in the program
  IrModule *lastModule;
  // The entry function to call
  IrFunc *entryFunc;
};

struct IrModule {
  // An owning pointer to the first function in the module. The functions in
  // a module follow insertion order but does not have any semantic meaning
  IrFunc *firstFunc;
  // The last function in the module
  IrFunc *lastFunc;
  // An owning pointer to the next module in the program
  IrModule *nextModule;
  // The parent program of this module
  IrProgram *program;
};

struct IrFunc {
  // The parameters of the function. Each entry of the vector is a IrVar*
  Vector *params;
  // An owning pointer to the first basic block of the function. The basic
  // blocks in a function follow insertion order but does not have any semantic
  // meaning
  IrBasicBlock *firstBlock;
  // The last basic block of the function
  IrBasicBlock *lastBlock;
  // The entry and exit basic blocks of a function
  IrBasicBlock *entryBlock, *exitBlock;
  // An owning pointer to the first variable in a function. The variables follow
  // reverse insertion order but does not have any semantic meaning
  IrVar *firstVar;
  // A vector of freed variables. When creating a new variable, they will first
  // be retrieved from here unless it is empty
  Vector *freeVars;
  // An owning pointer to the next function in the module
  IrFunc *nextFunc;
  // The parent module of this function
  IrModule *module;
};

struct IrVar {
  // The type of the variable
  IrType type;
  // An owning pointer to the next variable in the function
  IrVar *nextVar;
};

struct IrBasicBlock {
  // An owning pointer to the first operation in the block. The operations
  // follow insertion order
  IrOp *firstOp;
  // The last operation in the block
  IrOp *lastOp;
  union {
    // The variable to retrieve the condition from to perform a conditional
    // jump. If "cond" is NULL, then the basic block either performs an
    // unconditional jump or is the exit block of a function with no return
    // value
    IrVar *cond;
    // The variable to return from the function. This assumes the block is an
    // exit block of a function with return value
    IrVar *ret;
  };
  // The basic block to jump to when the condition is true (in the case of
  // conditional jumps), or the basic block to jump to unconditionally. If
  // "trueBlock" is NULL, then this is the exit basic block
  IrBasicBlock *trueBlock;
  // The basic block to jump to when the condition is false (in the case of
  // conditional jumps). In all other cases, this should be NULL
  IrBasicBlock *falseBlock;
  // An owning pointer to the next block in the function
  IrBasicBlock *nextBlock;
  // The previous block in the function
  IrBasicBlock *prevBlock;
  // The parent function of this block
  IrFunc *func;
};

struct IrOp {
  // The kind of operation
  IrOpKind kind;
  // An owning point to the next operation in the block
  IrOp *nextOp;
  // The previous operation in the block
  IrOp *prevOp;
  // The parent block of this operation
  IrBasicBlock *block;

  // Separate members for each kind of operation
  union {
    // Binary operation: IR_OP_KIND_ADD, IR_OP_KIND_SUB, IR_OP_KIND_MUL,
    // IR_OP_KIND_DIV, IR_OP_KIND_MOD, IR_OP_KIND_AND, IR_OP_KIND_OR,
    // IR_OP_KIND_XOR, IR_OP_KIND_LSHIFT, IR_OP_KIND_RSHIFT, IR_OP_KIND_LT,
    // IR_OP_KIND_LE, IR_OP_KIND_EQ, IR_OP_KIND_NE, IR_OP_KIND_GE,
    // IR_OP_KIND_GT
    struct {
      // The destination variable
      IrVar *dst;
      // The LHS of the binary operation
      IrVar *src1;
      // The RHS of the binary operation
      IrVar *src2;
    } binary;

    // Unary operation: IR_OP_KIND_NEG, IR_OP_KIND_NOT, IR_OP_KIND_AS,
    // IR_OP_KIND_INTO
    // Copy operation: IR_OP_KIND_COPY
    // Memory allocation operation: IR_OP_KIND_ALLOC
    struct {
      // The destination variable
      IrVar *dst;
      // The source variable
      IrVar *src;
    } unary;

    // Constant operation: IR_OP_KIND_CONST, IR_OP_KIND_CONST_ADDR
    struct {
      // The destination variable
      IrVar *dst;
      union {
        // IR_OP_KIND_CONST: the constant value stored in little-endian
        uint64_t val;
        // IR_OP_KIND_CONST_ADDR
        struct {
          // An owning pointer to the constant value
          uint8_t *addr;
          // The length in bytes of the constant value
          int len;
        };
      };
    } constant;

    // Memory access operation: IR_OP_KIND_LOAD, IR_OP_KIND_STORE
    struct {
      // The address variable
      IrVar *addr;
      // The value variable
      IrVar *val;
    } memAcc;

    // Memory allocation operation: IR_OP_KIND_FREE
    struct {
      // The destination variable
      IrVar *dst;
    } nullary;

    // Function call operation: IR_OP_KIND_CALL, IR_OP_KIND_CCALL
    struct {
      // The destination variable
      IrVar *dst;
      union {
        // The function to call
        IrFunc *func;
        // An owning pointer to the symbol of the C function to call
        char *symbol;
      };
      // An owning array of arguments to the function call
      IrVar **args;
      // The number of arguments to the function call
      int numArgs;
    } call;
  };
};

// Create an empty FIR program "prog". Remember to delete the program using
// IrProgramDelete
IrProgram *IrProgramNew(void);
// Delete an FIR program "prog" and all of its modules
void IrProgramDelete(IrProgram *prog);
// Set the entry function of "prog" to "func"
void IrProgramSetEntryFunc(IrProgram *prog, IrFunc *func);
// Get the entry function of "prog"
IrFunc *IrProgramGetEntryFunc(IrProgram *prog);
// Print the contents of "prog" to "file"
void IrProgramPrint(FILE *file, IrProgram *prog);
// Create a new FIR module, add it to the end of "prog" and return it
IrModule *IrModuleAdd(IrProgram *prog);
// Delete a "module" and all its functions
void IrModuleDelete(IrModule *module);
// Create a new FIR function, add it to the end of "module" and return it
IrFunc *IrFuncAdd(IrModule *module);
// Delete a "func" and all its parameters, variables and basic blocks
void IrFuncDelete(IrFunc *func);
// Create a new FIR basic block, add it to the end of "func" and return it.
// The new basic block will have no condition variable and no true/false
// successor blocks
IrBasicBlock *IrBasicBlockAdd(IrFunc *func);
// Get the first operation in "block"
IrOp *IrBasicBlockGetFirstOp(IrBasicBlock *block);
// Return true if and only if "block" is a conditional block
bool IrBasicBlockIsCond(IrBasicBlock *block);
// If "block" is a conditional basic block, return the variable that stores
// the condition of "block", otherwise return NULL
IrVar *IrBasicBlockGetCond(IrBasicBlock *block);
// Return true if and only if "block" is the exit block of the parent function
bool IrBasicBlockIsExit(IrBasicBlock *block);
// If "block" is an exit basic block, return the variable that stores the
// return value of the block, otherwise return NULL
IrVar *IrBasicBlockGetRet(IrBasicBlock *block);
// Get the next block to jump to from "block" if the condition evaluates to
// true, or the next block to jump to unconditionally
IrBasicBlock *IrBasicBlockGetTrueBlock(IrBasicBlock *block);
// Get the next block to jump to from "block" if the condition evaluates to
// false
IrBasicBlock *IrBasicBlockGetFalseBlock(IrBasicBlock *block);
// Delete a "block" and all its operations
void IrBasicBlockDelete(IrBasicBlock *block);
// Set the entry basic block of "func" to "block"
void IrFuncSetEntryBlock(IrFunc *func, IrBasicBlock *block);
// Get the entry basic block of "func"
IrBasicBlock *IrFuncGetEntryBlock(IrFunc *func);
// Set the exit basic block of "func" to "block"
void IrFuncSetExitBlock(IrFunc *func, IrBasicBlock *block);
// Create a new variable of type "type", add it to "func" and return it
IrVar *IrFuncAddVar(IrFunc *func, IrType type);
// Delete a "var"
void IrVarDelete(IrVar *var);
// Add "op" to the end of "block"
void IrOpAppend(IrBasicBlock *block, IrOp *op);
// Get the next operation of "op" in the block
IrOp *IrOpGetNextOp(IrOp *op);
// Get the basic block that contains "op"
IrBasicBlock *IrOpGetParentBasicBlock(IrOp *op);
// Create a new constant address operation with destination variable "var",
// pointer to the constant data "addr", and length of the constant data "len"
IrOp *IrOpNewConstAddr(IrVar *dst, uint8_t *addr, int len);
// Create a new constant operation with destination variable "var" and constant
// value "val"
IrOp *IrOpNewConst(IrVar *dst, uint64_t val);
// Create a new ccall operation to the C function with symbol "name" (owning
// pointer) and "numArgs" arguments presented as an owning pointer to an array
// of arguments "args"
IrOp *IrOpNewCcall(IrVar *dst, char *name, int numArgs, IrVar **args);
// Delete a "op"
void IrOpDelete(IrOp *op);
// Create a new copy operation from variable "src" to variable "dst"
IrOp *IrOpNewCopy(IrVar *dst, IrVar *src);

// Macros
// General macro for iterating through linked lists
#define IrFor(NodeType, firstNode, nextEntry, node) \
  for (NodeType _iter = firstNode, _next, node; \
       ((node) = _iter) && (_next = _iter->nextEntry, true); \
       _iter = _next)
// Iterates all modules in a IrProgram* "prog", storing the module of each
// iteration in the IrModule* "module"
typedef IrModule *IrModulePtr;
#define IrForModule(prog, module) \
  IrFor(IrModulePtr, prog->firstModule, nextModule, module)
// Iterates all functions in a IrModule* "module", storing the function of each
// iteration in the IrFunc* "func"
typedef IrFunc *IrFuncPtr;
#define IrForFunc(module, func) \
  IrFor(IrFuncPtr, module->firstFunc, nextFunc, func)
// Iterates all blocks in a IrFunc* "func", storing the variable of each
// iteration in the IrBasicBlock* "block"
typedef IrBasicBlock *IrBasicBlockPtr;
#define IrForBasicBlock(func, block) \
  IrFor(IrBasicBlockPtr, func->firstBlock, nextBlock, block)
// Iterates all parameters in a IrFunc* "func", storing the parameter of each
// iteration in the IrVar* "var"
typedef IrVar *IrVarPtr;
#define IrForParam(func, var) \
  for (int _idx = 0; _idx < func->params->size; ++_idx) \
    /* This second for loop iterates exactly once. This is used to enclose
     * both "_idx" and "var" in the scope. */ \
    for (IrVar *var = func->params->arr[_idx], **t = &var; t; t = NULL)
// Iterates all variables in a IrFunc* "func", storing the variable of each
// iteration in the IrVar* "var"
#define IrForVar(func, var) \
  IrFor(IrVarPtr, func->firstVar, nextVar, var)
// Iterates all operations in a IrBasicBlock* "block", storing the variable of
// each iteration in the IrOp* "op"
typedef IrOp *IrOpPtr;
#define IrForOp(func, op) \
  IrFor(IrOpPtr, func->firstOp, nextOp, op)

#endif // IR_H
