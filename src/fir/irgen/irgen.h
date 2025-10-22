#ifndef IRGEN_H
#define IRGEN_H

#include "fir/ir/ir.h"

// Forward declaration
typedef struct IrgenInfo IrgenInfo;

// Information included in each AST node for IR generation
struct IrgenInfo {
  union {
    // SYNTAX_AST_KIND_METHOD_DECL, SYNTAX_AST_KIND_OP: the corresponding IR
    // function
    IrFunc *func;
    // SYNTAX_AST_KIND_VAR_INIT, SYNTAX_AST_KIND_OP, SYNTAX_AST_KIND_LITERAL:
    // the corresponding IR variable that stores the value of this node
    IrVar *var;
  };
};

// Generate an IR program from the file pointed to by "path" and all the file
// it recursively includes. This function performs a semantic check before
// generating the IR. If semantic check fails, NULL is returned, otherwise the
// generated program is returned
IrProgram *IrgenFromFile(const char *path);
// Initializes all fields in IrgenInfo. This is used in SyntaxASTNew to properly
// initialize the SemaInfo struct inside SyntaxAST
void IrgenInfoInit(IrgenInfo *info);

#endif // IRGEN_H
