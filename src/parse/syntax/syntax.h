#ifndef SYNTAX_H
#define SYNTAX_H

#include "util/source/source.h"
#include <stdio.h>
#include <stdint.h>

// Forward declarations
typedef struct SyntaxAST SyntaxAST;

#include "sema/sema.h"

// Macros for convenient definition of enums and their strings
#define SYNTAX_GEN_ENUM(ENUM) ENUM,
#define SYNTAX_GEN_STR(ENUM) #ENUM,

// An enum of all the kinds of AST nodes, and a string array
// "SYNTAX_AST_KIND_STRS" that stores the string form of each enum for debugging
// purposes
#define SYNTAX_AST_FOREACH_KIND(ENUM) \
  ENUM(SYNTAX_AST_KIND_PLACEHOLDER) \
  ENUM(SYNTAX_AST_KIND_MODULE) \
  ENUM(SYNTAX_AST_KIND_IMPORT_DECLS) \
  ENUM(SYNTAX_AST_KIND_IMPORT_DECL) \
  ENUM(SYNTAX_AST_KIND_MODULE_PATH) \
  ENUM(SYNTAX_AST_KIND_IDENTIFIER) \
  ENUM(SYNTAX_AST_KIND_CLASS_DECLS) \
  ENUM(SYNTAX_AST_KIND_CLASS_DECL) \
  ENUM(SYNTAX_AST_KIND_STMTS) \
  ENUM(SYNTAX_AST_KIND_VAR_DECL) \
  ENUM(SYNTAX_AST_KIND_TYPE) \
  ENUM(SYNTAX_AST_KIND_TYPE_LIST) \
  ENUM(SYNTAX_AST_KIND_VAR_INIT_LIST) \
  ENUM(SYNTAX_AST_KIND_VAR_INIT) \
  ENUM(SYNTAX_AST_KIND_OP) \
  ENUM(SYNTAX_AST_KIND_EXPR_LIST) \
  ENUM(SYNTAX_AST_KIND_LITERAL) \
  ENUM(SYNTAX_AST_KIND_METHOD_DECLS) \
  ENUM(SYNTAX_AST_KIND_MEMBER_ACCESS) \
  ENUM(SYNTAX_AST_KIND_METHOD_DECL) \
  ENUM(SYNTAX_AST_KIND_PARAM_LIST) \
  ENUM(SYNTAX_AST_KIND_PARAM) \
  ENUM(SYNTAX_AST_KIND_EXPR_STMT) \
  ENUM(SYNTAX_AST_KIND_ASSIGN_STMT) \
  ENUM(SYNTAX_AST_KIND_ASSIGN) \
  ENUM(SYNTAX_AST_KIND_IF_STMT) \
  ENUM(SYNTAX_AST_KIND_SWITCH_STMT) \
  ENUM(SYNTAX_AST_KIND_CASE) \
  ENUM(SYNTAX_AST_KIND_LABEL) \
  ENUM(SYNTAX_AST_KIND_FOR_STMT) \
  ENUM(SYNTAX_AST_KIND_WHILE_STMT) \
  ENUM(SYNTAX_AST_KIND_BREAK_STMT) \
  ENUM(SYNTAX_AST_KIND_RETURN_STMT)
typedef enum {
  SYNTAX_AST_FOREACH_KIND(SYNTAX_GEN_ENUM)
} SyntaxASTKind;
extern const char *SYNTAX_AST_KIND_STRS[];

// Variable declaration modifiers
enum {
  SYNTAX_VAR_DECL_STATIC = 1,
  SYNTAX_VAR_DECL_CONST  = 2,
};

// Types
#define SYNTAX_FOREACH_TYPE(ENUM) \
  ENUM(SYNTAX_TYPE_I64) \
  ENUM(SYNTAX_TYPE_U64) \
  ENUM(SYNTAX_TYPE_I32) \
  ENUM(SYNTAX_TYPE_U32) \
  ENUM(SYNTAX_TYPE_I16) \
  ENUM(SYNTAX_TYPE_U16) \
  ENUM(SYNTAX_TYPE_I8) \
  ENUM(SYNTAX_TYPE_U8) \
  ENUM(SYNTAX_TYPE_F64) \
  ENUM(SYNTAX_TYPE_F32) \
  ENUM(SYNTAX_TYPE_BOOL) \
  ENUM(SYNTAX_TYPE_ANY) \
  ENUM(SYNTAX_TYPE_THIS) \
  ENUM(SYNTAX_TYPE_NULL) \
  ENUM(SYNTAX_TYPE_STR) \
  ENUM(SYNTAX_TYPE_VOID) \
  ENUM(SYNTAX_TYPE_MODULE_PATH) \
  ENUM(SYNTAX_TYPE_FUNC) \
  ENUM(SYNTAX_TYPE_VAR)
typedef enum {
  SYNTAX_FOREACH_TYPE(SYNTAX_GEN_ENUM)
} SyntaxType;
extern const char *SYNTAX_TYPE_STRS[];

// Operators
#define SYNTAX_FOREACH_OP(ENUM) \
  ENUM(SYNTAX_OP_ALLOC) \
  ENUM(SYNTAX_OP_TERNARY) \
  ENUM(SYNTAX_OP_LOGIC_OR) \
  ENUM(SYNTAX_OP_LOGIC_AND) \
  ENUM(SYNTAX_OP_BIT_OR) \
  ENUM(SYNTAX_OP_BIT_XOR) \
  ENUM(SYNTAX_OP_BIT_AND) \
  ENUM(SYNTAX_OP_LT) \
  ENUM(SYNTAX_OP_LE) \
  ENUM(SYNTAX_OP_EQEQ) \
  ENUM(SYNTAX_OP_NEQ) \
  ENUM(SYNTAX_OP_GT) \
  ENUM(SYNTAX_OP_GE) \
  ENUM(SYNTAX_OP_LSHIFT) \
  ENUM(SYNTAX_OP_RSHIFT) \
  ENUM(SYNTAX_OP_ADD) \
  ENUM(SYNTAX_OP_SUB) \
  ENUM(SYNTAX_OP_MUL) \
  ENUM(SYNTAX_OP_DIV) \
  ENUM(SYNTAX_OP_MOD) \
  ENUM(SYNTAX_OP_NEG) \
  ENUM(SYNTAX_OP_NOT) \
  ENUM(SYNTAX_OP_BIT_NOT) \
  ENUM(SYNTAX_OP_CAST_IS) \
  ENUM(SYNTAX_OP_CAST_AS) \
  ENUM(SYNTAX_OP_CAST_INTO) \
  ENUM(SYNTAX_OP_CALL) \
  ENUM(SYNTAX_OP_ARRAY_ACCESS) \
  ENUM(SYNTAX_OP_ARRAY_TYPE) \
  ENUM(SYNTAX_OP_INC) \
  ENUM(SYNTAX_OP_DEC) \
  ENUM(SYNTAX_OP_EQ) \
  ENUM(SYNTAX_OP_ADD_EQ) \
  ENUM(SYNTAX_OP_SUB_EQ) \
  ENUM(SYNTAX_OP_MUL_EQ) \
  ENUM(SYNTAX_OP_DIV_EQ) \
  ENUM(SYNTAX_OP_MOD_EQ) \
  ENUM(SYNTAX_OP_LSHIFT_EQ) \
  ENUM(SYNTAX_OP_RSHIFT_EQ) \
  ENUM(SYNTAX_OP_BIT_AND_EQ) \
  ENUM(SYNTAX_OP_BIT_XOR_EQ) \
  ENUM(SYNTAX_OP_BIT_OR_EQ)
typedef enum {
  SYNTAX_FOREACH_OP(SYNTAX_GEN_ENUM)
} SyntaxOp;
extern const char *SYNTAX_OP_STRS[];

// Method declaration modifiers
#define SYNTAX_FOREACH_METHOD_TYPE(ENUM) \
  ENUM(SYNTAX_METHOD_TYPE_FN) \
  ENUM(SYNTAX_METHOD_TYPE_METHOD)
typedef enum {
  SYNTAX_FOREACH_METHOD_TYPE(SYNTAX_GEN_ENUM)
} SyntaxMethodType;
extern const char *SYNTAX_METHOD_TYPE_STRS[];

// A structure to store the AST of a fartlang program. This structs stores
// any node in the AST. A node has pointers "firstChild" and "lastChild" that
// points to the first and last children of the node, respectively, and a
// pointer "sibling" that points to the next node that shares the same parent as
// the current node. A node also stores a "kind", which is a label of what kind
// of AST node it is, and depending on the kind, there may be additional data
// stored in the union. Finally, the node stores a "loc", which is a source
// location pointing to the part of code that parsed into this AST node

struct SyntaxAST {
  SyntaxAST *firstChild;
  SyntaxAST *lastChild;
  SyntaxAST *sibling;
  int kind;
  SourceLocation loc;
  // TODO: fill this in later
  union {
    // SYNTAX_AST_KIND_IDENTIFIER, SYNTAX_AST_KIND_CLASS_DECL,
    // SYNTAX_AST_KIND_VAR_INIT, SYNTAX_AST_KIND_MEMBER_ACCESS,
    // SYNTAX_AST_KIND_PARAM, SYNTAX_AST_KIND_LABEL,
    struct {
      char *string; 
      SourceLocation stringLoc;
    };
    // SYNTAX_AST_KIND_IMPORT_DECL
    struct {
      char *namespace;
      bool isWildcard;
      SourceLocation extLoc;
    } import;
    // SYNTAX_AST_KIND_VAR_DECL
    unsigned varDeclModifiers;
    // SYNTAX_AST_KIND_TYPE
    struct {
      SyntaxType baseType;
      int arrayLevels;
    } type;
    // SYNTAX_AST_KIND_OP, SYNTAX_AST_KIND_ASSIGN
    SyntaxOp op;
    // SYNTAX_AST_KIND_LITERAL
    struct {
      SyntaxType type;
      union {
        uint64_t intVal;
        double floatVal;
        bool boolVal;
        char *strVal;
      };
    } literal;
    // SYNTAX_AST_KIND_METHOD_DECL
    struct {
      SyntaxMethodType type;
      char *name;
      SourceLocation nameLoc;
    } method;
  };

  // Information for semantic analysis
  SemaInfo semaInfo;
};

// Create the lexer and parser and save them to files so that later parsing
// would have the files already created. Note that SyntaxParseFile does the same
// thing, so this is only useful if you want to create the lexer and parser
// ahead of time
void SyntaxSetupParser(void);
// Parse a fartlang source "file" with name "filename" into an AST
SyntaxAST *SyntaxParseFile(FILE *file, const char *filename);
// Deletes an entire AST (the "node" and all its successors) created from
// SyntaxParseFile
void SyntaxASTDelete(void *p);

#endif // SYNTAX_H
