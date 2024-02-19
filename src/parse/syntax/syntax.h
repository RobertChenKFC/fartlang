#ifndef SYNTAX_H
#define SYNTAX_H

#include "util/source/source.h"
#include <stdio.h>

// Forward declarations
typedef struct SyntaxAST SyntaxAST;

// Macros for convenient definition of enums and their strings
#define SYNTAX_GEN_ENUM(ENUM) ENUM,
#define SYNTAX_GEN_STR(ENUM) #ENUM,

// An enum of all the kinds of AST nodes, and a string array
// "SYNTAX_AST_KIND_STRS" that stores the string form of each enum for debugging
// purposes
#define SYNTAX_AST_FOREACH_KIND(ENUM) \
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
  ENUM(SYNTAX_AST_KIND_TYPE_LIST)
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
typedef enum {
  SYNTAX_TYPE_I64,
  SYNTAX_TYPE_U64,
  SYNTAX_TYPE_I32,
  SYNTAX_TYPE_U32,
  SYNTAX_TYPE_I16,
  SYNTAX_TYPE_U16,
  SYNTAX_TYPE_I8,
  SYNTAX_TYPE_U8,
  SYNTAX_TYPE_F64,
  SYNTAX_TYPE_F32,
  SYNTAX_TYPE_BOOL,
  SYNTAX_TYPE_ANY,
  SYNTAX_TYPE_VOID,
  SYNTAX_TYPE_IDENTIFIER,
} SyntaxType;

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
    // SYNTAX_AST_KIND_IDENTIFIER, SYNTAX_AST_KIND_CLASS_DECL
    char *string; 
    // SYNTAX_AST_KIND_IMPORT_DECL
    struct {
      char *namespace;
      bool isWildcard;
    } import;
    // SYNTAX_AST_KIND_VAR_DECL
    unsigned varDeclModifiers;
    // SYNTAX_AST_KIND_TYPE
    struct {
      SyntaxType baseType;
      int arrayLevels;
    } type;
  };
};

// Parse a fartlang source "file" with name "filename" into an AST
SyntaxAST *SyntaxParseFile(FILE *file, const char *filename);
// Deletes an entire AST (the "node" and all its successors) created from
// SyntaxParseFile
void SyntaxASTDelete(void *p);

#endif // SYNTAX_H
