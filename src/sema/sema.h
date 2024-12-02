#ifndef SEMA_H
#define SEMA_H

#include "util/vector/vector.h"
#include "util/hashtable/hashtable.h"
#include <stdio.h>

// Forward declarations
typedef struct SemaType SemaType;
typedef struct SemaSymInfo SemaSymInfo;
typedef struct SemaInfo SemaInfo;
typedef struct SemaFileCtx SemaFileCtx;
typedef struct SemaCtx SemaCtx;
typedef struct SyntaxAST SyntaxAST;

// Representing the different kinds of type. See SemaType for more detail
typedef enum {
  SEMA_TYPE_KIND_ARRAY,
  SEMA_TYPE_KIND_FN,
  SEME_TYPE_KIND_PRIM_TYPE,
  SEMA_TYPE_KIND_CLASS,
  SEMA_TYPE_KIND_NAMESPACE
} SemaTypeKind;

// The primitive types
typedef enum {
  SEMA_PRIM_TYPE_U64,
  SEMA_PRIM_TYPE_I64,
  SEMA_PRIM_TYPE_U32,
  SEMA_PRIM_TYPE_I32,
  SEMA_PRIM_TYPE_U16,
  SEMA_PRIM_TYPE_I16,
  SEMA_PRIM_TYPE_U8,
  SEMA_PRIM_TYPE_I8,
  SEMA_PRIM_TYPE_BOOL,
  SEMA_PRIM_TYPE_ANY
} SemaPrimType;

// Attributes of each symbol
typedef enum {
  SEMA_ATTR_VAR,
  SEMA_ATTR_CONST,
  SEMA_ATTR_STATIC_VAR,
  SEMA_ATTR_STATIC_CONST,
  SEMA_ATTR_CTOR,
  SEMA_ATTR_FN,
  SEMA_ATTR_METHOD,
} SemaAttr;

struct SemaType {
  // Which kind of type this is. Each type kind stores different information
  // as described below:
  SemaTypeKind kind;
  union {
    // For type kind SEMA_TYPE_KIND_ARRAY: records the type of each array
    // element in "baseType", and records the number of array dimensions in
    // "arrayLevels". For example, the type u8[][][] will have "baseType" set
    // to u8 and "arrayLevels" set to 3
    struct {
      SemaType *baseType;
      int arrayLevels;
    };
    // For type kind SEMA_TYPE_KIND_FN: the function return type (or NULL, if
    // the function does not have a return type) is stored in "retType", while
    // the function parameter types are stored in the vector "paramTypes". For
    // instance, the type fn (u8[], u64) -> Vector will have "retType" set to
    // Vector and "paramTypes" set to the contents [u8[], u64]
    struct {
      SemaType *retType;
      Vector *paramTypes;
    };
    // For type kind SEMA_TYPE_KIND_PRIM_TYPE: which primitive type this is
    SemaPrimType primType;
    // For type kind SEMA_TYPE_KIND_CLASS: a table from the symbol of each
    // member to their types. For instance, the following class:
    //   class A {
    //     var variable: u64;
    //     fn function() -> i32 {}
    //   }
    // would have two entries in its memberTable:
    //   - "variable": u64
    //   - "function": fn () -> i32
    // The table key is of type char*, and value is of type SemaSymInfo*
    HashTable *memberTable;
  };
};

// Representing the information of a symbol. Each information field is
// described below:
struct SemaSymInfo {
  // The type of this symbol
  SemaType type;
  // The attributes of the declared symbol
  SemaAttr attr;
  // The entry of this symbol in the symbol table
  HashTableEntry *entry;
  // The next symbol in the same scope
  SemaSymInfo *nextInScope;
  // The AST node that declared this symbol
  SyntaxAST *decl;
};

// All information recorded for semantic analysis. This information is stored
// in each SyntaxAST node
struct SemaInfo {
  union {
    // Information if the AST node is a symbol
    SemaSymInfo symInfo;
  };
  // Skip semantic analysis for the next analysis pass. Used when the current
  // analysis pass already detects errors for this AST node
  bool skipAnalysis;
};

#include "parse/syntax/syntax.h"

// The context of a file when performing semantic analysis. The function of
// each field is described below:
struct SemaFileCtx {
  // The path of the source file
  char *path;
  // A file handle of the source file
  FILE *file;
  // The source code of the file used for error printing
  Source *source;
  // The AST of the source file
  SyntaxAST *node;
  // The symbol table of the file. The table reflects the declared symbols
  // depending on where we are semantic checking. For instance, for the
  // following class:
  //   class A {
  //     fn f(x: u64) {
  //       // (1) here
  //     }
  //
  //     fn g() {
  //       // (2) here
  //     }
  //   }
  // the symbol x would be in symbolTable when semantic checking at (1), but
  // not at (2). The table key is of type char*, and value is of type
  // SemaSymInfo*
  HashTable *symbolTable;
  // Stores a stack of one symbol in each scope up to the innermost scope of
  // the current position at which we are semantic checking. If a scope doesn't
  // have any symbols declared, then NULL is pushed into scopes. Otherwise, the
  // latest symbol of type SemaSymInfo* is pushed
  Vector *scopes;
};

// The context of the current semantic checking. The purpose of each field is
// described below:
struct SemaCtx {
  // All the files we are currently semantic checking
  Vector *fileCtxs;
};

// Perform semantic check on the file pointed to by "path" and the files that
// are (recursively) imported. Assumes that "ctx" is either uninitialized or was
// deleted by calling SemaCtxDelete, and modifies "ctx" before returning, which
// must be deleted with SemaCtxDelete. Returns true if and only if the semantic
// check returned no errors
bool SemaCheck(SemaCtx *ctx, const char *path);
// Deletes all resources allocated in "ctx" when running SemaCheck
void SemaCtxDelete(SemaCtx *ctx);

#endif // SEMA_H
