#ifndef SEMA_H
#define SEMA_H

#include "util/vector/vector.h"
#include "util/hashtable/hashtable.h"
#include <stdio.h>

// Forward declarations
typedef struct SemaType SemaType;
typedef struct SemaTypeInfo SemaTypeInfo;
typedef struct SemaSymInfo SemaSymInfo;
typedef struct SemaInfo SemaInfo;
typedef struct SemaFileCtx SemaFileCtx;
typedef struct SemaCtx SemaCtx;
typedef struct SyntaxAST SyntaxAST;

// Representing the different kinds of type. See SemaType for more detail
typedef enum {
  SEMA_TYPE_KIND_ARRAY,
  SEMA_TYPE_KIND_FN,
  SEMA_TYPE_KIND_PRIM_TYPE,
  SEMA_TYPE_KIND_CLASS,
  SEMA_TYPE_KIND_NAMESPACE,
  SEMA_TYPE_KIND_PLACEHOLDER
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
  SEMA_PRIM_TYPE_F64,
  SEMA_PRIM_TYPE_F32,
  SEMA_PRIM_TYPE_BOOL,
  SEMA_PRIM_TYPE_ANY,
  SEMA_PRIM_TYPE_VOID,
  SEMA_PRIM_TYPE_NIL
} SemaPrimType;

// Attributes of each symbol
typedef enum {
  SEMA_ATTR_VAR,
  SEMA_ATTR_CONST,
  SEMA_ATTR_STATIC_VAR,
  SEMA_ATTR_STATIC_CONST,
  SEMA_ATTR_CLASS,
  SEMA_ATTR_CTOR,
  SEMA_ATTR_FN,
  SEMA_ATTR_METHOD,
} SemaAttr;

// Different stages of the semantic analysis
typedef enum {
  SEMA_STAGE_SYNTAX,
  SEMA_STAGE_ADD_ALL_FILES,
  SEMA_STAGE_POPULATE_CLASS_SYMBOLS,
  SEMA_STAGE_POPULATE_IMPORT_SYMBOLS,
  SEMA_STAGE_POPULATE_MEMBERS,
  SEMA_STAGE_TYPE_CHECK,
} SemaStage;

struct SemaType {
  // Which kind of type this is. Each type kind stores different information
  // as described below:
  SemaTypeKind kind;
  union {
    // Recursive types: SEMA_TYPE_KIND_ARRAY and SEMA_TYPE_KIND_FN. Recursive
    // types are types constructed from other types. Therefore, recursive
    // pointers to other SemaType are stored
    //
    // For type kind SEMA_TYPE_KIND_ARRAY: records the type of each array
    // element in "baseType", and records the number of array dimensions in
    // "arrayLevels". For example, the type u8[][][] will have "baseType" set
    // to u8 and "arrayLevels" set to 3
    struct {
      SemaType *baseType;
      bool isBaseTypeOwner;
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
    struct {
      // (1) For type kind SEMA_TYPE_KIND_CLASS: a table from the symbol of each
      //     member to their types. For instance, the following class:
      //
      //     class A {
      //       var variable: u64;
      //       fn function() -> i32 {}
      //     }
      //
      //     would have two entries in its memberTable:
      //       - "variable": u64
      //       - "function": fn () -> i32
      //
      // (2) For type kind SEMA_TYPE_KIND_NAMESPACE: a table from the symbol of
      //     each class to the symbol info of the class
      //
      // In either case, the table key is of type char*, and value is of type
      // SemaSymInfo*
      HashTable *memberTable;
      // The AST node that declared this type
      SyntaxAST *node;
    };
  };
};

// Representing the type of an AST node
struct SemaTypeInfo {
  // The type of this AST node
  SemaType *type;
  // Used to record whether or not "type" was allocated by this SemaTypeInfo
  bool isTypeOwner;
};

// Representing the information of a symbol. Each information field is
// described below:
struct SemaSymInfo {
  // The type of this symbol
  SemaTypeInfo typeInfo;

  union {
    // SYNTAX_AST_KIND_VAR_INIT, SYNTAX_AST_KIND_METHOD_DECL
    struct {
      // The attributes of the declared symbol
      SemaAttr attr;
      // The next symbol in the same scope
      SemaSymInfo *nextInScope;
    };
    // SYNTAX_AST_KIND_IMPORT_DECL
    SemaFileCtx *importFileCtx;
  };

  // The entry of this symbol in the symbol table
  HashTableEntry *entry;
  // The AST node that declared this symbol
  SyntaxAST *decl;
};

// All information recorded for semantic analysis. This information is stored
// in each SyntaxAST node
struct SemaInfo {
  union {
    // A pointer to SemaSymInfo is stored for the following kinds of AST nodes:
    // SYNTAX_AST_KIND_CLASS_DECL, SYNTAX_AST_KIND_IMPORT_DECL,
    // SYNTAX_AST_KIND_VAR_INIT, SYNTAX_AST_KIND_METHOD_DECL,
    // SYNTAX_AST_KIND_IDENTIFIER
    // Note that the AST node that declares the symbol will be the owner of
    // "symInfo", so in the case of variable initialization and uses, the
    // initialization will be the owner of "symInfo".
    SemaSymInfo *symInfo;

    // A pointer to SemaTypeInfo is stored for the following kinds of AST nodes:
    // SYNTAX_AST_KIND_LITERAL, SYNTAX_AST_KIND_OP, SYNTAX_AST_KIND_ASSIGN
    SemaTypeInfo typeInfo;
  };

  // The stage of the analysis that the current SyntaxAST has gone through.
  // This ensures that we only delete the resources in SemaInfo if we have
  // already reached the stage of allocating them
  SemaStage stage;
  // Skip semantic analysis for the next analysis pass. Used when the current
  // analysis pass already detects errors for this AST node
  bool skipAnalysis;
  // The file that this AST node belongs to
  SemaFileCtx *fileCtx;
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
  // The type of the current class we are semantic checking
  SemaType *classType;
  // The symbol info of the current method we are semantic checking. If we are
  // not currently in any method, this will be set to NULL
  SemaSymInfo *methodSymInfo;
};

// The context of the current semantic checking. The purpose of each field is
// described below:
struct SemaCtx {
  // All the files we are currently semantic checking
  Vector *fileCtxs;
};

// Initializes all fields in SemaInfo. This is used in SyntaxASTNew to properly
// initialize the SemaInfo struct inside SyntaxAST
void SemaInfoInit(SemaInfo *info);
// Perform semantic check on the file pointed to by "path" and the files that
// are (recursively) imported. Assumes that "ctx" is either uninitialized or was
// deleted by calling SemaCtxDelete, and modifies "ctx" before returning, which
// must be deleted with SemaCtxDelete. Returns true if and only if the semantic
// check returned no errors
bool SemaCheck(SemaCtx *ctx, const char *path);
// Deletes all resources allocated in "ctx" when running SemaCheck
void SemaCtxDelete(SemaCtx *ctx);
// Deletes all resources allocated in the SemaInfo of AST "node"
void SemaDeleteASTSemaInfo(SyntaxAST *node);

#endif // SEMA_H
