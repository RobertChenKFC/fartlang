#include "sema.h"
#include "parse/parser/parser.h"
#include "util/vector/vector.h"
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdarg.h>

// Helper functions
// Initializes all fields of SemaCtx "ctx"
void SemaNew(SemaCtx *ctx);
// Add all files that are recursively included by the file that "path" points to
// to "ctx", and populating their AST. Returns true if and only if all files are
// added and parsed successfully
bool SemaAddAllFiles(SemaCtx *ctx, const char *path);
// Creates a new SemaFileCtx that contains "path", the file that "path" points
// to, the AST of the file, and all other fields initialized, and returns
// the SemaFileCtx*. If any part of the process fails, then returns NULL.
// Can provide an extra "importDecl", which is the AST node of the import
// declartion of this file, and the "importFilePath", which is the path that
// points to the file that contains this import declaration. Otherwise, provide
// NULL to both arguments if this is the root file (not imported)
SemaFileCtx *SemaFileCtxNew(
    const char *path, SyntaxAST *importDecl, const char *importFilePath);
// Destructor for SemaFileCtx "p" created by SemaFileCtxNew. Can be used as
// hash table destructors
void SemaFileCtxDelete(void *p);
// Populates the symbol tables of each SemaFileCtx in "ctx" with the classes
// declared in the file. Returns true if and only if the class names declared
// in each file are unique
bool SemaPopulateClassSymbols(SemaCtx *ctx);
// Destructor for SemaSymInfo "p". Can be used as hash table destructors
// TODO: this may not be needed. Remove it if possible
void SemaSymInfoDelete(void *p);
// Destructor for SemaType "type"
void SemaTypeDelete(SemaType *type);
// Initializes all the SemaInfo stored in the AST "node" and recursively
// initializes all descendants using the "init" function. The remaining
// arguments to the function are passed into the "init" function as a va_list
// every time it is called
typedef void (*SemaASTInitFn)(SemaInfo*, va_list);
void SemaASTInit(SyntaxAST *node, SemaASTInitFn init, ...);
// Same as SemaASTInit, but takes in a va_list for the remaining arguments
void SemaASTInitV(SyntaxAST *node, SemaASTInitFn init, va_list args);
// Populates the symbol tables of each SemaFileCtx in "ctx" with the imported
// namespace and class symbols. Returns true if and only if the imported symbols
// do not collide with existing symbols
bool SemaPopulateImportSymbols(SemaCtx *ctx);
// Populates the member tables of each class in the symbol table of every
// SemaFileCtx in "ctx". Returns true if and only if the declared variables and
// methods do not collide with existing symbols
bool SemaPopulateMembers(SemaCtx *ctx);
// Given the AST "syntaxType" of the type in a parent AST node "parentAST",
// converts "syntaxType" to a SemaType. If all identifiers present in
// "syntaxType" can be looked up successfully in "symbolTable", the converted
// SemaType is returned, otherwise an error message is printed using "fileCtx",
// and NULL is returned. If a new SemaType is allocated, "isTypeOwner" will be
// set to true, otherwise it is set to false
SemaType *SemaTypeFromSyntaxType(
    SyntaxAST *syntaxType, HashTable *symbolTable, SemaFileCtx *fileCtx,
    SyntaxAST *parentAST, bool *isTypeOwner);
// Given the AST "methodDecl" of a method declaration, returns a SemaType
// corresponding to the function type of this method if all the return type
// and parameter type lookups for the method are successful, otherwise prints
// an error message using "fileCtx" and NULL is returned
SemaType *SemaTypeFromMethodDecl(
    SyntaxAST *methodDecl, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Construct a SemaType "type" from the base primitive type "primType"
void SemaTypeFromSemaPrimType(SemaType *type, SemaPrimType primType);
// Type checks all each variable declaration and statement in "ctx". Returns
// true if and only if every statement correctly type checks
bool SemaTypeCheck(SemaCtx *ctx);
// Given the AST "term" of a term, returns the SemaType of this
// expression. If the expression successfully type checks, then the
// corresponding SemaType is returned. Otherwise, and error message is printed
// using "fileCtx" and NULL is returned
SemaType *SemaTypeFromTerm(
    SyntaxAST *term, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Same as SemaTypeFromTerm, but for AST "expr" of expressions instead
SemaType *SemaTypeFromExpr(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Check if the type expressed by "syntaxType" matches any of the primitive
// types. If so, update "type" to reflect the expressed type and return true.
// Otherwise, return false
bool SemaCheckAllPrimTypes(SyntaxType syntaxType, SemaType *type);
// Deletes the type held in "typeInfo" if "typeInfo" is the owner of the type
void SemaTypeInfoDelete(SemaTypeInfo *typeInfo);
// Prints the "type" as a string to "file"
void SemaTypePrint(FILE *file, SemaType *type);
// Checks if "type1" and "type2" are exactly the same
bool SemaTypeEqual(SemaType *type1, SemaType *type2);
// Initializes "info" after SemaAddAllFiles. This init function takes in
// an additional argument "SemaFileCtx *fileCtx". In particular, this init
// function sets the stage of "info" to "SEMA_STAGE_ADD_ALL_FILES" and the
// fileCtx of "info" to "fileCtx"
void SemaASTInitAddAllFiles(SemaInfo *info, va_list arg);
// Initializes "info" after SemaPopulateClassSymbols. This init function takes
// no additional arguments. In particular, this init function sets the stage of
// "info" to "SEMA_STAGE_POPULATE_CLASS_SYMBOLS"
void SemaASTInitPopulateClassSymbols(SemaInfo *info, va_list arg);
// Initializes "info" after SemaPopulateImportSymbols. This init function takes
// no additional arguments. In particular, this init function sets the stage of
// "info" to "SEMA_STAGE_POPULATE_IMPORT_SYMBOLS"
void SemaASTInitPopulateImportSymbols(SemaInfo *info, va_list arg);
// Initializes "info" after SemaPopulateMembers. This init function takes
// no additional arguments. In particular, this init function sets the stage of
// "info" to "SEMA_STAGE_POPULATE_MEMBERS"
void SemaASTInitPopulateMembers(SemaInfo *info, va_list arg);
// Initializes "info" after SemaTypeCheck. This init function takes
// no additional arguments. In particular, this init function sets the stage of
// "info" to "SEMA_STAGE_TYPE_CHECK"
void SemaASTInitTypeCheck(SemaInfo *info, va_list arg);
// Checks if the an instance of the "right" type can be assigned to an instance
// of the "left" type. This returns true if and only if (1) the "left" type is
// "any" or (2) the "left" and "right" types are the same
bool SemaTypeIsAssignable(SemaType *left, SemaType *right);
// Checks if "type" is a float type
bool SemaTypeIsFloat(SemaType *type);
// Checks if "type" is a signed type
bool SemaTypeIsSigned(SemaType *type);
// Checks if "type" is an unsigned type
bool SemaTypeIsUnsigned(SemaType *type);
// Returns the bitwidth of "type"
int SemaTypeBitwidth(SemaType *type);
// Checks if all the operands of the bitwise expression expressed
// in the AST node "expr" are all the signed or all unsigned types, and all have
// the same bitwidth. If so, return the result type, otherwise return NULL.
// The "symbolTable" and "ctx" are used to recursively type check the operands
SemaType *SemaTypeCheckBitwiseOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if the condition of ternary "expr" is of type bool, and the true and
// false branches of the expression evaluate to the same type. If so, return
// the type of the true branch, otherwise, return NULL. Remaining arguments are
// used in the same way as above
SemaType *SemaTypeCheckTernaryOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if all operands of the logic "expr" is of type bool. If so, return
// the type of the last operand, otherwise return NULL. Remaining arguments
// are used in the same way as above
SemaType *SemaTypeCheckLogicOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if all operands are float types, all are signed types or all are
// unsigned types. If so, return the type with the largest bitwidth, otherwise
// return NULL. Remaining arguments are used in the same way as above
SemaType *SemaTypeCheckComparisonOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if "type" is a primitive type of kind "primType"
bool SemaTypeIsPrimType(SemaType *type, SemaPrimType primType);
// Checks if "expr" is a comparison expression
bool SemaIsComparisonExpr(SyntaxAST *expr);

void SemaInfoInit(SemaInfo *info) {
  info->stage = SEMA_STAGE_SYNTAX;
  info->skipAnalysis = false;
}

void SemaNew(SemaCtx *ctx) {
  ctx->fileCtxs = VectorNew();
}

SemaFileCtx *SemaFileCtxNew(
    const char *path, SyntaxAST *importDecl, const char *importFilePath) {
  FILE *file = fopen(path, "r");
  if (!file) {
    if (importDecl) {
      SyntaxAST *modulePath = importDecl->firstChild;
      assert(modulePath);
      SourceLocation *loc = &modulePath->loc;
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
              importFilePath, loc->from.lineNo + 1);
      fprintf(stderr, "imported file does not exist\n");
      FILE *importFile = fopen(importFilePath, "r");
      Source *source = SourceFromFile(importFile);
      SourceLocationPrint(source, 1, SOURCE_COLOR_RED, loc);
      SourceDelete(source);
      fclose(importFile);
    } else {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET);
      fprintf(stderr, " compilation file "SOURCE_COLOR_RED"%s"
              SOURCE_COLOR_RESET" does not exist\n", path);
    }
    return NULL;
  }

  SyntaxAST *node = SyntaxParseFile(file, path);
  if (node == PARSER_OBJECT_FAILURE) {
    fclose(file);
    return NULL;
  }

  SemaFileCtx *fileCtx = malloc(sizeof(SemaFileCtx));
  fileCtx->path = strdup(path);
  fileCtx->file = file;
  fileCtx->source = SourceFromFile(file);
  fileCtx->node = node;
  fileCtx->symbolTable = HashTableNew(
      ParserStringHash, ParserStringEqual, NULL, NULL);
  SemaASTInit(node, SemaASTInitAddAllFiles, fileCtx);
  return fileCtx;
}

void SemaFileCtxDelete(void *p) {
  SemaFileCtx *fileCtx = p;
  free(fileCtx->path);
  fclose(fileCtx->file);
  SourceDelete(fileCtx->source);
  SyntaxASTDelete(fileCtx->node);
  HashTableDelete(fileCtx->symbolTable);
  free(fileCtx);
}

bool SemaAddAllFiles(SemaCtx *ctx, const char *path) {
  // Initialize the context
  SemaNew(ctx);

  // Add the root file
  SemaFileCtx *fileCtx = SemaFileCtxNew(path, NULL, NULL);
  if (!fileCtx)
    return false;
  Vector *fileCtxs = ctx->fileCtxs;
  VectorAdd(fileCtxs, fileCtx);

  bool success = true;
  // Record the set of imported file paths so that we do not import the same
  // file twice
  HashTable *importedFiles = HashTableNew(
      ParserStringHash, ParserStringEqual, NULL, NULL);
  HashTableEntryAdd(importedFiles, (void*)path, fileCtx);
  // A heap-allocated buffer with expandable capacity to store the path of the
  // imported file
  int capacity = 16;
  char *importPath = malloc(capacity);
  // Process until no more new imports
  for (int idx = 0; idx < fileCtxs->size; ++idx) {
    fileCtx = fileCtxs->arr[idx];
    SyntaxAST *node = fileCtx->node;
    SyntaxAST *importDecls = node->firstChild;
    assert(importDecls);

    // Iterate through all import declarations
    for (SyntaxAST *importDecl = importDecls->firstChild; importDecl;
         importDecl = importDecl->sibling) {
      // Convert module path to real import path (a string)
      // TODO: provide functionality to import files from separate directories
      SyntaxAST *modulePath = importDecl->firstChild;
      assert(modulePath);

      importPath[0] = '\0';
      int length = 0;
      for (SyntaxAST *identifier = modulePath->firstChild; identifier;
           identifier = identifier->sibling) {
        int identifierLength = strlen(identifier->string);
        // The 7 comes from the "/", ".fart" and "\0"
        if (length + identifierLength + 7 >= capacity) {
          capacity *= 2;
          importPath = realloc(importPath, capacity);
        }
        if (length != 0)
          importPath[length++] = '/';
        strcpy(importPath + length, identifier->string);
        length += identifierLength;
      }
      strcpy(importPath + length, ".fart");

      // Check if the file has already been added
      HashTableEntry *entry = HashTableEntryRetrieve(importedFiles, importPath);
      SemaSymInfo *symInfo = malloc(sizeof(SemaSymInfo));
      importDecl->semaInfo.symInfo = symInfo;
      if (entry) {
        symInfo->importFileCtx = entry->value;
        continue;
      }
      // Parse the file
      SemaFileCtx *newFileCtx = SemaFileCtxNew(
          importPath, importDecl, fileCtx->path);
      if (!newFileCtx) {
        success = false;
        goto CLEANUP;
      }
      // Add the file to the hash table and stack
      HashTableEntryAdd(importedFiles, importPath, newFileCtx);
      VectorAdd(fileCtxs, newFileCtx);
      // Attach the file context of the imported file to the import declaration
      symInfo->importFileCtx = newFileCtx;
    }
  }

CLEANUP:
  // Cleanup
  HashTableDelete(importedFiles);
  free(importPath);

  return success;
}

bool SemaCheck(SemaCtx *ctx, const char *path) {
  if (!SemaAddAllFiles(ctx, path)) {
    return false;
  }
  if (!SemaPopulateClassSymbols(ctx)) {
    return false;
  }
  if (!SemaPopulateImportSymbols(ctx)) {
    return false;
  }
  if (!SemaPopulateMembers(ctx)) {
    return false;
  }
  if (!SemaTypeCheck(ctx)) {
    return false;
  }
  return true;
}

void SemaCtxDelete(SemaCtx *ctx) {
  Vector *fileCtxs = ctx->fileCtxs;
  int n = fileCtxs->size;
  for (int i = 0; i < n; ++i)
    SemaFileCtxDelete(fileCtxs->arr[i]);
  VectorDelete(fileCtxs);
}

bool SemaPopulateClassSymbols(SemaCtx *ctx) {
  Vector *fileCtxs = ctx->fileCtxs;
  int n = fileCtxs->size;
  bool success = true;
  for (int i = 0; i < n; ++i) {
    SemaFileCtx *fileCtx = fileCtxs->arr[i];
    HashTable *symbolTable = fileCtx->symbolTable;

    SyntaxAST *module = fileCtx->node;
    assert(module && module->kind == SYNTAX_AST_KIND_MODULE);
    SyntaxAST *classDecls = module->lastChild;
    assert(classDecls && classDecls->kind == SYNTAX_AST_KIND_CLASS_DECLS);
    for (SyntaxAST *classDecl = classDecls->firstChild; classDecl;
         classDecl = classDecl->sibling) {
      assert(classDecl->kind == SYNTAX_AST_KIND_CLASS_DECL);
      char *className = classDecl->string;

      HashTableEntry *entry = HashTableEntryRetrieve(symbolTable, className);
      if (entry) {
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
                fileCtx->path, classDecl->loc.from.lineNo + 1);
        fprintf(stderr, "redeclaration of class "SOURCE_COLOR_RED"%s"
                SOURCE_COLOR_RESET" in the current module\n",
                className);
        SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                            &classDecl->stringLoc);
        classDecl->semaInfo.skipAnalysis = true;
        success = false;
      } else {
        SemaSymInfo *classInfo = malloc(sizeof(SemaSymInfo));
        classDecl->semaInfo.symInfo = classInfo;
        SemaType *type = malloc(sizeof(SemaType));
        type->kind = SEMA_TYPE_KIND_CLASS;
        type->memberTable = HashTableNew(
            ParserStringHash, ParserStringEqual, NULL, NULL);
        type->node = classDecl;
        SemaTypeInfo *classTypeInfo = &classInfo->typeInfo;
        classTypeInfo->type = type;
        classTypeInfo->isTypeOwner = true;
        HashTableEntryAdd(symbolTable, className, classInfo);
        entry = HashTableEntryRetrieve(symbolTable, className);
        assert(entry);
        classInfo->entry = entry;
        classInfo->decl = classDecl;
      }
    }
    SemaASTInit(module, SemaASTInitPopulateClassSymbols);
  }
  return success;
}

void SemaSymInfoDelete(void *p) {
  SemaSymInfo *symInfo = p;
  SemaTypeInfoDelete(&symInfo->typeInfo);
  free(symInfo);
}

void SemaTypeDelete(SemaType *type) {
  switch (type->kind) {
    case SEMA_TYPE_KIND_ARRAY:
      if (type->isBaseTypeOwner) {
        SemaTypeDelete(type->baseType);
      }
      break;
    case SEMA_TYPE_KIND_FN:
      if (type->isRetTypeOwner) {
        SemaTypeDelete(type->retType);
      }
      for (int i = 0; i < type->paramTypes->size; ++i) {
        if (type->isParamTypeOwner->arr[i]) {
          SemaTypeDelete(type->paramTypes->arr[i]);
        }
      }
      VectorDelete(type->paramTypes);
      VectorDelete(type->isParamTypeOwner);
      break;
    case SEMA_TYPE_KIND_CLASS:
      HashTableDelete(type->memberTable);
      break;
    default:
      break;
  }
  free(type);
}

void SemaASTInit(SyntaxAST *node, SemaASTInitFn init, ...) {
  va_list args;
  va_start(args, init);
  SemaASTInitV(node, init, args);
  va_end(args);
}

void SemaASTInitV(SyntaxAST *node, SemaASTInitFn init, va_list args) {
  SemaInfo *info = &node->semaInfo;
  init(info, args);
  for (SyntaxAST *cur = node->firstChild; cur; cur = cur->sibling) {
    SemaASTInitV(cur, init, args);
  }
}

bool SemaPopulateImportSymbols(SemaCtx *ctx) {
  Vector *fileCtxs = ctx->fileCtxs;
  int n = fileCtxs->size;
  bool success = true;
  for (int i = 0; i < n; ++i) {
    SemaFileCtx *fileCtx = fileCtxs->arr[i];
    SyntaxAST *module = fileCtx->node;
    assert(module && module->kind == SYNTAX_AST_KIND_MODULE);
    SyntaxAST *importDecls = module->firstChild;
    assert(importDecls && importDecls->kind == SYNTAX_AST_KIND_IMPORT_DECLS);
    HashTable *symbolTable = fileCtx->symbolTable;
    for (SyntaxAST *importDecl = importDecls->firstChild; importDecl;
         importDecl = importDecl->sibling) {
      assert(importDecl->kind == SYNTAX_AST_KIND_IMPORT_DECL);
      if (importDecl->import.isWildcard) {
        SemaFileCtx *importFileCtx =
            importDecl->semaInfo.symInfo->importFileCtx;
        HashTable *importSymbolTable = importFileCtx->symbolTable;
        for (HashTableEntry *importEntry = importSymbolTable->head; importEntry;
             importEntry = importEntry->nextInTable) {
          char *symbol = importEntry->key;
          SemaSymInfo *symInfo = importEntry->value;
          SemaTypeInfo *typeInfo = &symInfo->typeInfo;
          if (typeInfo->type->kind != SEMA_TYPE_KIND_CLASS ||
              symInfo->decl->semaInfo.fileCtx != importFileCtx) {
            // Only class symbols are imported. There are no transitive imports
            continue;
          }

          HashTableEntry *entry = HashTableEntryRetrieve(symbolTable, symbol);
          if (entry) {
            fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                    " %s:%d: ", fileCtx->path, importDecl->loc.from.lineNo + 1);
            // TODO: the red in the error message doesn't really match the red
            // in the source location print. See if this should be changed
            fprintf(stderr, "wildcard import brings in used identifier "
                    SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET"\n", symbol);
            SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                                &importDecl->loc);
            importDecl->semaInfo.skipAnalysis = true;
            success = false;
            break;
          }
          HashTableEntryAdd(symbolTable, symbol, importEntry->value);
        }
      } else {
        char *namespace = importDecl->import.namespace;
        SourceLocation namespaceLoc;
        if (!namespace) {
          // If the import declaration does not specify a namespace and is not
          // a wildcard import, then the default namespace is the last
          // identifier in the module path
          SyntaxAST *modulePath = importDecl->firstChild;
          assert(modulePath && modulePath->kind == SYNTAX_AST_KIND_MODULE_PATH);
          SyntaxAST *identifier = modulePath->lastChild;
          assert(identifier && identifier->kind == SYNTAX_AST_KIND_IDENTIFIER);
          namespace = identifier->string;
          namespaceLoc = identifier->loc;
        } else {
          namespaceLoc = importDecl->import.extLoc;
        }
        HashTableEntry *entry = HashTableEntryRetrieve(symbolTable, namespace);
        if (entry) {
          fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
                  fileCtx->path, importDecl->loc.from.lineNo + 1);
          fprintf(stderr, "reuse of identifier "SOURCE_COLOR_RED"%s"
                  SOURCE_COLOR_RESET" for import namespace\n", namespace);
          SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                              &namespaceLoc);
          importDecl->semaInfo.skipAnalysis = true;
          success = false;
          continue;
        }
        SemaInfo *info = &importDecl->semaInfo;
        SemaSymInfo *symInfo = info->symInfo;
        SemaFileCtx *importFileCtx = symInfo->importFileCtx;
        SemaType *type = malloc(sizeof(SemaType));
        type->kind = SEMA_TYPE_KIND_NAMESPACE;
        type->memberTable = importFileCtx->symbolTable;
        type->node = importDecl;
        SemaTypeInfo *typeInfo = &symInfo->typeInfo;
        typeInfo->type = type;
        typeInfo->isTypeOwner = true;
        symInfo->decl = importDecl;
        HashTableEntryAdd(symbolTable, namespace, symInfo);
        symInfo->entry = HashTableEntryRetrieve(symbolTable, namespace);
      }
    }
    SemaASTInit(module, SemaASTInitPopulateImportSymbols);
  }
  return success;
}

bool SemaPopulateMembers(SemaCtx *ctx) {
  Vector *fileCtxs = ctx->fileCtxs;
  int n = fileCtxs->size;
  bool success = true;
  for (int i = 0; i < n; ++i) {
    SemaFileCtx *fileCtx = fileCtxs->arr[i];
    SyntaxAST *module = fileCtx->node;
    assert(module && module->kind == SYNTAX_AST_KIND_MODULE);
    SyntaxAST *classDecls = module->lastChild;
    assert(classDecls && classDecls->kind == SYNTAX_AST_KIND_CLASS_DECLS);
    HashTable *symbolTable = fileCtx->symbolTable;
    for (SyntaxAST *classDecl = classDecls->firstChild; classDecl;
         classDecl = classDecl->sibling) {
      assert(classDecl && classDecl->kind == SYNTAX_AST_KIND_CLASS_DECL);
      char *classIdentifier = classDecl->string;
      HashTableEntry *classEntry = HashTableEntryRetrieve(
          symbolTable, classIdentifier);
      assert(classEntry);
      SemaSymInfo *classInfo = classEntry->value;
      SemaTypeInfo *classTypeInfo = &classInfo->typeInfo;
      HashTable *memberTable = classTypeInfo->type->memberTable;
      
      // Populate all the variable declarations
      SyntaxAST *varDecls = classDecl->firstChild;
      assert(varDecls && varDecls->kind == SYNTAX_AST_KIND_STMTS);
      for (SyntaxAST *varDecl = varDecls->firstChild; varDecl;
           varDecl = varDecl->sibling) {
        assert(varDecl && varDecl->kind == SYNTAX_AST_KIND_VAR_DECL);
        SyntaxAST *varInitList = varDecl->firstChild;
        assert(varInitList &&
               varInitList->kind == SYNTAX_AST_KIND_VAR_INIT_LIST);
        for (SyntaxAST *varInit = varInitList->firstChild; varInit;
             varInit = varInit->sibling) {
          assert(varInit && varInit->kind == SYNTAX_AST_KIND_VAR_INIT);

          // Check if variable identifier is unique
          char *varIdentifier = varInit->string;
          HashTableEntry *entry = HashTableEntryRetrieve(
              memberTable, varIdentifier);
          if (entry) {
            fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                    " %s:%d: ", fileCtx->path, varInit->loc.from.lineNo + 1);
            fprintf(stderr, "reuse of identifier "SOURCE_COLOR_RED"%s"
                    SOURCE_COLOR_RESET" for variable declaration\n",
                    varIdentifier);
            SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                                &varInit->stringLoc);
            success = false;
            varInit->semaInfo.skipAnalysis = true;
            continue;
          }

          // Check if variable type is valid
          SyntaxAST *varType = varInit->firstChild;
          assert(varType && varType->kind == SYNTAX_AST_KIND_TYPE);
          SemaSymInfo *varInfo = malloc(sizeof(SemaSymInfo));
          varInit->semaInfo.symInfo = varInfo;
          SemaTypeInfo *varTypeInfo = &varInfo->typeInfo;
          SemaType *semaType = SemaTypeFromSyntaxType(
              varType, symbolTable, fileCtx, varInit,
              &varTypeInfo->isTypeOwner);
          if (!semaType) {
            success = false;
            varInit->semaInfo.skipAnalysis = true;
            continue;
          }
          varTypeInfo->type = semaType;
          HashTableEntryAdd(memberTable, varIdentifier, varInfo);
        }
      }

      // Populate all method declarations
      SyntaxAST *methodDecls = classDecl->lastChild;
      assert(methodDecls && methodDecls->kind == SYNTAX_AST_KIND_METHOD_DECLS);
      for (SyntaxAST *methodDecl = methodDecls->firstChild; methodDecl;
           methodDecl = methodDecl->sibling) {
        assert(methodDecl->kind == SYNTAX_AST_KIND_METHOD_DECL);

        // Check if the method name is unique
        char *methodIdentifier = methodDecl->method.name;
        HashTableEntry *entry = HashTableEntryRetrieve(
            memberTable, methodIdentifier);
        if (entry) {
          fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                  " %s:%d: ", fileCtx->path, methodDecl->loc.from.lineNo + 1);
          fprintf(stderr, "reuse of identifier "SOURCE_COLOR_RED"%s"
                  SOURCE_COLOR_RESET" for method declaration\n",
                  methodIdentifier);
          SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                              &methodDecl->method.nameLoc);
          success = false;
          methodDecl->semaInfo.skipAnalysis = true;
          continue;
        }

        // Check if method return type and parameter types are valid
        SemaType *methodType = SemaTypeFromMethodDecl(
            methodDecl, symbolTable, fileCtx);
        if (!methodType) {
          success = false;
          methodDecl->semaInfo.skipAnalysis = true;
          continue;
        }

        SemaSymInfo *methodInfo = malloc(sizeof(SemaSymInfo));
        methodDecl->semaInfo.symInfo = methodInfo;
        SemaTypeInfo *methodTypeInfo = &methodInfo->typeInfo;
        methodTypeInfo->type = methodType;
        methodTypeInfo->isTypeOwner = true;
        HashTableEntryAdd(memberTable, methodIdentifier, methodInfo);
      }
    }
    SemaASTInit(module, SemaASTInitPopulateMembers);
  }
  return success;
}

SemaType *SemaTypeFromSyntaxType(
    SyntaxAST *syntaxType, HashTable *symbolTable, SemaFileCtx *fileCtx,
    SyntaxAST *parentAST, bool *isTypeOwner) {
  SemaType *type = malloc(sizeof(SemaType));
  *isTypeOwner = true;

  int arrayLevels = syntaxType->type.arrayLevels;
  if (arrayLevels > 0) {
    // Temporarily set the array levels to 0 to convert the element type of
    // the array, then set the array level back to the original
    syntaxType->type.arrayLevels = 0;
    SemaType *baseType = SemaTypeFromSyntaxType(
        syntaxType, symbolTable, fileCtx, parentAST, &type->isBaseTypeOwner);
    syntaxType->type.arrayLevels = arrayLevels;
    if (!baseType) {
      free(type);
      return NULL;
    }
    type->kind = SEMA_TYPE_KIND_ARRAY;
    type->baseType = baseType;
    type->arrayLevels = arrayLevels;
    return type;
  }

  if (SemaCheckAllPrimTypes(syntaxType->type.baseType, type)) {
    return type;
  }
  switch (syntaxType->type.baseType) {
    case SYNTAX_TYPE_VAR:
      assert(syntaxType->type.arrayLevels == 0);
      type->kind = SEMA_TYPE_KIND_PLACEHOLDER;
      SyntaxAST *initExpr = syntaxType->sibling;
      if (!initExpr) {
        assert(parentAST->kind == SYNTAX_AST_KIND_VAR_INIT);
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                " %s:%d: ", fileCtx->path, parentAST->loc.from.lineNo + 1);
        fprintf(stderr, "no initialization for variable "SOURCE_COLOR_RED"%s"
                SOURCE_COLOR_RESET" provided when using type deduction\n",
                parentAST->string);
        SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                            &parentAST->stringLoc);
        free(type);
        return NULL;
      }
      break;
    case SYNTAX_TYPE_MODULE_PATH: {
      SyntaxAST *modulePath = syntaxType->firstChild;
      assert(modulePath && modulePath->kind == SYNTAX_AST_KIND_MODULE_PATH);
      SyntaxAST *identifier1 = modulePath->firstChild;
      assert(identifier1 && identifier1->kind == SYNTAX_AST_KIND_IDENTIFIER);
      SyntaxAST *identifier2 = identifier1->sibling;
      HashTable *memberTable;
      SemaSymInfo *namespaceInfo = NULL;
      // For types, module paths can only be of two forms:
      if (identifier2) {
        // 1st form: a.B, where a is a namespace and B is a class
        if (identifier2->sibling) {
          fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                  " %s:%d: ", fileCtx->path, syntaxType->loc.from.lineNo + 1);
          fprintf(stderr, "invalid type expression\n");
          SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                              &modulePath->loc);
          parentAST->semaInfo.skipAnalysis = true;
          free(type);
          return NULL;
        }
        // Check if the 1st identifier is a valid namespace
        HashTableEntry *entry = HashTableEntryRetrieve(
            symbolTable, identifier1->string);
        // TODO: refactor the error message
        if (!entry) {
          fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                  " %s:%d: ", fileCtx->path, syntaxType->loc.from.lineNo + 1);
          fprintf(stderr, "reference to unknown identifier "SOURCE_COLOR_RED
                  "%s"SOURCE_COLOR_RESET"\n", identifier1->string);
          SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                              &identifier1->stringLoc);
          parentAST->semaInfo.skipAnalysis = true;
          free(type);
          return NULL;
        }
        namespaceInfo = entry->value;
        SemaTypeInfo *namespaceTypeInfo = &namespaceInfo->typeInfo;
        if (namespaceTypeInfo->type->kind != SEMA_TYPE_KIND_NAMESPACE) {
          fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                  " %s:%d: ", fileCtx->path, syntaxType->loc.from.lineNo + 1);
          fprintf(stderr, SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET" is not a "
                  "namespace\n", identifier1->string);
          SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                              &identifier1->stringLoc);
          parentAST->semaInfo.skipAnalysis = true;
          free(type);
          return NULL;
        }
        memberTable = namespaceTypeInfo->type->memberTable;
      } else {
        // 2nd form: A, where A is a class
        identifier2 = identifier1;
        memberTable = symbolTable;
      }
      // Check if the 2nd identifier is a valid class
      HashTableEntry *entry = HashTableEntryRetrieve(
          memberTable, identifier2->string);
      bool isValidClass;
      if (entry) {
        SemaSymInfo *classInfo = entry->value;
        SemaTypeInfo *classTypeInfo = &classInfo->typeInfo;
        // If we are importing from a namespace, make sure not to include
        // transitive imports
        if (namespaceInfo) {
          isValidClass = classInfo->decl->semaInfo.fileCtx ==
              namespaceInfo->decl->semaInfo.symInfo->importFileCtx;
        } else {
          isValidClass = true;
        }
        if (classTypeInfo->type->kind != SEMA_TYPE_KIND_CLASS) {
          isValidClass = false;
        }
      } else {
        isValidClass = false;
      }
      if (!isValidClass) {
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                " %s:%d: ", fileCtx->path, syntaxType->loc.from.lineNo + 1);
        fprintf(stderr, "identifier "SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET
                " is not a class ", identifier2->string);
        if (namespaceInfo) {
          fprintf(stderr, "in "SOURCE_COLOR_GREEN"%s"SOURCE_COLOR_RESET"\n",
                  identifier1->string);
          SourceLocationPrint(fileCtx->source, 2,
                              SOURCE_COLOR_GREEN, &identifier1->stringLoc,
                              SOURCE_COLOR_RED, &identifier2->stringLoc);
        } else {
          fprintf(stderr, "\n");
          SourceLocationPrint(fileCtx->source, 1,
                              SOURCE_COLOR_RED, &identifier2->stringLoc);
        }
        parentAST->semaInfo.skipAnalysis = true;
        free(type);
        return NULL;
      } 
      SemaSymInfo *classInfo = entry->value;
      SemaTypeInfo *classTypeInfo = &classInfo->typeInfo;
      SemaType *modulePathType = classTypeInfo->type;
      if (syntaxType->type.arrayLevels > 0) {
        type->kind = SEMA_TYPE_KIND_ARRAY;
        type->baseType = modulePathType;
        type->arrayLevels = syntaxType->type.arrayLevels;
      } else {
        free(type);
        type = modulePathType;
        *isTypeOwner = false;
      }
      break;
    }
    case SYNTAX_TYPE_FUNC: {
      SyntaxAST *paramSyntaxTypes = syntaxType->firstChild;
      assert(paramSyntaxTypes &&
             paramSyntaxTypes->kind == SYNTAX_AST_KIND_TYPE_LIST);
      SyntaxAST *retSyntaxType = syntaxType->lastChild;
      assert(retSyntaxType && retSyntaxType->kind == SYNTAX_AST_KIND_TYPE);

      SemaType *retType = SemaTypeFromSyntaxType(
          retSyntaxType, symbolTable, fileCtx, parentAST, &type->isRetTypeOwner);
      if (!retType) {
        free(type);
        return NULL;
      }

      Vector *paramTypes = VectorNew();
      Vector *isParamTypeOwner = VectorNew();
      for (SyntaxAST *paramSyntaxType = paramSyntaxTypes->firstChild;
           paramSyntaxType; paramSyntaxType = paramSyntaxType->sibling) {
        assert(paramSyntaxType &&
               paramSyntaxType->kind == SYNTAX_AST_KIND_TYPE);
        bool isCurTypeOwner;
        SemaType *paramType = SemaTypeFromSyntaxType(
            paramSyntaxType, symbolTable, fileCtx, parentAST, &isCurTypeOwner);
        if (!paramType) {
          free(type);
          SemaTypeDelete(retType);
          for (int i = 0; i < paramTypes->size; ++i) {
            if (isParamTypeOwner->arr[i]) {
              SemaTypeDelete(paramTypes->arr[i]);
            }
          }
          VectorDelete(paramTypes);
          VectorDelete(isParamTypeOwner);
        }
        VectorAdd(paramTypes, paramType);
        VectorAdd(isParamTypeOwner, (void*)(int64_t)isCurTypeOwner);
      }

      type->kind = SEMA_TYPE_KIND_FN;
      type->retType = retType;
      type->paramTypes = paramTypes;
      type->isParamTypeOwner = isParamTypeOwner;
      break;
    } default: {
      // A syntax type that is not handled by the semantic checker
      fprintf(stderr, "Unhandled syntax type %d\n", syntaxType->type.baseType);
      assert(false);
    }
  }
  return type;
}

SemaType *SemaTypeFromMethodDecl(
    SyntaxAST *methodDecl, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SemaType *methodType = malloc(sizeof(SemaType));
  methodType->kind = SEMA_TYPE_KIND_FN;

  SyntaxAST *retSyntaxType = methodDecl->firstChild;
  assert(retSyntaxType && retSyntaxType->kind == SYNTAX_AST_KIND_TYPE);
  SemaType *retType = SemaTypeFromSyntaxType(
      retSyntaxType, symbolTable, fileCtx, methodDecl,
      &methodType->isRetTypeOwner);
  if (!retType) {
    goto METHOD_TYPE_CLEANUP;
  }
  methodType->retType = retType;

  Vector *paramTypes = VectorNew();
  Vector *isParamTypeOwner = VectorNew();
  methodType->paramTypes = paramTypes;
  methodType->isParamTypeOwner = isParamTypeOwner;
  SyntaxAST *paramList = retSyntaxType->sibling;
  assert(paramList && paramList->kind == SYNTAX_AST_KIND_PARAM_LIST);
  for (SyntaxAST *param = paramList->firstChild; param;
       param = param->sibling) {
    assert(param->kind == SYNTAX_AST_KIND_PARAM);
    bool isOwner;
    SyntaxAST *paramSyntaxType = param->firstChild;
    assert(paramSyntaxType && paramSyntaxType->kind == SYNTAX_AST_KIND_TYPE);
    SemaType *paramType = SemaTypeFromSyntaxType(
        paramSyntaxType, symbolTable, fileCtx, methodDecl, &isOwner);
    if (!paramType) {
      goto VECTOR_CLEANUP;
    }
    VectorAdd(paramTypes, paramType);
    VectorAdd(isParamTypeOwner, (void*)(int64_t)isOwner);
  }
  return methodType;

VECTOR_CLEANUP:
  VectorDelete(paramTypes);
  VectorDelete(isParamTypeOwner);
  SemaTypeDelete(retType);
METHOD_TYPE_CLEANUP:
  free(methodType);
  return NULL;
}

void SemaDeleteASTSemaInfo(SyntaxAST *node) {
  bool deleteSymInfo = true;
  SemaInfo *info = &node->semaInfo;
  SemaStage stage = info->stage;
  switch (node->kind) {
    case SYNTAX_AST_KIND_IMPORT_DECL:
      deleteSymInfo =
          !node->import.isWildcard &&
          stage >= SEMA_STAGE_POPULATE_IMPORT_SYMBOLS;
      goto DELETE_SYM_INFO;
    case SYNTAX_AST_KIND_CLASS_DECL:
      deleteSymInfo = stage >= SEMA_STAGE_POPULATE_CLASS_SYMBOLS;
      goto DELETE_SYM_INFO;
    case SYNTAX_AST_KIND_VAR_INIT:
      // TODO: handle var init for member variables and inside methods
      // separately
    case SYNTAX_AST_KIND_METHOD_DECL:
      deleteSymInfo = stage >= SEMA_STAGE_POPULATE_MEMBERS;
DELETE_SYM_INFO:
      if (deleteSymInfo && !info->skipAnalysis) {
        SemaSymInfo *symInfo = info->symInfo;
        SemaTypeInfo *typeInfo = &symInfo->typeInfo;
        SemaTypeInfoDelete(typeInfo);
        free(symInfo);
      }
      break;
    case SYNTAX_AST_KIND_OP:
    case SYNTAX_AST_KIND_LITERAL:
      if (stage >= SEMA_STAGE_TYPE_CHECK && !info->skipAnalysis) {
        SemaTypeInfoDelete(&info->typeInfo);
      }
      break;
  }
}

void SemaTypeFromSemaPrimType(SemaType *type, SemaPrimType primType) {
  type->kind = SEMA_TYPE_KIND_PRIM_TYPE;
  type->primType = primType;
}

bool SemaTypeCheck(SemaCtx *ctx) {
  bool success = true;
  Vector *fileCtxs = ctx->fileCtxs;
  int n = fileCtxs->size;
  for (int i = 0; i < n; ++i) {
    SemaFileCtx *fileCtx = fileCtxs->arr[i];
    SyntaxAST *module = fileCtx->node;
    assert(module && module->kind == SYNTAX_AST_KIND_MODULE);
    SyntaxAST *classDecls = module->lastChild;
    assert(classDecls && classDecls->kind == SYNTAX_AST_KIND_CLASS_DECLS);
    HashTable *symbolTable = fileCtx->symbolTable;
    for (SyntaxAST *classDecl = classDecls->firstChild; classDecl;
         classDecl = classDecl->sibling) {
      assert(classDecl && classDecl->kind == SYNTAX_AST_KIND_CLASS_DECL);
      char *classIdentifier = classDecl->string;
      HashTableEntry *classEntry = HashTableEntryRetrieve(
          symbolTable, classIdentifier);
      assert(classEntry);
      SemaSymInfo *classInfo = classEntry->value;
      SemaTypeInfo *classTypeInfo = &classInfo->typeInfo;
      HashTable *memberTable = classTypeInfo->type->memberTable;
      
      // Type check all class variable initializations
      SyntaxAST *varDecls = classDecl->firstChild;
      assert(varDecls && varDecls->kind == SYNTAX_AST_KIND_STMTS);
      for (SyntaxAST *varDecl = varDecls->firstChild; varDecl;
           varDecl = varDecl->sibling) {
        assert(varDecl && varDecl->kind == SYNTAX_AST_KIND_VAR_DECL);
        SyntaxAST *varInitList = varDecl->firstChild;
        assert(varInitList &&
               varInitList->kind == SYNTAX_AST_KIND_VAR_INIT_LIST);
        for (SyntaxAST *varInit = varInitList->firstChild; varInit;
             varInit = varInit->sibling) {
          assert(varInit && varInit->kind == SYNTAX_AST_KIND_VAR_INIT);
          SyntaxAST *initExpr = varInit->lastChild;
          SemaSymInfo *varInfo = varInit->semaInfo.symInfo;
          SemaTypeInfo *varTypeInfo = &varInfo->typeInfo;
          if (initExpr->kind != SYNTAX_AST_KIND_TYPE) {
            SemaType *initExprType = SemaTypeFromExpr(
                initExpr, symbolTable, fileCtx);
            if (!initExprType) {
              success = false;
              continue;
            }
            if (varTypeInfo->type->kind == SEMA_TYPE_KIND_PLACEHOLDER) {
              // This is an auto-deducted type, so replace the placeholder type
              // with the newly generated type
              if (varTypeInfo->isTypeOwner) {
                SemaTypeDelete(varTypeInfo->type);
              }
              varTypeInfo->type = initExprType;
              varTypeInfo->isTypeOwner = false;
            } else {
              // This variable is declared with a type, so must check if the
              // types match
              if (!SemaTypeIsAssignable(varTypeInfo->type, initExprType)) {
                SyntaxAST *varType = varInit->firstChild;
                SourceLocation *varTypeLoc = &varType->loc;
                SourceLocation *initExprLoc = &initExpr->loc;
                fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                        " %s:%d: ", fileCtx->path,
                        initExprLoc->from.lineNo + 1);
                fprintf(stderr, "expected expression to be of type "
                        SOURCE_COLOR_GREEN);
                SemaTypePrint(stderr, varTypeInfo->type);
                fprintf(stderr, SOURCE_COLOR_RESET", got type "
                        SOURCE_COLOR_RED);
                SemaTypePrint(stderr, initExprType);
                fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
                SourceLocationPrint(
                    fileCtx->source, 2, SOURCE_COLOR_GREEN, varTypeLoc,
                    SOURCE_COLOR_RED, initExprLoc);
                success = false;
              }
            }
          } else {
            assert(varTypeInfo->type->kind != SEMA_TYPE_KIND_PLACEHOLDER);
          }
        }
      }
    }
    SemaASTInit(module, SemaASTInitTypeCheck);
  }
  return success;
}

SemaType *SemaTypeFromTerm(
    SyntaxAST *term, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SemaType *type = malloc(sizeof(SemaType));
  switch (term->kind) {
    case SYNTAX_AST_KIND_LITERAL: {
      // TODO: handle other kinds of literals
      switch (term->literal.type) {
        case SYNTAX_TYPE_U64:
        case SYNTAX_TYPE_I64:
        case SYNTAX_TYPE_U32:
        case SYNTAX_TYPE_I32:
        case SYNTAX_TYPE_U16:
        case SYNTAX_TYPE_I16:
        case SYNTAX_TYPE_U8:
        case SYNTAX_TYPE_I8:
        case SYNTAX_TYPE_F64:
        case SYNTAX_TYPE_F32:
        case SYNTAX_TYPE_BOOL:
        case SYNTAX_TYPE_ANY:
          assert(SemaCheckAllPrimTypes(term->literal.type, type));
          break;
        case SYNTAX_TYPE_STR: {
          SemaType *baseType = malloc(sizeof(SemaType));
          baseType->kind = SEMA_TYPE_KIND_PRIM_TYPE;
          baseType->primType = SEMA_PRIM_TYPE_U8;
          type->kind = SEMA_TYPE_KIND_ARRAY;
          type->baseType = baseType;
          type->isBaseTypeOwner = true;
          type->arrayLevels = 1;
          break;
        } default: {
          break;
        }
      }
      SemaTypeInfo *typeInfo = &term->semaInfo.typeInfo;
      typeInfo->type = type;
      typeInfo->isTypeOwner = true;
      break;
    } case SYNTAX_AST_KIND_IDENTIFIER: {
      char *varName = term->string;
      HashTableEntry *entry = HashTableEntryRetrieve(symbolTable, varName);
      if (!entry) {
        SourceLocation *loc = &term->stringLoc;
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
        fprintf(stderr, "identifier "SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET
                " is not defined\n", varName);
        SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
        free(type);
        type = NULL;
        term->semaInfo.skipAnalysis = true;
        break;
      }
      SemaSymInfo *varInfo = entry->value;
      term->semaInfo.symInfo = varInfo;
      break;
    }

    // TODO: continue here. Add more cases for different terms: identifier,
    // allocations

    default: {
      // DEBUG
      printf("Term kind: %d\n", term->kind);

      assert(false);
    }
  }
  return type;
}

SemaType *SemaTypeFromExpr(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  if (expr->kind != SYNTAX_AST_KIND_OP) {
    return SemaTypeFromTerm(expr, symbolTable, fileCtx);
  }
  switch (expr->op) {
    case SYNTAX_OP_TERNARY:
      return SemaTypeCheckTernaryOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_LOGIC_OR:
    case SYNTAX_OP_LOGIC_AND:
      return SemaTypeCheckLogicOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_BIT_OR:
    case SYNTAX_OP_BIT_AND:
    case SYNTAX_OP_BIT_XOR:
    case SYNTAX_OP_BIT_NOT:
      return SemaTypeCheckBitwiseOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_LT:
    case SYNTAX_OP_LE:
    case SYNTAX_OP_EQEQ:
    case SYNTAX_OP_NEQ:
    case SYNTAX_OP_GT:
    case SYNTAX_OP_GE:
      return SemaTypeCheckComparisonOp(expr, symbolTable, fileCtx);
    default:
      assert(false);
  }
}

bool SemaCheckAllPrimTypes(SyntaxType syntaxType, SemaType *type) {
  switch (syntaxType) {
    case SYNTAX_TYPE_I64:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_I64);
      return true;
    case SYNTAX_TYPE_U64:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_U64);
      return true;
    case SYNTAX_TYPE_I32:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_I32);
      return true;
    case SYNTAX_TYPE_U32:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_U32);
      return true;
    case SYNTAX_TYPE_I16:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_I16);
      return true;
    case SYNTAX_TYPE_U16:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_U16);
      return true;
    case SYNTAX_TYPE_I8:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_I8);
      return true;
    case SYNTAX_TYPE_U8:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_U8);
      return true;
    case SYNTAX_TYPE_F64:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_F64);
      return true;
    case SYNTAX_TYPE_F32:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_F32);
      return true;
    case SYNTAX_TYPE_BOOL:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_BOOL);
      return true;
    case SYNTAX_TYPE_ANY:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_ANY);
      return true;
    case SYNTAX_TYPE_VOID:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_VOID);
      return true;
    default:
      return false;
  }
}

void SemaTypeInfoDelete(SemaTypeInfo *typeInfo) {
  if (typeInfo->isTypeOwner) {
    SemaTypeDelete(typeInfo->type);
  }
}

void SemaTypePrint(FILE *file, SemaType *type) {
  switch (type->kind) {
    case SEMA_TYPE_KIND_ARRAY: {
      SemaTypePrint(file, type->baseType);
      for (int i = 0; i < type->arrayLevels; ++i) {
        fprintf(file, "[]");
      }
      break;
    } case SEMA_TYPE_KIND_FN: {
      fprintf(file, "(");
      Vector *paramTypes = type->paramTypes;
      int n = paramTypes->size;
      for (int i = 0; i < n; ++i) {
        if (i != 0) {
          fprintf(file, ", ");
        }
        SemaTypePrint(file, paramTypes->arr[i]);
      }
      fprintf(file, ") -> ");
      SemaTypePrint(file, type->retType);
      break;
    } case SEMA_TYPE_KIND_PRIM_TYPE: {
      switch (type->primType) {
        case SEMA_PRIM_TYPE_U64:
          fprintf(file, "u64");
          break;
        case SEMA_PRIM_TYPE_I64:
          fprintf(file, "i64");
          break;
        case SEMA_PRIM_TYPE_U32:
          fprintf(file, "u32");
          break;
        case SEMA_PRIM_TYPE_I32:
          fprintf(file, "i32");
          break;
        case SEMA_PRIM_TYPE_U16:
          fprintf(file, "u16");
          break;
        case SEMA_PRIM_TYPE_I16:
          fprintf(file, "i16");
          break;
        case SEMA_PRIM_TYPE_U8:
          fprintf(file, "u8");
          break;
        case SEMA_PRIM_TYPE_I8:
          fprintf(file, "i8");
          break;
        case SEMA_PRIM_TYPE_F64:
          fprintf(file, "f64");
          break;
        case SEMA_PRIM_TYPE_F32:
          fprintf(file, "f32");
          break;
        case SEMA_PRIM_TYPE_BOOL:
          fprintf(file, "bool");
          break;
        case SEMA_PRIM_TYPE_ANY:
          fprintf(file, "any");
          break;
        case SEMA_PRIM_TYPE_VOID:
          fprintf(file, "void");
          break;
      }
      break;
    } case SEMA_TYPE_KIND_CLASS: {
      fprintf(file, "%s", type->node->string);
    } case SEMA_TYPE_KIND_NAMESPACE: {
      char *namespace = type->node->import.namespace;
      assert(namespace);
      fprintf(file, "%s", namespace);
    } default: {
      // DEBUG
      printf("Sema type kind: %d\n", type->kind);

      assert(false);
    }
  }
}

bool SemaTypeEqual(SemaType *type1, SemaType *type2) {
  if (type1->kind != type2->kind) {
    return false;
  }
  switch (type1->kind) {
    case SEMA_TYPE_KIND_ARRAY:
      return type1->arrayLevels == type2->arrayLevels &&
             SemaTypeEqual(type1->baseType, type2->baseType);
    case SEMA_TYPE_KIND_FN: {
      Vector *paramTypes1 = type1->paramTypes;
      Vector *paramTypes2 = type2->paramTypes;
      if (paramTypes1->size != paramTypes2->size) {
        return false;
      }
      for (int i = 0; i < paramTypes1->size; ++i) {
        if (!SemaTypeEqual(paramTypes1->arr[i], paramTypes2->arr[i])) {
          return false;
        }
      }
      return SemaTypeEqual(type1->retType, type2->retType);
    }
    case SEMA_TYPE_KIND_PRIM_TYPE:
      return type1->primType == type2->primType;
    case SEMA_TYPE_KIND_CLASS:
    case SEMA_TYPE_KIND_NAMESPACE:
      return type1->node == type2->node;
  }
}

void SemaASTInitAddAllFiles(SemaInfo *info, va_list arg) {
  SemaFileCtx *fileCtx = va_arg(arg, SemaFileCtx*);
  info->stage = SEMA_STAGE_ADD_ALL_FILES;
  info->fileCtx = fileCtx;
}

void SemaASTInitPopulateClassSymbols(SemaInfo *info, va_list arg) {
  info->stage = SEMA_STAGE_POPULATE_CLASS_SYMBOLS;
}

void SemaASTInitPopulateImportSymbols(SemaInfo *info, va_list arg) {
  info->stage = SEMA_STAGE_POPULATE_IMPORT_SYMBOLS;
}

void SemaASTInitPopulateMembers(SemaInfo *info, va_list arg) {
  info->stage = SEMA_STAGE_POPULATE_MEMBERS;
}

void SemaASTInitTypeCheck(SemaInfo *info, va_list arg) {
  info->stage = SEMA_STAGE_TYPE_CHECK;
}

bool SemaTypeIsAssignable(SemaType *left, SemaType *right) {
  if (left->kind == SEMA_TYPE_KIND_PRIM_TYPE &&
      left->primType == SEMA_PRIM_TYPE_ANY) {
    return true;
  }
  return SemaTypeEqual(left, right);
}

bool SemaTypeIsFloat(SemaType *type) {
  if (type->kind != SEMA_TYPE_KIND_PRIM_TYPE) {
    return false;
  }
  switch (type->primType) {
    case SEMA_PRIM_TYPE_F64:
    case SEMA_PRIM_TYPE_F32:
      return true;
    default:
      return false;
  }
}

bool SemaTypeIsSigned(SemaType *type) {
  if (type->kind != SEMA_TYPE_KIND_PRIM_TYPE) {
    return false;
  }
  switch (type->primType) {
    case SEMA_PRIM_TYPE_I64:
    case SEMA_PRIM_TYPE_I32:
    case SEMA_PRIM_TYPE_I16:
    case SEMA_PRIM_TYPE_I8:
      return true;
    default:
      return false;
  }
}

bool SemaTypeIsUnsigned(SemaType *type) {
  if (type->kind != SEMA_TYPE_KIND_PRIM_TYPE) {
    return false;
  }
  switch (type->primType) {
    case SEMA_PRIM_TYPE_U64:
    case SEMA_PRIM_TYPE_U32:
    case SEMA_PRIM_TYPE_U16:
    case SEMA_PRIM_TYPE_U8:
      return true;
    default:
      return false;
  }
}

int SemaTypeBitwidth(SemaType *type) {
  if (type->kind != SEMA_TYPE_KIND_PRIM_TYPE) {
    return 64;
  }
  switch (type->primType) {
    case SEMA_PRIM_TYPE_F64:
    case SEMA_PRIM_TYPE_U64:
    case SEMA_PRIM_TYPE_I64:
    case SEMA_PRIM_TYPE_ANY:
      return 64;
    case SEMA_PRIM_TYPE_F32:
    case SEMA_PRIM_TYPE_U32:
    case SEMA_PRIM_TYPE_I32:
      return 32;
    case SEMA_PRIM_TYPE_U16:
    case SEMA_PRIM_TYPE_I16:
      return 16;
    case SEMA_PRIM_TYPE_U8:
    case SEMA_PRIM_TYPE_I8:
    case SEMA_PRIM_TYPE_BOOL:
      return 8;
    default:
      assert(false);
  }
}

SemaType *SemaTypeCheckBitwiseOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *firstOperand = expr->firstChild, *operand = firstOperand;
  SemaType *firstOperandType = SemaTypeFromExpr(
      firstOperand, symbolTable, fileCtx);
  if (!firstOperandType) {
    goto CLEANUP;
  }
  if (!SemaTypeIsSigned(firstOperandType) &&
      !SemaTypeIsUnsigned(firstOperandType)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, firstOperand->loc.from.lineNo + 1);
    fprintf(stderr, "expected signed or unsigned type for bitwise operand, "
            "got "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, firstOperandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &firstOperand->loc);
    operand = operand->sibling;
    goto CLEANUP;
  }
  for (operand = firstOperand->sibling; operand; operand = operand->sibling) {
    SemaType *operandType = SemaTypeFromExpr(operand, symbolTable, fileCtx);
    if (!operandType) {
      goto CLEANUP;
    }
    if (!SemaTypeEqual(firstOperandType, operandType)) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, firstOperand->loc.from.lineNo + 1);
      fprintf(stderr, "bitwise operand of type "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, operandType);
      fprintf(stderr, SOURCE_COLOR_RESET" does not match first operand of type "
              SOURCE_COLOR_GREEN);
      SemaTypePrint(stderr, firstOperandType);
      fprintf(stderr, SOURCE_COLOR_RESET"\n");
      SourceLocationPrint(
          fileCtx->source, 2, SOURCE_COLOR_GREEN, &firstOperand->loc,
          SOURCE_COLOR_RED, &operand->loc);
      operand = operand->sibling;
      goto CLEANUP;
    }
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = firstOperandType;
  typeInfo->isTypeOwner = false;
  return firstOperandType;
CLEANUP:
  for (; operand; operand = operand->sibling) {
    operand->semaInfo.skipAnalysis = true;
  }
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

SemaType *SemaTypeCheckTernaryOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *trueNode = expr->firstChild;
  SyntaxAST *condNode = trueNode->sibling;
  SyntaxAST *falseNode = condNode->sibling;
  SemaType *condType = SemaTypeFromExpr(
      condNode, symbolTable, fileCtx);
  if (!condType) {
    goto TERNARY_OP_SKIP_COND;
  }
  SemaType *trueType = SemaTypeFromExpr(trueNode, symbolTable, fileCtx);
  if (!trueType) {
    goto TERNARY_OP_SKIP_TRUE;
  }
  SemaType *falseType = SemaTypeFromExpr(falseNode, symbolTable, fileCtx);
  if (!trueType) {
    goto TERNARY_OP_SKIP_FALSE;
  }
  if (!SemaTypeIsPrimType(condType, SEMA_PRIM_TYPE_BOOL)) {
    SourceLocation *loc = &condNode->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, "expected condition to be of type bool, got type "
            SOURCE_COLOR_RED);
    SemaTypePrint(stderr, condType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
    goto TERNARY_OP_SKIP_EXPR;
  }
  if (!SemaTypeEqual(trueType, falseType)) {
    SourceLocation *trueLoc = &trueNode->loc;
    SourceLocation *falseLoc = &falseNode->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, expr->loc.from.lineNo + 1);
    fprintf(stderr, "mismatch types "SOURCE_COLOR_BLUE);
    SemaTypePrint(stderr, trueType);
    fprintf(stderr, SOURCE_COLOR_RESET" and "SOURCE_COLOR_YELLOW);
    SemaTypePrint(stderr, falseType);
    fprintf(stderr, SOURCE_COLOR_RESET" for ternary operator\n");
    // TODO: improve type printing by also sourcing the location of where
    // each type is declared if the types happen to have the same name
    SourceLocationPrint(
        fileCtx->source, 2, SOURCE_COLOR_BLUE, &trueNode->loc,
        SOURCE_COLOR_YELLOW, &falseNode->loc);
    goto TERNARY_OP_SKIP_EXPR;
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = trueType;
  typeInfo->isTypeOwner = false;
  return trueType;
TERNARY_OP_SKIP_COND:
  condNode->semaInfo.skipAnalysis = true;
TERNARY_OP_SKIP_TRUE:
  trueNode->semaInfo.skipAnalysis = true;
TERNARY_OP_SKIP_FALSE:
  falseNode->semaInfo.skipAnalysis = true;
TERNARY_OP_SKIP_EXPR:
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

SemaType *SemaTypeCheckLogicOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *operand;
  SemaType *operandType;
  for (operand = expr->firstChild; operand; operand = operand->sibling) {
    operandType = SemaTypeFromExpr(operand, symbolTable, fileCtx);
    if (!operandType) {
      goto CLEANUP;
    }
    if (!SemaTypeIsPrimType(operandType, SEMA_PRIM_TYPE_BOOL)) {
      SourceLocation *loc = &operand->loc;
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
      fprintf(stderr, "expected operand of logical operator to be of type "
              "bool, got type "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, operandType);
      fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
      SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
      operand = operand->sibling;
      goto CLEANUP;
    }
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = operandType;
  typeInfo->isTypeOwner = false;
  return operandType;
CLEANUP:
  for (; operand; operand = operand->sibling) {
    operand->semaInfo.skipAnalysis = true;
  }
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

SemaType *SemaTypeCheckComparisonOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  // The AST of the expression 3 < 5 > 7 == 9 looks like this:
  //
  //         ==
  //        /
  //       > - 9
  //      /
  //     < - 7
  //    /
  //   3 - 5
  //
  // Note that we must process all of these operands (3, 5, 7, 9) in one go,
  // otherwise the comparison operator returns bool, and it won't type check
  // with the subsequent comparison.
  //
  // We will first traverse to the leftmost child of the comparison chain,
  // and record all of the right children in a stack, then we can type check
  // each operand in order.
  
  Vector *operandsVec = VectorNew();
  SyntaxAST *rootExpr = expr;
  for (; SemaIsComparisonExpr(expr); expr = expr->firstChild) {
    VectorAdd(operandsVec, expr->lastChild);
  }
  VectorAdd(operandsVec, expr);

  typedef enum {
    FLOAT,
    SIGNED,
    UNSIGNED,
    NON_NUMERIC
  } TypeGroup;

  void **operands = operandsVec->arr;
  int numOperands = operandsVec->size;
  int operandIndex;
  SyntaxAST *firstOperand = operands[numOperands - 1];
  SyntaxAST *failingOperand;
  TypeGroup typeGroup;
  for (operandIndex = numOperands - 1; operandIndex >= 0; --operandIndex) {
    SyntaxAST *operand = failingOperand = operands[operandIndex];
    SemaType *operandType = SemaTypeFromExpr(operand, symbolTable, fileCtx);
    if (!operandType) {
      goto CLEANUP;
    }
    TypeGroup curTypeGroup;
    if (SemaTypeIsFloat(operandType)) {
      curTypeGroup = FLOAT;
    } else if (SemaTypeIsSigned(operandType)) {
      curTypeGroup = SIGNED;
    } else if (SemaTypeIsUnsigned(operandType)) {
      curTypeGroup = UNSIGNED;
    } else {
      curTypeGroup = NON_NUMERIC;
    }
    if (operand != firstOperand && curTypeGroup != typeGroup) {
      SourceLocation *loc = &operand->loc;
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
      fprintf(stderr, "operand of "SOURCE_COLOR_RED);
      switch (curTypeGroup) {
        case FLOAT:    fprintf(stderr, "float"); break;
        case SIGNED:   fprintf(stderr, "signed"); break;
        case UNSIGNED: fprintf(stderr, "unsigned"); break;
        case NON_NUMERIC: fprintf(stderr, "non-numeric"); break;
      }
      fprintf(stderr, SOURCE_COLOR_RESET" type does not match operand of "
              SOURCE_COLOR_GREEN);
      switch (typeGroup) {
        case FLOAT:    fprintf(stderr, "float"); break;
        case SIGNED:   fprintf(stderr, "signed"); break;
        case UNSIGNED: fprintf(stderr, "unsigned"); break;
        case NON_NUMERIC: fprintf(stderr, "non-numeric"); break;
      }
      fprintf(stderr, SOURCE_COLOR_RESET" type\n");
      SourceLocationPrint(
          fileCtx->source, 2, SOURCE_COLOR_GREEN, &firstOperand->loc,
          SOURCE_COLOR_RED, loc);
      --operandIndex;
      goto CLEANUP;
    }
    typeGroup = curTypeGroup;
  }
  SemaType *boolType = malloc(sizeof(SemaType));
  SemaTypeFromSemaPrimType(boolType, SEMA_PRIM_TYPE_BOOL);
  for (expr = rootExpr; SemaIsComparisonExpr(expr); expr = expr->firstChild) {
    SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
    typeInfo->type = boolType;
    typeInfo->isTypeOwner = expr == rootExpr;
  }
  VectorDelete(operandsVec);
  return boolType;
CLEANUP:
  for (; operandIndex >= 0; --operandIndex) {
    SyntaxAST *operand = operands[operandIndex];
    operand->semaInfo.skipAnalysis = true;
  }
  for (expr = rootExpr; SemaIsComparisonExpr(expr); expr = expr->firstChild) {
    expr->semaInfo.skipAnalysis = true;
  }
  VectorDelete(operandsVec);
  return NULL;
}

bool SemaTypeIsPrimType(SemaType *type, SemaPrimType primType) {
  return type->kind == SEMA_TYPE_KIND_PRIM_TYPE &&
         type->primType == primType;
}

bool SemaIsComparisonExpr(SyntaxAST *expr) {
  if (expr->kind != SYNTAX_AST_KIND_OP) {
    return false;
  }
  switch (expr->op) {
    case SYNTAX_OP_LT:
    case SYNTAX_OP_LE:
    case SYNTAX_OP_EQEQ:
    case SYNTAX_OP_NEQ:
    case SYNTAX_OP_GT:
    case SYNTAX_OP_GE:
      return true;
    default:
      return false;
  }
}
