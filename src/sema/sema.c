#include "sema.h"
#include "parse/parser/parser.h"
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

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
// initializes all descendants. The initialization includes:
// - Setting "node->semaInfo.skipAnalysis" to false
// - Setting "node->semaInfo.fileCtx" to the provided "fileCtx"
void SemaASTInit(SyntaxAST *node, SemaFileCtx *fileCtx);
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
  SemaASTInit(node, fileCtx);
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

void SemaASTInit(SyntaxAST *node, SemaFileCtx *fileCtx) {
  SemaInfo *info = &node->semaInfo;
  info->skipAnalysis = false;
  info->fileCtx = fileCtx;

  for (SyntaxAST *cur = node->firstChild; cur; cur = cur->sibling) {
    SemaASTInit(cur, fileCtx);
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

          // DEBUG
          printf("Adding %s to symbol table %p\n", varIdentifier, symbolTable);

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
  switch (node->kind) {
    case SYNTAX_AST_KIND_IMPORT_DECL:
      deleteSymInfo = !node->import.isWildcard;
    case SYNTAX_AST_KIND_CLASS_DECL:
    case SYNTAX_AST_KIND_VAR_INIT:
    case SYNTAX_AST_KIND_METHOD_DECL:
      if (deleteSymInfo) {
        SemaSymInfo *symInfo = info->symInfo;
        if (!info->skipAnalysis) {
          SemaTypeInfo *typeInfo = &symInfo->typeInfo;
          SemaTypeInfoDelete(typeInfo);
        }
        free(symInfo);
      }
      break;
    case SYNTAX_AST_KIND_LITERAL:
      SemaTypeInfoDelete(&info->typeInfo);
      break;
  }
}

void SemaTypeFromSemaPrimType(SemaType *type, SemaPrimType primType) {
  type->kind = SEMA_TYPE_KIND_PRIM_TYPE;
  type->primType = primType;
}

bool SemaTypeCheck(SemaCtx *ctx) {
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
              continue;
            }
            if (varTypeInfo->type->kind == SEMA_TYPE_KIND_PLACEHOLDER) {
              // This is an auto-deducted type, so replace the placeholder type
              // with the newly generated type
              if (varTypeInfo->isTypeOwner) {
                SemaTypeDelete(varTypeInfo->type);
              }
              varTypeInfo->type = initExprType;
              varTypeInfo->isTypeOwner =
                  initExpr->semaInfo.typeInfo.isTypeOwner;
            } else {
              // This variable is declared with a type, so must check if the
              // types match
              assert(false);
            }
          } else {
            assert(varTypeInfo->type->kind != SEMA_TYPE_KIND_PLACEHOLDER);
          }
        }
      }
    }
  }
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

      // DEBUG
      printf("Trying to retrieve %s from symbol table %p\n",
             varName, symbolTable);
      printf("Symbol table currently has the following entries:\n");
      for (HashTableEntry *curEntry = symbolTable->head; curEntry;
           curEntry = curEntry->nextInTable) {
        printf("- entry: %s\n", (char*)curEntry->key);
      }
      printf("====== end of entries =====\n");

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
    case SYNTAX_OP_TERNARY: {
      SyntaxAST *trueNode = expr->firstChild;
      SyntaxAST *condNode = trueNode->sibling;
      SyntaxAST *falseNode = condNode->sibling;
      SemaType *condType = SemaTypeFromExpr(
          condNode, symbolTable, fileCtx);
      SemaType *trueType = SemaTypeFromExpr(
          trueNode, symbolTable, fileCtx);
      SemaType *falseType = SemaTypeFromExpr(
          falseNode, symbolTable, fileCtx);
      if (condType->kind != SEMA_TYPE_KIND_PRIM_TYPE ||
          condType->primType != SEMA_PRIM_TYPE_BOOL) {
        SourceLocation *loc = &condNode->loc;
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
        fprintf(stderr, "expected condition to be of type bool, got type "
                SOURCE_COLOR_RED);
        SemaTypePrint(stderr, condType);
        fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
        SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
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
      }
      break;
    } default: {
      assert(false);
    }
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
