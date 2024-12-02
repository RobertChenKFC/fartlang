#include "sema.h"
#include "parse/parser/parser.h"
#include <stdlib.h>
#include <assert.h>

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
void SemaSymInfoDelete(void *p);
// Frees the contents of the SemaType "type" but does not deallocate the memory
// used by "type" itself
void SemaTypeDeleteContent(SemaType *type);
// Initializes all the SemaInfo stored in the AST "node" and recursively
// initializes all descendants
void SemaASTInit(SyntaxAST *node);

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
      fprintf(stderr, " compilation file %s does not exist\n", path);
    }
    return NULL;
  }

  SyntaxAST *node = SyntaxParseFile(file, path);
  if (node == PARSER_OBJECT_FAILURE) {
    fclose(file);
    return NULL;
  }
  SemaASTInit(node);

  SemaFileCtx *fileCtx = malloc(sizeof(SemaFileCtx));
  fileCtx->path = strdup(path);
  fileCtx->file = file;
  fileCtx->source = SourceFromFile(file);
  fileCtx->node = node;
  fileCtx->symbolTable = HashTableNew(
      ParserStringHash, ParserStringEqual, NULL, SemaSymInfoDelete);
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
  HashTableEntryAdd(importedFiles, (void*)path, NULL);
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
      if (entry)
        continue;
      // Parse the file
      SemaFileCtx *newFileCtx = SemaFileCtxNew(
          importPath, importDecl, fileCtx->path);
      if (!newFileCtx) {
        success = false;
        goto CLEANUP;
      }
      // Add the file to the hash table and stack
      HashTableEntryAdd(importedFiles, importPath, NULL);
      VectorAdd(fileCtxs, newFileCtx);
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
        fprintf(stderr, "redeclaration of class \"%s\" in the current module\n",
                className);
        SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                            &classDecl->stringLoc);
        classDecl->semaInfo.skipAnalysis = true;
        success = false;
      } else {
        SemaSymInfo *classInfo = malloc(sizeof(SemaSymInfo));
        classInfo->type.kind = SEMA_TYPE_KIND_CLASS;
        classInfo->type.memberTable = HashTableNew(
            ParserStringHash, ParserStringEqual, NULL, SemaSymInfoDelete);
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
  SemaTypeDeleteContent(&symInfo->type);
  free(symInfo);
}

void SemaTypeDeleteContent(SemaType *type) {
  switch (type->kind) {
    case SEMA_TYPE_KIND_FN:
      VectorDelete(type->paramTypes);
      break;
    case SEMA_TYPE_KIND_CLASS:
      HashTableDelete(type->memberTable);
      break;
    default:
      break;
  }
}

void SemaASTInit(SyntaxAST *node) {
  SemaInfo *info = &node->semaInfo;
  info->skipAnalysis = false;

  for (SyntaxAST *cur = node->firstChild; cur; cur = cur->sibling) {
    SemaASTInit(cur);
  }
}
