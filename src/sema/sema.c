#include "sema.h"
#include "parse/parser/parser.h"

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

void SemaNew(SemaCtx *ctx) {
  ctx->fileCtxs = VectorNew();
}

SemaFileCtx *SemaFileCtxNew(
    const char *path, SyntaxAST *importDecl, const char *importFilePath) {
  FILE *file = fopen(path, "r");
  if (!file) {
    if (importDecl) {
      SourceLocation *loc = &importDecl->loc;
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
              importFilePath, loc->from.lineNo + 1);
      fprintf(stderr, "imported file does not exist\n");
      Source *source = SourceFromFile(importFilePath);
      SourceLocationPrint(source, 1, SOURCE_COLOR_RED, loc);
      SourceDelete(source);
    } else {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET);
      fprintf(stderr, " compilation file %s does not exist\n");
    }
    return NULL;
  }

  SyntaxAST *node = SyntaxParseFile(file, path);
  if (node == PARSER_OBJECT_FAILURE) {
    fclose(file);
    return NULL;
  }

  SemaFileCtx *fileCtx = malloc(sizeof(SemaFileCtx));
  fileCtx->path = path;
  fileCtx->file = file;
  fileCtx->node = node;
  fileCtx->symbolTable = HashTableNew(
      ParserStringHash, ParserStringEqual, free, SemaFileCtxDelete);
  return fileCtx;
}

void SemaFileCtxDelete(void *p) {
  SemaFileCtx *fileCtx = p;
  fclose(file);
  SyntaxASTDelete(fileCtx->node);
  HashTableDelete(fileCtx->symbolTable);
  free(fileCtx);
}

bool SemaAddAllFiles(SemaCtx *ctx, const char *path) {
  // Add the root file
  SemaFileCtx *fileCtx = SemaFileCtxNew(path, NULL, NULL);
  if (!fileCtx)
    return false;
  Vector *fileCtxs = ctx->fileCtxs;
  VectorAdd(fileCtxs, fileCtx);

  // Record the set of imported file paths so that we do not import the same
  // file twice
  HashTable *importedFiles = HashTableNew(
      ParserStringHash, ParserStringEqual, NULL, NULL);
  HashTableEntryAdd(importedFiles, path, NULL);
  // Maintain a stack of AST nodes of files whose imports have yet to be
  // processed
  Vector *stack = VectorNew();
  VectorAdd(stack, fileCtx->node);
  // Process until stack is empty
  while (stack->size != 0) {
    SyntaxAST *importDecls = node->firstChild;
    assert(importDecls);
    // Iterate through all import declarations
    for (SyntaxAST *importDecl = importDecls->firstChild; importDecl;
         importDecl = importDecl->sibling) {
      // Convert module path to real import path (a string)
      // TODO: provide functionality import files from separate directories
      SyntaxAST *modulePath = importDecl->firstChild;
      assert(modulePath);
      int capacity = 16, length = 0;
      char *importPath = malloc(capacity);
      importPath[0] = '\0';
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

      // TODO: continue here
    }
  }

  // Cleanup
  HashTableDelete(importedFiles);

  return true;
}

bool SemaCheck(SemaCtx *ctx, const char *path) {
  if (!SemaAddAllFiles(ctx, path))
    return false;
}
