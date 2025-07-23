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
// an error message using "fileCtx" and NULL is returned. If succeeds, also
// sets the attribute "attr" of the method declaration
SemaType *SemaTypeFromMethodDecl(
    SyntaxAST *methodDecl, HashTable *symbolTable, SemaFileCtx *fileCtx,
    SemaAttr *attr);
// Construct a SemaType "type" from the base primitive type "primType"
void SemaTypeFromSemaPrimType(SemaType *type, SemaPrimType primType);
// Type checks all each variable declaration and statement in "ctx". Returns
// true if and only if every statement correctly type checks
bool SemaTypeCheck(SemaCtx *ctx);
// Given the AST "term" of a term, returns the SemaType of this
// expression. If the expression successfully type checks, then the
// corresponding SemaType is returned. Otherwise, and error message is printed
// using "fileCtx" and NULL is returned. The "parentExpr" that uses this term
// is also provided (or NULL if there is no parent expression)
SemaType *SemaTypeFromTerm(
    SyntaxAST *term, SyntaxAST *parentExpr, HashTable *symbolTable,
    SemaFileCtx *fileCtx);
// Same as SemaTypeFromTerm, but for AST "expr" of expressions instead
SemaType *SemaTypeFromExpr(
    SyntaxAST *expr, SyntaxAST *parentExpr, HashTable *symbolTable,
    SemaFileCtx *fileCtx);
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
SemaType *SemaTypeFromBitwiseOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if the condition of ternary "expr" is of type bool, and the true and
// false branches of the expression evaluate to the same type. If so, return
// the type of the true branch, otherwise, return NULL. Remaining arguments are
// used in the same way as above
SemaType *SemaTypeFromTernaryOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if all operands of the logic "expr" is of type bool. If so, return
// the type of the last operand, otherwise return NULL. Remaining arguments
// are used in the same way as above
SemaType *SemaTypeFromLogicOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if all operands are float types, all are signed types or all are
// unsigned types. If so, return the type with the largest bitwidth, otherwise
// return NULL. Remaining arguments are used in the same way as above
SemaType *SemaTypeFromComparisonOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if "type" is a primitive type of kind "primType"
bool SemaTypeIsPrimType(SemaType *type, SemaPrimType primType);
// Checks if "expr" is a comparison expression
bool SemaIsComparisonExpr(SyntaxAST *expr);
// Performs the type unification procedure for "type1" and "type2". Returns
// the unified type if the procedure succeeds, and NULL if the procedure fails.
// "isTypeOwner" is set to true if and only if a new type is allocated and
// returned
SemaType *SemaTypeUnification(
    SemaType *type1, SemaType *type2, bool *isTypeOwner);
// Checks if "type1" can be implicitly cast to "type2"
bool SemaTypeImplicitCast(SemaType *type1, SemaType *type2);
// Given the AST "expr" for an expression and its "type", if "type" is void,
// prints an error message using "fileCtx". Returns true if and only if "type"
// is not void
bool SemaErrorIfVoid(SyntaxAST *expr, SemaType *type, SemaFileCtx *fileCtx);
// Checks if "type" is a class type
bool SemaTypeIsClass(SemaType *type);
// Checks if the expresShiftOpinside the alloc "expr" is of unsigned type, and
// the type inside the alloc "expr" is well formed. If so, return the type of
// the alloc expression, which is the type inside the alloc "expr" with one
// additional array level. Otherwise, return NULL. Remaining arguments are
// used in the same way as all the type check functions above
SemaType *SemaTypeFromAlloc(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Returns a newly allocated type with one more array level than "baseType"
SemaType *SemaTypeIncreaseArrayLevel(SemaType *baseType);
// Checks if the left operand of the shift expression "expr" is of integral
// type, and the right operand is of unsigned type. If so, return the type of
// the shift expression, otherwise return NULL. The remaining arguments are
// used in the same way as all the type check functions above
SemaType *SemaTypeFromShiftOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if "type" is an integral type
bool SemaTypeIsIntegral(SemaType *type);
// Checks if applying type unification to the left and right operands of "expr"
// results in a numeric type. If so, return the unified type, otherwise return
// NULL. Additional type checks are performed for specific arithmetic operators
// with the helper functions below. The remaining arguments are used in the same
// way as all the type check functions above
SemaType *SemaTypeFromArithOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Checks if "type" is a numeric type
bool SemaTypeIsNumeric(SemaType *type);
// Check if "leftOperandType" and "rightOperandType" are both integral types.
// The remaining arguments are used for error reporting
bool SemaTypeCheckModOp(
    SyntaxAST *leftOperand, SemaType *leftOperandType, SyntaxAST *rightOperand,
    SemaType *rightOperandType, SemaFileCtx *fileCtx);
// Check if "operandType" is not an unsigned types. The remaining arguments are
// used for error reporting
bool SemaTypeCheckNegOp(
    SyntaxAST *operand, SemaType *operandType, SemaFileCtx *fileCtx);
// Generic type check function for cast operation "expr". Extracts the operand
// type and target type of the "expr" and feed them to the "check" function.
// If the check passes, then
// (1) If "retBoolType" is true, a new bool SemaType is created and returned
// (2) If "retBoolType", the target type is returned
// Otherwise, NULL is returned. The remaining arguments are used in the same way
// as the above type checking functions
typedef bool (*SemaCastOpCheckFn)(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx);
SemaType *SemaTypeFromCastOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx,
    SemaCastOpCheckFn check, bool retBoolType);
// Check if the "operandType" is of type any
bool SemaTypeCheckCastIsOp(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx);
// Check if the "operandType" is not a void type
bool SemaTypeCheckCastAsOp(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx);
// Check for one of the following cases in order:
// 1. If the operand is of type any or if the target type is any, check succeeds
// 2. If the operand is of type nil, check if the target type is a class type
// 3. Check if the operand type and the target type are both numeric types
bool SemaTypeCheckCastIntoOp(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx);
// If the stage currently stored in "info" is earlier than the provided "stage",
// then update the stored stage to the provided "stage"
void SemaInfoUpdateStage(SemaInfo *info, SemaStage stage);
// Check if the operand of "expr" is a function type, and each of the type of
// the sub-expressions matches the argument types of the function type. If so,
// return the return type of the function type, otherwise return NULL. The
// remaining arguments are used in the same way as above
SemaType *SemaTypeFromCall(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Check if "type" is a function type
bool SemaTypeIsFunction(SemaType *type);
// Returns true if and only if type checking the class variable declarations
// stored in "varDecls" succeeds. There are two HashTable's: "memberTable", which
// is the table that contains the variables, and "symbolTable", which is the
// table lookup in the current scope. The remaining arguments are used in the
// same way as the type check functions above
bool SemaTypeCheckClassVarDecls(
    SyntaxAST *varDecls, HashTable *memberTable, HashTable *symbolTable,
    SemaFileCtx *fileCtx);
// Returns true if and only if type checking the variable initializations stored
// in "varInitList" succeeds. The remaining arguments are used in the same way
// as the type check functions above
bool SemaTypeCheckVarInitList(
    SyntaxAST *varInitList, HashTable *memberTable, HashTable *symbolTable,
    SemaFileCtx *fileCtx);
// Returns true if and only if type checking the method body stored under
// "methodDecl" succeeds, and the return value of the body is consistent with
// the method declaration. The remaining arguments are used in the same way
// as the type check functions above
bool SemaTypeCheckMethodDecl(
    SyntaxAST *methodDecl, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Populates the varaiable declared in "var" to "memberTable". Note that the
// name of the identifier "varName" and the type of the variable "varType" are
// specified as separate arguments to the function so that this is a general
// function that can take on any form of variable declaration: class variables,
// method variables and method parameters. The variable attribute "attr" is
// also taken as a separate argument and will be used to set the attribute
// of the SemaSymInfo of the variable.
//
// Note that there are two HashTable's: "memberTable", which is the table we
// will populate the variable with, and "symbolTable", which is the symbol table
// to lookup in the current scope (mainly to type check "varType"). Returns true
// if and only if the the variable name doesn't already exist in the
// "memberTable", and "varType" (if provided, can be NULL) is well-formed. The
// "addToScope" argument specifies whether to also add the variable to the
// current scope. The remaining arguments are used in the same way as the type
// check functions above
bool SemaPopulateVar(
    SyntaxAST *var, char *varName, SourceLocation *varLoc, SyntaxAST *varType,
    SemaAttr attr, bool addToScope, HashTable *symbolTable,
    HashTable *memberTable, SemaFileCtx *fileCtx);
// Adds a new scope with no symbols to the "fileCtx"
void SemaPushScope(SemaFileCtx *fileCtx);
// Remove the most lately-pushed scope in "fileCtx", and removing all the
// symbols in the scope from "symbolTable"
void SemaPopScope(SemaFileCtx *fileCtx, HashTable *symbolTable);
// Returns true if and only if type checking the statement stored in "stmt"
// succeeds. The remaining arguments are used in the same way as the type check
// functions above
bool SemaTypeCheckStmt(
    SyntaxAST *stmt, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Returns true if and only if type checking the variable declaration statement
// stored in "stmt" succeeds. Note that this function is specifically used for
// variable declaration statements in method bodies and nowhere else! The
// remaining arguments are used in the same way as the type check functions
// above
bool SemaTypeCheckVarDeclStmt(
    SyntaxAST *stmt, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Sets skipAnalysis to true for "node" and all its descendants
void SemaSkipAnalysisForSubtree(SyntaxAST *node);
// Sets skipAnalysis to true for "node" and all its siblings and descendants
void SemaSkipAnalysisForSubforest(SyntaxAST *node);
// Checks if the AST node "thisNode" of the this literal is enclosed in a
// constructor method. If so, return the type of the enclosing class, otherwise
// return NULL. The remaining arguments are used in the same way as the type
// checking functions above
SemaType *SemaTypeFromThisLiteral(
    SyntaxAST *thisNode, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Returns the namespace identifier string of the provided "importDecl". If
// "loc" is provided (not NULL), will also set "loc" to the location of the
// namespace identifier. There are three cases (decided in this order):
// (1) If the import declaration is a wildcard import, returns NULL. "loc" will
//     not be set in this case
// (2) If the import declaration supplies a namespace, returns the supplied
//     namespace
// (3) Otherwise, return the last identifier of the module path
char *SemaGetNamespaceIdentifier(SyntaxAST *importDecl, SourceLocation *loc);
// Populate "memberTable" with all the variables declared in "varDecl". See
// SemaPopulateVar for usage of the remaining parameters. Returns true if every
// variable was populated successfully
bool SemaPopulateVarDecl(
    SyntaxAST *varDecl, HashTable *symbolTable, HashTable *memberTable,
    bool addToScope, SemaFileCtx *fileCtx);
// Check if the "memberAccess" is well-formed by checking if the member access'
// identifier is a member of the member access' operand, taking into account
// whether the operand is a namespace, class or instance of a class. If type
// check succeeds, returns the type of the member, otherwise return NULL. The
// remaining arguments are used in the same way as the type check functions
// above
SemaType *SemaTypeFromMemberAccess(
    SyntaxAST *memberAccess, SyntaxAST *parentExpr, HashTable *symbolTable,
    SemaFileCtx *fileCtx);
// Returns true if and only if an expression or term stored in the AST node
// "value" can be captured in a variable, ie. is not a class or a namespace.
// Note that "value" must be type checked by the appropriate function
// (SemaTypeFromExpr or SemaTypeFromTerm) before being passed to this function
bool SemaValueIsCapturable(SyntaxAST *value);
// Returns true if and only if the AST "node" is a namespace. Note that "node"
// must be type checked by the appropriate function (SemaTypeFromExpr or
// SemaTypeFromTerm) before being passed to this function
bool SemaNodeIsNamespace(SyntaxAST *node);
// Calls SemaPrintErrorForUncapturableValue when "value" is a class or namespace
// used in "parentExpr" and "parentExpr" is not an access operator. Note that
// "parentExpr" must be type checked by the appropriate function prior to
// calling. Returns true if and only if no errors were printed
bool SemaCheckErrorForUncapturableValue(
    SyntaxAST *value, SyntaxAST *parentExpr, SemaFileCtx *fileCtx);
// Print the error message for "value" when "value" is a class or namespace used
// in "parentExpr", but "parentExpr" is not an access operator.
void SemaPrintErrorForUncapturableValue(SyntaxAST *value, SemaFileCtx *fileCtx);
// Check if the operand of "expr" is an array type, and the index expression
// is an unsigned type. If so, return the type of "expr" with one fewer array
// dimension, otherwise return NULL. The remaining arguments are used in the
// same way as above
SemaType *SemaTypeFromIndexOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx);
// Returns a type with one fewer array level than "arrayType". A new type is
// allocated if and only if "isTypeOwner" is set to true
SemaType *SemaTypeDecreaseArrayLevel(SemaType *arrayType, bool *isTypeOwner);

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
  fileCtx->scopes = VectorNew();
  fileCtx->classType = NULL;
  fileCtx->methodSymInfo = NULL;

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
  VectorDelete(fileCtx->scopes);
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

      // Import declaration does not have any type information here yet
      symInfo->typeInfo.isTypeOwner = false;
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
        classInfo->attr = SEMA_ATTR_CLASS;
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
      VectorDelete(type->paramTypes);
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
  for (SyntaxAST *cur = node->firstChild; cur; cur = cur->sibling) {
    va_list argsCopy;
    va_copy(argsCopy, args);
    SemaASTInitV(cur, init, argsCopy);
    va_end(argsCopy);
  }
  init(info, args);
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
            fprintf(stderr, "wildcard import brings in used identifier "
                    SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET"\n", symbol);
            SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                                &importDecl->import.extLoc);
            importDecl->semaInfo.skipAnalysis = true;
            free(importDecl->semaInfo.symInfo);
            success = false;
            break;
          }
          HashTableEntryAdd(symbolTable, symbol, importEntry->value);
        }
      } else {
        SourceLocation namespaceLoc;
        char *namespace = SemaGetNamespaceIdentifier(importDecl, &namespaceLoc);
        HashTableEntry *entry = HashTableEntryRetrieve(symbolTable, namespace);
        if (entry) {
          fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
                  fileCtx->path, importDecl->loc.from.lineNo + 1);
          fprintf(stderr, "reuse of identifier "SOURCE_COLOR_RED"%s"
                  SOURCE_COLOR_RESET" for import namespace\n", namespace);
          SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED,
                              &namespaceLoc);
          importDecl->semaInfo.skipAnalysis = true;
          free(importDecl->semaInfo.symInfo);
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

      fileCtx->classType = classDecl->semaInfo.symInfo->typeInfo.type;

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
        if (!SemaPopulateVarDecl(
              varDecl, symbolTable, memberTable, /*addToScope=*/false,
              fileCtx)) {
          success = false;
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
          SemaSkipAnalysisForSubtree(methodDecl);
          continue;
        }

        // Check if method return type and parameter types are valid
        SemaAttr methodAttr;
        SemaType *methodType = SemaTypeFromMethodDecl(
            methodDecl, symbolTable, fileCtx, &methodAttr);
        if (!methodType) {
          success = false;
          methodDecl->semaInfo.skipAnalysis = true;
          continue;
        }

        SemaSymInfo *methodInfo = malloc(sizeof(SemaSymInfo));
        methodInfo->attr = methodAttr;
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
        type->isBaseTypeOwner = false;
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

      SemaTypeInfo *retTypeInfo = &retSyntaxType->semaInfo.typeInfo;
      SemaType *retType = SemaTypeFromSyntaxType(
          retSyntaxType, symbolTable, fileCtx, parentAST,
          &retTypeInfo->isTypeOwner);
      if (!retType) {
        retSyntaxType->semaInfo.skipAnalysis = true;
        free(type);
        return NULL;
      }
      retTypeInfo->type = retType;
      // See set-to-a-later-stage for more info
      retSyntaxType->semaInfo.stage = SEMA_STAGE_TYPE_CHECK;

      Vector *paramTypes = VectorNew();
      for (SyntaxAST *paramSyntaxType = paramSyntaxTypes->firstChild;
           paramSyntaxType; paramSyntaxType = paramSyntaxType->sibling) {
        assert(paramSyntaxType &&
               paramSyntaxType->kind == SYNTAX_AST_KIND_TYPE);
        SemaTypeInfo *paramTypeInfo = &paramSyntaxType->semaInfo.typeInfo;
        SemaType *paramType = SemaTypeFromSyntaxType(
            paramSyntaxType, symbolTable, fileCtx, parentAST,
            &paramTypeInfo->isTypeOwner);
        if (!paramType) {
          paramSyntaxType->semaInfo.skipAnalysis = true;
          free(type);
          SemaTypeDelete(retType);
          VectorDelete(paramTypes);
        }
        VectorAdd(paramTypes, paramType);
        paramTypeInfo->type = paramType;
        // See set-to-a-later-stage for more info
        paramSyntaxType->semaInfo.stage = SEMA_STAGE_TYPE_CHECK;
      }

      type->kind = SEMA_TYPE_KIND_FN;
      type->retType = retType;
      type->paramTypes = paramTypes;
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
    SyntaxAST *methodDecl, HashTable *symbolTable, SemaFileCtx *fileCtx,
    SemaAttr *attr) {
  SemaType *methodType = malloc(sizeof(SemaType));
  methodType->kind = SEMA_TYPE_KIND_FN;

  SyntaxAST *retSyntaxType = methodDecl->firstChild;
  SyntaxAST *paramList = retSyntaxType->sibling;
  SyntaxAST *param = paramList->firstChild;
  assert(retSyntaxType && retSyntaxType->kind == SYNTAX_AST_KIND_TYPE);
  SemaTypeInfo *retTypeInfo = &retSyntaxType->semaInfo.typeInfo;

  SemaAttr methodAttr;
  switch (methodDecl->method.type) {
    case SYNTAX_METHOD_TYPE_FN: {
      // The parser indicates that a method is a constructor by setting the
      // return type of the method to SYNTAX_TYPE_THIS
      bool isCtor =
          retSyntaxType->type.baseType == SYNTAX_TYPE_THIS &&
          retSyntaxType->type.arrayLevels == 0 &&
          !retSyntaxType->firstChild;
      if (isCtor) {
        methodAttr = SEMA_ATTR_CTOR;
      } else {
        methodAttr = SEMA_ATTR_FN;
      }
      break;
    } case SYNTAX_METHOD_TYPE_METHOD: {
      methodAttr = SEMA_ATTR_METHOD;
      break;
    } default: {
      assert(false);
    }
  }
  *attr = methodAttr;

  SemaType *retType;
  if (methodAttr == SEMA_ATTR_CTOR) {
    // Since SYNTAX_TYPE_THIS cannot be converted to a valid SemaType, set it
    // to the type of the current enclosing class
    assert(fileCtx->classType);
    retType = fileCtx->classType;
    retTypeInfo->isTypeOwner = false;
  } else {
    retType = SemaTypeFromSyntaxType(
        retSyntaxType, symbolTable, fileCtx, methodDecl,
        &retTypeInfo->isTypeOwner);
    if (!retType) {
      goto RET_TYPE_CLEANUP;
    }
  }
  retTypeInfo->type = methodType->retType = retType;
  // See set-to-a-later-stage for more info
  retSyntaxType->semaInfo.stage = SEMA_STAGE_TYPE_CHECK;

  Vector *paramTypes = VectorNew();
  if (methodAttr == SEMA_ATTR_METHOD) {
    // Methods take on an implicit argument "this" of the enclosing class type
    assert(fileCtx->classType);
    VectorAdd(paramTypes, fileCtx->classType);
  }
  methodType->paramTypes = paramTypes;
  assert(paramList && paramList->kind == SYNTAX_AST_KIND_PARAM_LIST);
  for (; param; param = param->sibling) {
    assert(param->kind == SYNTAX_AST_KIND_PARAM);
    SyntaxAST *paramSyntaxType = param->firstChild;
    SemaSymInfo *paramSymInfo = malloc(sizeof(SemaSymInfo));
    param->semaInfo.symInfo = paramSymInfo;
    SemaTypeInfo *paramTypeInfo = &paramSyntaxType->semaInfo.typeInfo;
    assert(paramSyntaxType && paramSyntaxType->kind == SYNTAX_AST_KIND_TYPE);
    SemaType *paramType = SemaTypeFromSyntaxType(
        paramSyntaxType, symbolTable, fileCtx, methodDecl,
        &paramTypeInfo->isTypeOwner);
    if (!paramType) {
      VectorDelete(paramTypes);
      free(paramSymInfo);
      goto PARAM_CLEANUP;
    }
    VectorAdd(paramTypes, paramType);
    SemaTypeInfo *paramSymTypeInfo = &paramSymInfo->typeInfo;
    paramSymTypeInfo->type = paramTypeInfo->type = paramType;
    paramSymTypeInfo->isTypeOwner = false;

    // See set-to-a-later-stage for more info
    paramSyntaxType->semaInfo.stage = SEMA_STAGE_TYPE_CHECK;
  }
  return methodType;

RET_TYPE_CLEANUP:
  retSyntaxType->semaInfo.skipAnalysis = true;
PARAM_CLEANUP:
  for (; param; param = param->sibling) {
    param->semaInfo.skipAnalysis = true;
    param->firstChild->semaInfo.skipAnalysis = true;
  }
  free(methodType);
  return NULL;
}

void SemaDeleteASTSemaInfo(SyntaxAST *node) {
  bool deleteSymInfo = true;
  SemaInfo *info = &node->semaInfo;
  SemaStage stage = info->stage;
  switch (node->kind) {
    case SYNTAX_AST_KIND_IMPORT_DECL:
      deleteSymInfo = stage >= SEMA_STAGE_ADD_ALL_FILES;
      goto DELETE_SYM_INFO;
    case SYNTAX_AST_KIND_CLASS_DECL:
      deleteSymInfo = stage >= SEMA_STAGE_POPULATE_CLASS_SYMBOLS;
      goto DELETE_SYM_INFO;
    case SYNTAX_AST_KIND_VAR_INIT:
      deleteSymInfo = stage >= SEMA_STAGE_POPULATE_MEMBERS;
      goto DELETE_SYM_INFO;
    case SYNTAX_AST_KIND_PARAM:
      deleteSymInfo = stage >= SEMA_STAGE_POPULATE_MEMBERS;
      goto DELETE_SYM_INFO;
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
    case SYNTAX_AST_KIND_TYPE:
    case SYNTAX_AST_KIND_MEMBER_ACCESS:
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

      fileCtx->classType = classDecl->semaInfo.symInfo->typeInfo.type;

      // Type check all class variable initializations
      SyntaxAST *varDecls = classDecl->firstChild;
      SemaSymInfo *classInfo = classDecl->semaInfo.symInfo;
      SemaTypeInfo *classTypeInfo = &classInfo->typeInfo;
      HashTable *memberTable = classTypeInfo->type->memberTable;
      if (!SemaTypeCheckClassVarDecls(
            varDecls, memberTable, symbolTable, fileCtx)) {
        success = false;
      }

      // Type check all methods
      SyntaxAST *methodDecls = varDecls->sibling;
      assert(methodDecls && methodDecls->kind == SYNTAX_AST_KIND_METHOD_DECLS);
      for (SyntaxAST *methodDecl = methodDecls->firstChild; methodDecl;
           methodDecl = methodDecl->sibling) {
        assert(methodDecl && methodDecl->kind == SYNTAX_AST_KIND_METHOD_DECL);
        fileCtx->methodSymInfo = methodDecl->semaInfo.symInfo;

        if (!SemaTypeCheckMethodDecl(methodDecl, symbolTable, fileCtx)) {
          success = false;
        }
      }
    }

    SemaASTInit(module, SemaASTInitTypeCheck);
  }
  return success;
}

SemaType *SemaTypeFromTerm(
    SyntaxAST *term, SyntaxAST *parentExpr, HashTable *symbolTable,
    SemaFileCtx *fileCtx) {
  SemaType *type = malloc(sizeof(SemaType));
  switch (term->kind) {
    case SYNTAX_AST_KIND_LITERAL: {
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
        case SYNTAX_TYPE_NULL:
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
        } case SYNTAX_TYPE_THIS: {
          free(type);
          return SemaTypeFromThisLiteral(term, symbolTable, fileCtx);
        } default: {
          printf("Literal kind: %d\n", term->literal.type);
          assert(false);
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
      SemaTypeInfo *varTypeInfo = &varInfo->typeInfo;
      SemaTypeInfo *typeInfo = &term->semaInfo.typeInfo;
      typeInfo->type = varTypeInfo->type;
      typeInfo->isTypeOwner = false;
      free(type);
      type = typeInfo->type;

      if (!SemaCheckErrorForUncapturableValue(term, parentExpr, fileCtx)) {
        type = NULL;
        term->semaInfo.skipAnalysis = true;
      }
      break;
    } default: {
      printf("Term kind: %d\n", term->kind);
      assert(false);
    }
  }
  return type;
}

SemaType *SemaTypeFromExpr(
    SyntaxAST *expr, SyntaxAST *parentExpr, HashTable *symbolTable,
    SemaFileCtx *fileCtx) {
  if (expr->kind == SYNTAX_AST_KIND_MEMBER_ACCESS) {
    return SemaTypeFromMemberAccess(expr, parentExpr, symbolTable, fileCtx);
  }
  if (expr->kind != SYNTAX_AST_KIND_OP) {
    return SemaTypeFromTerm(expr, parentExpr, symbolTable, fileCtx);
  }
  switch (expr->op) {
    case SYNTAX_OP_ALLOC:
      return SemaTypeFromAlloc(expr, symbolTable, fileCtx);
    case SYNTAX_OP_TERNARY:
      return SemaTypeFromTernaryOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_LOGIC_OR:
    case SYNTAX_OP_LOGIC_AND:
    case SYNTAX_OP_NOT:
      return SemaTypeFromLogicOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_BIT_OR:
    case SYNTAX_OP_BIT_AND:
    case SYNTAX_OP_BIT_XOR:
    case SYNTAX_OP_BIT_NOT:
      return SemaTypeFromBitwiseOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_LT:
    case SYNTAX_OP_LE:
    case SYNTAX_OP_EQEQ:
    case SYNTAX_OP_NEQ:
    case SYNTAX_OP_GT:
    case SYNTAX_OP_GE:
      return SemaTypeFromComparisonOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_LSHIFT:
    case SYNTAX_OP_RSHIFT:
      return SemaTypeFromShiftOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_ADD:
    case SYNTAX_OP_SUB:
    case SYNTAX_OP_MUL:
    case SYNTAX_OP_DIV:
    case SYNTAX_OP_MOD:
    case SYNTAX_OP_NEG:
      return SemaTypeFromArithOp(expr, symbolTable, fileCtx);
    case SYNTAX_OP_CAST_IS:
      return SemaTypeFromCastOp(
          expr, symbolTable, fileCtx, SemaTypeCheckCastIsOp,
          /*retBoolType=*/true);
    case SYNTAX_OP_CAST_AS:
      return SemaTypeFromCastOp(
          expr, symbolTable, fileCtx, SemaTypeCheckCastAsOp,
          /*retBoolType=*/false);
    case SYNTAX_OP_CAST_INTO:
      return SemaTypeFromCastOp(
          expr, symbolTable, fileCtx, SemaTypeCheckCastIntoOp,
          /*retBoolType=*/false);
    case SYNTAX_OP_CALL:
      return SemaTypeFromCall(expr, symbolTable, fileCtx);
    case SYNTAX_OP_ARRAY_ACCESS:
      return SemaTypeFromIndexOp(expr, symbolTable, fileCtx);
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
    case SYNTAX_TYPE_NULL:
      SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_NIL);
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
      fprintf(file, "fn (");
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
        case SEMA_PRIM_TYPE_NIL:
          fprintf(file, "nil");
          break;
      }
      break;
    } case SEMA_TYPE_KIND_CLASS: {
      // TODO: improve type printing by also sourcing the location of where
      // each type is declared if the types happen to have the same name
      fprintf(file, "%s", type->node->string);
      break;
    } case SEMA_TYPE_KIND_NAMESPACE: {
      char *namespace = SemaGetNamespaceIdentifier(type->node, /*loc=*/NULL);
      assert(namespace);
      fprintf(file, "%s", namespace);
      break;
    } default: {
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
    default:
      assert(false);
  }
}

void SemaASTInitAddAllFiles(SemaInfo *info, va_list arg) {
  SemaFileCtx *fileCtx = va_arg(arg, SemaFileCtx*);
  SemaInfoUpdateStage(info, SEMA_STAGE_ADD_ALL_FILES);
  info->fileCtx = fileCtx;
}

void SemaASTInitPopulateClassSymbols(SemaInfo *info, va_list arg) {
  SemaInfoUpdateStage(info, SEMA_STAGE_POPULATE_CLASS_SYMBOLS);
}

void SemaASTInitPopulateImportSymbols(SemaInfo *info, va_list arg) {
  SemaInfoUpdateStage(info, SEMA_STAGE_POPULATE_IMPORT_SYMBOLS);
}

void SemaASTInitPopulateMembers(SemaInfo *info, va_list arg) {
  SemaInfoUpdateStage(info, SEMA_STAGE_POPULATE_MEMBERS);
}

void SemaASTInitTypeCheck(SemaInfo *info, va_list arg) {
  SemaInfoUpdateStage(info, SEMA_STAGE_TYPE_CHECK);
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

SemaType *SemaTypeFromBitwiseOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *firstOperand = expr->firstChild, *operand = firstOperand;
  SemaType *firstOperandType = SemaTypeFromExpr(
      firstOperand, expr, symbolTable, fileCtx);
  if (!firstOperandType) {
    goto CLEANUP;
  }
  if (!SemaTypeIsIntegral(firstOperandType)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, firstOperand->loc.from.lineNo + 1);
    fprintf(stderr, "expected integral type for bitwise operand, "
            "got "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, firstOperandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &firstOperand->loc);
    operand = operand->sibling;
    goto CLEANUP;
  }
  for (operand = firstOperand->sibling; operand; operand = operand->sibling) {
    SemaType *operandType = SemaTypeFromExpr(
        operand, expr, symbolTable, fileCtx);
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

SemaType *SemaTypeFromTernaryOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *trueNode = expr->firstChild;
  SyntaxAST *condNode = trueNode->sibling;
  SyntaxAST *falseNode = condNode->sibling;
  SemaType *condType = SemaTypeFromExpr(
      condNode, expr, symbolTable, fileCtx);
  if (!condType) {
    goto TERNARY_OP_SKIP_COND;
  }
  SemaType *trueType = SemaTypeFromExpr(trueNode, expr, symbolTable, fileCtx);
  if (!trueType) {
    goto TERNARY_OP_SKIP_TRUE;
  }
  SemaType *falseType = SemaTypeFromExpr(falseNode, expr, symbolTable, fileCtx);
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
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  SemaType *unifiedType = SemaTypeUnification(
      trueType, falseType, &typeInfo->isTypeOwner);
  if (!unifiedType) {
    SemaErrorIfVoid(trueNode, trueType, fileCtx);
    SemaErrorIfVoid(falseNode, falseType, fileCtx);
    goto TERNARY_OP_SKIP_EXPR;
  }
  typeInfo->type = unifiedType;
  return unifiedType;
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

SemaType *SemaTypeFromLogicOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *operand;
  SemaType *operandType;
  for (operand = expr->firstChild; operand; operand = operand->sibling) {
    operandType = SemaTypeFromExpr(operand, expr, symbolTable, fileCtx);
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

SemaType *SemaTypeFromComparisonOp(
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
    SemaType *operandType = SemaTypeFromExpr(
        operand, expr, symbolTable, fileCtx);
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

SemaType *SemaTypeUnification(
    SemaType *type1, SemaType *type2, bool *isTypeOwner) {
  *isTypeOwner = false;
  if ((SemaTypeIsSigned(type1) && SemaTypeIsSigned(type2)) ||
      (SemaTypeIsUnsigned(type1) && SemaTypeIsUnsigned(type2))) {
    SemaType *ret =
        SemaTypeBitwidth(type1) >= SemaTypeBitwidth(type2) ? type1 : type2;
    return ret;
  }
  if ((SemaTypeIsFloat(type1) && SemaTypeIsNumeric(type2)) ||
      (SemaTypeIsNumeric(type1) && SemaTypeIsFloat(type2))) {
    int width1 = SemaTypeBitwidth(type1);
    int width2 = SemaTypeBitwidth(type2);
    int width = width1 > width2 ? width1 : width2;
    SemaType *type = malloc(sizeof(SemaType));
    *isTypeOwner = true;
    switch (width) {
      case 32:
        SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_F32);
        break;
      case 64:
        SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_F64);
        break;
      default:
        assert(false);
    }
    return type;
  }
  if (SemaTypeIsClass(type1) && SemaTypeIsPrimType(type2, SEMA_PRIM_TYPE_NIL)) {
    return type1;
  }
  if (SemaTypeIsClass(type2) && SemaTypeIsPrimType(type1, SEMA_PRIM_TYPE_NIL)) {
    return type2;
  }
  if (SemaTypeEqual(type1, type2)) {
    return type1;
  }
  if (SemaTypeIsPrimType(type1, SEMA_PRIM_TYPE_VOID) ||
      SemaTypeIsPrimType(type2, SEMA_PRIM_TYPE_VOID)) {
    return NULL;
  }
  *isTypeOwner = true;
  SemaType *type = malloc(sizeof(SemaType));
  SemaTypeFromSemaPrimType(type, SEMA_PRIM_TYPE_ANY);
  return type;
}

bool SemaTypeImplicitCast(SemaType *type1, SemaType *type2) {
  bool isTypeOwner;
  SemaType *type = SemaTypeUnification(type1, type2, &isTypeOwner);
  if (!type) {
    return false;
  }
  bool implicitCast = SemaTypeEqual(type2, type);
  if (isTypeOwner) {
    SemaTypeDelete(type);
  }
  return implicitCast;
}

bool SemaErrorIfVoid(SyntaxAST *expr, SemaType *type, SemaFileCtx *fileCtx) {
  if (!SemaTypeIsPrimType(type, SEMA_PRIM_TYPE_VOID)) {
    return true;
  }
  SourceLocation *loc = &expr->loc;
  fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
          " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
  fprintf(stderr, "invalid usage of "SOURCE_COLOR_RED" void type\n");
  SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
  return false;
}

bool SemaTypeIsClass(SemaType *type) {
  return type->kind == SEMA_TYPE_KIND_CLASS;
}

SemaType *SemaTypeFromAlloc(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *countExpr = expr->firstChild;
  SyntaxAST *syntaxBaseType = expr->lastChild;
  SemaType *countType = SemaTypeFromExpr(
      expr->firstChild, expr, symbolTable, fileCtx);
  if (!countType) {
    goto ALLOC_SKIP_COUNT_EXPR;
  }
  if (!SemaTypeIsUnsigned(countType)) {
    SourceLocation *loc = &countExpr->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, "expected expression to be of unsigned type, got type "
            SOURCE_COLOR_RED);
    SemaTypePrint(stderr, countType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
    goto ALLOC_SKIP_TYPE;
  }
  bool isTypeOwner;
  SemaType *baseType = SemaTypeFromSyntaxType(syntaxBaseType, symbolTable,
      fileCtx, expr, &isTypeOwner);
  if (!baseType) {
    goto ALLOC_SKIP_TYPE;
  }
  SemaTypeInfo *syntaxBaseTypeInfo = &syntaxBaseType->semaInfo.typeInfo;
  syntaxBaseTypeInfo->type = baseType;
  syntaxBaseTypeInfo->isTypeOwner = isTypeOwner;
  SemaType *type = SemaTypeIncreaseArrayLevel(baseType);
  SemaTypeInfo *exprTypeInfo = &expr->semaInfo.typeInfo;
  exprTypeInfo->type = type;
  exprTypeInfo->isTypeOwner = true;
  return type;
ALLOC_SKIP_COUNT_EXPR:
  countExpr->semaInfo.skipAnalysis = true;
ALLOC_SKIP_TYPE:
  syntaxBaseType->semaInfo.skipAnalysis = true;
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

SemaType *SemaTypeIncreaseArrayLevel(SemaType *baseType) {
  bool baseTypeIsArray = baseType->kind == SEMA_TYPE_KIND_ARRAY;
  SemaType *type = malloc(sizeof(SemaType));
  type->kind = SEMA_TYPE_KIND_ARRAY;
  type->isBaseTypeOwner = false;
  if (baseTypeIsArray) {
    // To avoid nested array types (an array type whose base type is also an
    // array type), we specially handle array types here
    type->baseType = baseType->baseType;
    type->arrayLevels = baseType->arrayLevels + 1;
  } else {
    type->baseType = baseType;
    type->arrayLevels = 1;
  }
  return type;
}

SemaType *SemaTypeFromShiftOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *leftOperand = expr->firstChild;
  SyntaxAST *rightOperand = leftOperand->sibling;
  assert(!rightOperand->sibling);
  SemaType *leftOperandType = SemaTypeFromExpr(
      leftOperand, expr, symbolTable, fileCtx);
  if (!leftOperandType) {
    goto CLEANUP_LEFT_OPERAND;
  }
  if (!SemaTypeIsIntegral(leftOperandType)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, leftOperand->loc.from.lineNo + 1);
    fprintf(stderr, "expected integral type for shift operand, "
            "got "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, leftOperandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &leftOperand->loc);
    goto CLEANUP_RIGHT_OPERAND;
  }
  SemaType *rightOperandType = SemaTypeFromExpr(
      rightOperand, expr, symbolTable, fileCtx);
  if (!SemaTypeIsUnsigned(rightOperandType)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, rightOperand->loc.from.lineNo + 1);
    fprintf(stderr, "expected unsigned type for shift operand, got "
            SOURCE_COLOR_RED);
    SemaTypePrint(stderr, rightOperandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &rightOperand->loc);
    goto CLEANUP;
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = leftOperandType;
  typeInfo->isTypeOwner = false;
  return leftOperandType;
CLEANUP_LEFT_OPERAND:
  leftOperand->semaInfo.skipAnalysis = true;
CLEANUP_RIGHT_OPERAND:
  rightOperand->semaInfo.skipAnalysis = true;
CLEANUP:
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

bool SemaTypeIsIntegral(SemaType *type) {
  return SemaTypeIsSigned(type) || SemaTypeIsUnsigned(type);
}

SemaType *SemaTypeFromArithOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *leftOperand = expr->firstChild;
  SemaType *leftOperandType = SemaTypeFromExpr(
      leftOperand, expr, symbolTable, fileCtx);
  if (!leftOperandType) {
    goto CLEANUP_LEFT_OPERAND;
  }
  bool hasOneOperand = expr->op == SYNTAX_OP_NEG;
  SyntaxAST *rightOperand;
  SemaType *rightOperandType;
  if (!hasOneOperand) {
    rightOperand = leftOperand->sibling;
    assert(!rightOperand->sibling);
    rightOperandType = SemaTypeFromExpr(
        rightOperand, expr, symbolTable, fileCtx);
    if (!rightOperandType) {
      goto CLEANUP_RIGHT_OPERAND;
    }
  }
  SemaType *exprType;
  bool isTypeOwner;
  if (hasOneOperand) {
    exprType = leftOperandType;
    isTypeOwner = false;
  } else {
    exprType = SemaTypeUnification(
        leftOperandType, rightOperandType, &isTypeOwner);
  }
  if (!SemaTypeIsNumeric(exprType)) {
    if (hasOneOperand) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, leftOperand->loc.from.lineNo + 1);
      fprintf(stderr, "expected numeric type, got "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, leftOperandType);
      fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
      SourceLocationPrint(
          fileCtx->source, 1, SOURCE_COLOR_RED, &leftOperand->loc);
    } else {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, rightOperand->loc.from.lineNo + 1);
      fprintf(stderr, "unification of "SOURCE_COLOR_YELLOW);
      SemaTypePrint(stderr, leftOperandType);
      fprintf(stderr, SOURCE_COLOR_RESET" and "SOURCE_COLOR_BLUE);
      SemaTypePrint(stderr, rightOperandType);
      fprintf(stderr, SOURCE_COLOR_RESET" result in non-numeric type "
          SOURCE_COLOR_RED);
      SemaTypePrint(stderr, exprType);
      fprintf(stderr, SOURCE_COLOR_RESET"\n");
      SourceLocationPrint(
          fileCtx->source, 2, SOURCE_COLOR_YELLOW, &leftOperand->loc,
            SOURCE_COLOR_BLUE, &rightOperand->loc);
      if (isTypeOwner) {
        SemaTypeDelete(exprType);
      }
    }
    goto CLEANUP;
  }
  bool typeCheckSucceeds;
  switch (expr->op) {
    case SYNTAX_OP_MOD:
      typeCheckSucceeds = SemaTypeCheckModOp(
          leftOperand, leftOperandType, rightOperand, rightOperandType,
          fileCtx);
      break;
    case SYNTAX_OP_NEG:
      typeCheckSucceeds = SemaTypeCheckNegOp(
          leftOperand, leftOperandType, fileCtx);
      break;
    default:
      typeCheckSucceeds = true;
      break;
  }
  if (!typeCheckSucceeds) {
    if (isTypeOwner) {
      SemaTypeDelete(exprType);
    }
    goto CLEANUP;
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = exprType;
  typeInfo->isTypeOwner = isTypeOwner;
  return exprType;
CLEANUP_LEFT_OPERAND:
  leftOperand->semaInfo.skipAnalysis = true;
CLEANUP_RIGHT_OPERAND:
  rightOperand->semaInfo.skipAnalysis = true;
CLEANUP:
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

bool SemaTypeIsNumeric(SemaType *type) {
  return SemaTypeIsIntegral(type) || SemaTypeIsFloat(type);
}

bool SemaTypeCheckModOp(
    SyntaxAST *leftOperand, SemaType *leftOperandType, SyntaxAST *rightOperand,
    SemaType *rightOperandType, SemaFileCtx *fileCtx) {
  SyntaxAST *operands[] = {leftOperand, rightOperand};
  SemaType *operandTypes[] = {leftOperandType, rightOperandType};
  for (int i = 0; i < 2; ++i) {
    SyntaxAST *operand = operands[i];
    SemaType *operandType = operandTypes[i];
    if (!SemaTypeIsIntegral(operandType)) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, operand->loc.from.lineNo + 1);
      fprintf(stderr, "expected integral type, got "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, operandType);
      fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
      SourceLocationPrint(
          fileCtx->source, 1, SOURCE_COLOR_RED, &operand->loc);
      return false;
    }
  }
  return true;
}

bool SemaTypeCheckNegOp(
    SyntaxAST *operand, SemaType *operandType, SemaFileCtx *fileCtx) {
  if (SemaTypeIsUnsigned(operandType)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, operand->loc.from.lineNo + 1);
    fprintf(stderr, "expected float or signed type, got "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, operandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &operand->loc);
    return false;
  }
  return true;
}

SemaType *SemaTypeFromCastOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx,
    SemaCastOpCheckFn check, bool retBoolType) {
  SyntaxAST *operand = expr->firstChild;
  SyntaxAST *target = operand->sibling;
  assert(!target->sibling);
  SemaType *operandType = SemaTypeFromExpr(operand, expr, symbolTable, fileCtx);
  if (!operandType) {
    goto CLEANUP_OPERAND;
  }
  bool isTypeOwner;
  SemaType *targetType = SemaTypeFromSyntaxType(
      target, symbolTable, fileCtx, target, &isTypeOwner);
  if (!targetType) {
    goto CLEANUP_TARGET;
  }
  SemaTypeInfo *typeInfo = &target->semaInfo.typeInfo;
  typeInfo->type = targetType;
  typeInfo->isTypeOwner = isTypeOwner;
  if (!check(operand, operandType, target, targetType, fileCtx)) {
    goto CLEANUP;
  }
  typeInfo = &expr->semaInfo.typeInfo;
  if (retBoolType) {
    SemaType *boolType = malloc(sizeof(SemaType));
    SemaTypeFromSemaPrimType(boolType, SEMA_PRIM_TYPE_BOOL);
    typeInfo->type = boolType;
    typeInfo->isTypeOwner = true;
    return boolType;
  }
  typeInfo->type = targetType;
  typeInfo->isTypeOwner = false;
  return targetType;

CLEANUP_OPERAND:
  operand->semaInfo.skipAnalysis = true;
CLEANUP_TARGET:
  target->semaInfo.skipAnalysis = true;
CLEANUP:
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

bool SemaTypeCheckCastIsOp(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx) {
  if (!SemaTypeIsPrimType(operandType, SEMA_PRIM_TYPE_ANY)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, operand->loc.from.lineNo + 1);
    fprintf(stderr, "expected type any, got "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, operandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &operand->loc);
    return false;
  }
  return true;
}

bool SemaTypeCheckCastAsOp(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx) {
  return SemaErrorIfVoid(operand, operandType, fileCtx);
}

bool SemaTypeCheckCastIntoOp(
    SyntaxAST *operand, SemaType *operandType, SyntaxAST *target,
    SemaType *targetType, SemaFileCtx *fileCtx) {
  if (SemaTypeIsPrimType(operandType, SEMA_PRIM_TYPE_ANY) ||
      SemaTypeIsPrimType(targetType, SEMA_PRIM_TYPE_ANY)) {
    return true;
  }
  if (SemaTypeIsPrimType(operandType, SEMA_PRIM_TYPE_NIL)) {
    if (!SemaTypeIsClass(targetType)) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, target->loc.from.lineNo + 1);
      fprintf(stderr, "expected a class type, got "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, targetType);
      fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
      SourceLocationPrint(
          fileCtx->source, 1, SOURCE_COLOR_RED, &target->loc);
      return false;
    }
    return true;
  }
  SemaType *types[] = {operandType, targetType};
  SyntaxAST *nodes[] = {operand, target};
  for (int i = 0; i < 2; ++i) {
    SemaType *type = types[i];
    SyntaxAST *node = nodes[i];
    if (!SemaTypeIsNumeric(type)) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, node->loc.from.lineNo + 1);
      fprintf(stderr, "expected a numeric type, got "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, type);
      fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
      SourceLocationPrint(
          fileCtx->source, 1, SOURCE_COLOR_RED, &node->loc);
      return false;
    }
  }
  return true;
}

void SemaInfoUpdateStage(SemaInfo *info, SemaStage stage) {
  if (info->stage < stage) {
    info->stage = stage;
  }
}

SemaType *SemaTypeFromCall(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *function = expr->firstChild;
  SyntaxAST *args = function->sibling;
  assert(!args->sibling);
  SyntaxAST *arg = args->firstChild;
  SemaType *functionType = SemaTypeFromExpr(
      function, expr, symbolTable, fileCtx);
  if (!functionType) {
    goto CLEANUP_FUNCTION;
  }
  if (!SemaTypeIsFunction(functionType)) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, function->loc.from.lineNo + 1);
    fprintf(stderr, "expected a function type, got "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, functionType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(
        fileCtx->source, 1, SOURCE_COLOR_RED, &function->loc);
    goto CLEANUP_ARGS;
  }
  Vector *paramTypesVec = functionType->paramTypes;
  void **paramTypes = paramTypesVec->arr;
  int numParams = paramTypesVec->size, i;
  for (i = 0; arg && i < numParams; arg = arg->sibling, ++i) {
    SemaType *argType = SemaTypeFromExpr(arg, expr, symbolTable, fileCtx);
    if (!argType) {
      goto CLEANUP_ARGS;
    }
    SemaType *paramType = paramTypes[i];
    if (!SemaTypeImplicitCast(argType, paramType)) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, function->loc.from.lineNo + 1);
      fprintf(stderr, "cannot implicitly cast type "SOURCE_COLOR_RED);
      SemaTypePrint(stderr, argType);
      fprintf(stderr, SOURCE_COLOR_RESET" to type "SOURCE_COLOR_GREEN);
      SemaTypePrint(stderr, paramType);
      fprintf(stderr, SOURCE_COLOR_RESET" expected by the function\n");
      SourceLocationPrint(
          fileCtx->source, 2, SOURCE_COLOR_GREEN, &function->loc,
          SOURCE_COLOR_RED, &arg->loc);
      arg = arg->sibling;
      goto CLEANUP_ARGS;
    }
  }
  int numArgs = i;
  for (SyntaxAST *curArg = arg; curArg; curArg = curArg->sibling) {
    ++numArgs;
  }
  if (numArgs != numParams) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
              " %s:%d: ", fileCtx->path, function->loc.from.lineNo + 1);
    fprintf(stderr, "expected "SOURCE_COLOR_GREEN"%d"SOURCE_COLOR_RESET
        " arguments to the function, got "SOURCE_COLOR_RED"%d"SOURCE_COLOR_RESET
        " arguments instead\n", numParams, numArgs);
    if (numArgs == 0) {
      SourceLocationPrint(
          fileCtx->source, 1, SOURCE_COLOR_GREEN, &function->loc);
    } else {
      SourceLocationPrint(
          fileCtx->source, 2, SOURCE_COLOR_GREEN, &function->loc,
          SOURCE_COLOR_RED, &args->loc);
    }
    goto CLEANUP_ARGS;
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = functionType->retType;
  typeInfo->isTypeOwner = false;
  return functionType->retType;

CLEANUP_FUNCTION:
  function->semaInfo.skipAnalysis = true;
CLEANUP_ARGS:
  for (; arg; arg = arg->sibling) {
    arg->semaInfo.skipAnalysis = true;
  }
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

bool SemaTypeIsFunction(SemaType *type) {
  return type->kind == SEMA_TYPE_KIND_FN;
}

bool SemaTypeCheckClassVarDecls(
    SyntaxAST *varDecls, HashTable *memberTable, HashTable *symbolTable,
    SemaFileCtx *fileCtx) {
  assert(varDecls && varDecls->kind == SYNTAX_AST_KIND_STMTS);
  bool success = true;
  for (SyntaxAST *varDecl = varDecls->firstChild; varDecl;
       varDecl = varDecl->sibling) {
    assert(varDecl && varDecl->kind == SYNTAX_AST_KIND_VAR_DECL);
    SyntaxAST *varInitList = varDecl->firstChild;
    if (!SemaTypeCheckVarInitList(
          varInitList, memberTable, symbolTable, fileCtx)) {
      success = false;
    }
  }
  return success;
}

bool SemaTypeCheckVarInitList(
    SyntaxAST *varInitList, HashTable *memberTable, HashTable *symbolTable,
    SemaFileCtx *fileCtx) {
  assert(varInitList &&
         varInitList->kind == SYNTAX_AST_KIND_VAR_INIT_LIST);
  bool success = true;
  for (SyntaxAST *varInit = varInitList->firstChild; varInit;
       varInit = varInit->sibling) {
    assert(varInit && varInit->kind == SYNTAX_AST_KIND_VAR_INIT);
    SyntaxAST *varType = varInit->firstChild;
    SyntaxAST *initExpr = varType->sibling;
    if (varInit->semaInfo.skipAnalysis) {
      // If analysis for variable initialization should be skipped, then it
      // should be skipped for the initialization expression as well
      SemaSkipAnalysisForSubtree(initExpr);
      continue;
    }
    SemaSymInfo *varInfo = varInit->semaInfo.symInfo;
    SemaTypeInfo *varTypeInfo = &varInfo->typeInfo;
    if (initExpr) {
      SemaType *initExprType = SemaTypeFromExpr(
          initExpr, /*parentExpr=*/NULL, symbolTable, fileCtx);
      if (!initExprType) {
        success = false;
        goto REMOVE_VAR;
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
        // initialization expression can implicit cast to the declared
        // type
        if (!SemaTypeImplicitCast(initExprType, varTypeInfo->type)) {
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
          goto REMOVE_VAR;
        }
      }
    } else {
      assert(varTypeInfo->type->kind != SEMA_TYPE_KIND_PLACEHOLDER);
    }
    continue;
REMOVE_VAR:
    // If type checking did not succeed, then we should remove the variable
    // from its member table. We also set the entry of the SemaSymInfo to NULL
    // so that SemaPopScope can know not to delete the entry from the member
    // table again
    HashTableEntryDelete(memberTable, varInfo->entry);
    varInfo->entry = NULL;
  }
  if (!success) {
  }
  return success;
}

bool SemaTypeCheckMethodDecl(
    SyntaxAST *methodDecl, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  assert(methodDecl && methodDecl->kind == SYNTAX_AST_KIND_METHOD_DECL);
  SyntaxAST *retType = methodDecl->firstChild;
  assert(retType && retType->kind == SYNTAX_AST_KIND_TYPE);
  SyntaxAST *paramList = retType->sibling;
  assert(paramList && paramList->kind == SYNTAX_AST_KIND_PARAM_LIST);

  SemaPushScope(fileCtx);

  bool success = true;
  // Populate the method parameters
  for (SyntaxAST *param = paramList->firstChild; param;
       param = param->sibling) {
    assert(param && param->kind == SYNTAX_AST_KIND_PARAM);
    // "varType" is not provided here because parameter types were already
    // type checked in SemaTypeFromMethodDecl, which is called in
    // SemaPopulateMembers
    
    if (!SemaPopulateVar(
        param, param->string, &param->stringLoc, /*varType=*/NULL,
        /*attr=*/SEMA_ATTR_VAR, /*addToScope=*/true, symbolTable, symbolTable,
        fileCtx)) {
      success = false;
    }
  }

  // Type check the method body
  SyntaxAST *body = paramList->sibling;
  if (body) {
    assert(body->kind == SYNTAX_AST_KIND_STMTS);
    for (SyntaxAST *stmt = body->firstChild; stmt; stmt = stmt->sibling) {
      if (!SemaTypeCheckStmt(stmt, symbolTable, fileCtx)) {
        success = false;
      }
    }
  }

  SemaPopScope(fileCtx, symbolTable);

  return success;
}

bool SemaPopulateVar(
    SyntaxAST *var, char *varName, SourceLocation *varLoc, SyntaxAST *varType,
    SemaAttr attr, bool addToScope, HashTable *symbolTable,
    HashTable *memberTable, SemaFileCtx *fileCtx) {
  // Check if variable identifier is unique
  HashTableEntry *entry = HashTableEntryRetrieve(
      memberTable, varName);
  if (entry) {
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, varLoc->from.lineNo + 1);
    fprintf(stderr, "reuse of identifier "SOURCE_COLOR_RED"%s"
            SOURCE_COLOR_RESET" for variable declaration\n",
            varName);
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, varLoc);
    goto CLEANUP;
  }

  // Check if variable type is valid
  SemaSymInfo *varDeclSymInfo;
  if (varType) {
    assert(varType->kind == SYNTAX_AST_KIND_TYPE);
    varDeclSymInfo = malloc(sizeof(SemaSymInfo));
    var->semaInfo.symInfo = varDeclSymInfo;
    SemaTypeInfo *varDeclTypeInfo = &varDeclSymInfo->typeInfo;
    SemaTypeInfo *varTypeInfo = &varType->semaInfo.typeInfo;
    SemaType *semaType = SemaTypeFromSyntaxType(
        varType, symbolTable, fileCtx, var,
        &varTypeInfo->isTypeOwner);
    if (!semaType) {
      free(varDeclSymInfo);
      goto CLEANUP;
    }

    varDeclTypeInfo->type = varTypeInfo->type = semaType;
    varDeclTypeInfo->isTypeOwner = false;
    // set-to-a-later-stage: most SYNTAX_AST_KIND_TYPE nodes only have
    // their SemaType avaiable after the type checking stage, but there are rare
    // exceptions such as SemaPopulateMembers, which populates type information
    // for class variables before the type checking stage. SemaDeleteASTSemaInfo
    // relies on the stage to determine whether the SemaType is ready, so we
    // modify the stage in this particular instance to the type check stage
    varType->semaInfo.stage = SEMA_STAGE_TYPE_CHECK;
  } else {
    // If "varType" is not provided, then type checking was already done at
    // a previous stage. Retrive the previously-computed type here
    varDeclSymInfo = var->semaInfo.symInfo;
  }

  HashTableEntryAdd(memberTable, varName, varDeclSymInfo);
  entry = HashTableEntryRetrieve(memberTable, varName);
  assert(entry);
  varDeclSymInfo->entry = entry;
  varDeclSymInfo->attr = attr;
  if (addToScope) {
    Vector *scopes = fileCtx->scopes;
    varDeclSymInfo->nextInScope = scopes->arr[scopes->size - 1];
    VectorAdd(scopes, varDeclSymInfo);
  }
  return true;

CLEANUP:
  var->semaInfo.skipAnalysis = true;
  if (varType) {
    varType->semaInfo.skipAnalysis = true;
  }
  return false;
}

void SemaPushScope(SemaFileCtx *fileCtx) {
  VectorAdd(fileCtx->scopes, NULL);
}

void SemaPopScope(SemaFileCtx *fileCtx, HashTable *symbolTable) {
  Vector *scopes = fileCtx->scopes;
  for (SemaSymInfo *symInfo = scopes->arr[--scopes->size]; symInfo;
       symInfo = symInfo->nextInScope) {
    // Note that it is possible for a variable to already be removed from the
    // table because of type check failures, so we check that the entry is
    // still in the table before removing it
    HashTableEntry *entry = symInfo->entry;
    if (entry) {
      HashTableEntryDelete(symbolTable, entry);
    }
  }
}

bool SemaTypeCheckStmt(
    SyntaxAST *stmt, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  assert(stmt);
  switch (stmt->kind) {
    case SYNTAX_AST_KIND_VAR_DECL:
      return SemaTypeCheckVarDeclStmt(stmt, symbolTable, fileCtx);
    default:
      printf("Stmt kind: %d\n", stmt->kind);
      assert(false);
  }
}

bool SemaTypeCheckVarDeclStmt(
    SyntaxAST *stmt, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  bool success = true;
  if (!SemaPopulateVarDecl(
        stmt, symbolTable, symbolTable, /*addToScope=*/true, fileCtx)) {
    success = false;
  }

  SyntaxAST *varInitList = stmt->firstChild;
  if (!SemaTypeCheckVarInitList(
        varInitList, symbolTable, symbolTable, fileCtx)) {
    success = false;
  }

  return success;
}

void SemaSkipAnalysisForSubtree(SyntaxAST *node) {
  if (!node) {
    return;
  }
  node->semaInfo.skipAnalysis = true;
  SemaSkipAnalysisForSubforest(node->firstChild);
}

void SemaSkipAnalysisForSubforest(SyntaxAST *node) {
  if (!node) {
    return;
  }
  node->semaInfo.skipAnalysis = true;
  SemaSkipAnalysisForSubforest(node->firstChild);
  SemaSkipAnalysisForSubforest(node->sibling);
}

SemaType *SemaTypeFromThisLiteral(
    SyntaxAST *thisNode, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SemaSymInfo *methodSymInfo = fileCtx->methodSymInfo;
  if (!methodSymInfo ||
      (methodSymInfo->attr != SEMA_ATTR_CTOR &&
       methodSymInfo->attr != SEMA_ATTR_METHOD)) {
    SourceLocation *loc = &thisNode->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, "cannot refer to "SOURCE_COLOR_RED"this"
            SOURCE_COLOR_RESET" outside of constructor or method bodies\n");
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
    goto CLEANUP;
  }
  SemaType *thisType = fileCtx->classType;
  assert(thisType);
  SemaTypeInfo *thisInfo = &thisNode->semaInfo.typeInfo;
  thisInfo->type = thisType;
  thisInfo->isTypeOwner = false;
  return thisType;
CLEANUP:
  thisNode->semaInfo.skipAnalysis = true;
  return NULL;
}

char *SemaGetNamespaceIdentifier(SyntaxAST *importDecl, SourceLocation *loc) {
  if (importDecl->import.isWildcard) {
    return NULL;
  }
  char *namespace = importDecl->import.namespace;
  if (!namespace) {
    // If the import declaration does not specify a namespace and is not
    // a wildcard import, then the default namespace is the last
    // identifier in the module path
    SyntaxAST *modulePath = importDecl->firstChild;
    assert(modulePath && modulePath->kind == SYNTAX_AST_KIND_MODULE_PATH);
    SyntaxAST *identifier = modulePath->lastChild;
    assert(identifier && identifier->kind == SYNTAX_AST_KIND_IDENTIFIER);
    namespace = identifier->string;
    if (loc) {
      *loc = identifier->loc;
    }
  } else {
    if (loc) {
      *loc = importDecl->import.extLoc;
    }
  }
  return namespace;
}

bool SemaPopulateVarDecl(
    SyntaxAST *varDecl, HashTable *symbolTable, HashTable *memberTable,
    bool addToScope, SemaFileCtx *fileCtx) {
  assert(varDecl && varDecl->kind == SYNTAX_AST_KIND_VAR_DECL);

  bool success = true;
  unsigned varDeclModifiers = varDecl->varDeclModifiers;
  SemaAttr attr;
  if (varDeclModifiers & SYNTAX_VAR_DECL_STATIC) {
    if (varDeclModifiers & SYNTAX_VAR_DECL_CONST) {
      attr = SEMA_ATTR_STATIC_CONST;
    } else {
      attr = SEMA_ATTR_STATIC_VAR;
    }
  } else {
    if (varDeclModifiers & SYNTAX_VAR_DECL_CONST) {
      attr = SEMA_ATTR_CONST;
    } else {
      attr = SEMA_ATTR_VAR;
    }
  }

  SyntaxAST *varInitList = varDecl->firstChild;
  assert(varInitList &&
         varInitList->kind == SYNTAX_AST_KIND_VAR_INIT_LIST);
  for (SyntaxAST *varInit = varInitList->firstChild; varInit;
       varInit = varInit->sibling) {
    assert(varInit && varInit->kind == SYNTAX_AST_KIND_VAR_INIT);

    if (!SemaPopulateVar(varInit, varInit->string, &varInit->stringLoc,
          varInit->firstChild, attr, addToScope, symbolTable, memberTable,
          fileCtx)) {
      success = false;
    }
  }
  return success;
}

SemaType *SemaTypeFromMemberAccess(
    SyntaxAST *memberAccess, SyntaxAST *parentExpr, HashTable *symbolTable,
    SemaFileCtx *fileCtx) {
  SyntaxAST *operand = memberAccess->firstChild;
  char *identifier = memberAccess->string;
  SemaType *operandType = SemaTypeFromExpr(
      operand, memberAccess, symbolTable, fileCtx);
  if (!operandType) {
    goto CLEANUP;
  }
  if (operandType->kind != SEMA_TYPE_KIND_CLASS &&
      operandType->kind != SEMA_TYPE_KIND_NAMESPACE) {
    SourceLocation *loc = &operand->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, "expected a namespace, class or instance of class, got "
        "type "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, operandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
    goto CLEANUP;
  }
  HashTableEntry *memberEntry =
      HashTableEntryRetrieve(operandType->memberTable, identifier);
  if (!memberEntry) {
    SourceLocation *loc = &memberAccess->stringLoc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET" is not a member of "
        "type "SOURCE_COLOR_GREEN, identifier);
    SemaTypePrint(stderr, operandType);
    fprintf(stderr, SOURCE_COLOR_RESET"\n");
    SourceLocationPrint(
        fileCtx->source, 2, SOURCE_COLOR_GREEN, &operand->loc, SOURCE_COLOR_RED,
        loc);
    goto CLEANUP;
  }
  SemaSymInfo *memberSymInfo = memberEntry->value;
  SemaTypeInfo *memberTypeInfo = &memberSymInfo->typeInfo;
  SemaAttr memberAttr = memberSymInfo->attr;
  SemaType *memberType = memberTypeInfo->type;

  if (!SemaCheckErrorForUncapturableValue(memberAccess, parentExpr, fileCtx)) {
    goto CLEANUP;
  }

  bool operandIsCapturable = SemaValueIsCapturable(operand);
  if (operandIsCapturable) {
    // Operand is an instance of a class, so it can access any method or
    // non-static variable of the class
    if (memberAttr != SEMA_ATTR_METHOD && memberAttr != SEMA_ATTR_VAR &&
        memberAttr != SEMA_ATTR_CONST) {
        SourceLocation *loc = &memberAccess->stringLoc;
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
        fprintf(stderr, SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET" is not a "
            "method or non-static variable of type "SOURCE_COLOR_GREEN,
            identifier);
        SemaTypePrint(stderr, operandType);
        fprintf(stderr, SOURCE_COLOR_RESET"\n");
        SourceLocationPrint(
            fileCtx->source, 2, SOURCE_COLOR_GREEN, &operand->loc,
            SOURCE_COLOR_RED, loc);
        goto CLEANUP;
    }
  } else if (operandType->kind == SEMA_TYPE_KIND_CLASS) {
    // Operand is a class, so it can access anything other than non-static
    // variables of the class
    if (memberAttr == SEMA_ATTR_VAR || memberAttr == SEMA_ATTR_CONST) {
        SourceLocation *loc = &memberAccess->stringLoc;
        fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
        fprintf(stderr, SOURCE_COLOR_RED"%s"SOURCE_COLOR_RESET" is a "
            "non-static variable of type "SOURCE_COLOR_GREEN,
            identifier);
        SemaTypePrint(stderr, operandType);
        fprintf(stderr, SOURCE_COLOR_RESET"\n");
        SourceLocationPrint(
            fileCtx->source, 2, SOURCE_COLOR_GREEN, &operand->loc,
            SOURCE_COLOR_RED, loc);
        goto CLEANUP;
    }
  } else {
    assert(operandType->kind == SEMA_TYPE_KIND_NAMESPACE);
  }

  SemaTypeInfo *typeInfo = &memberAccess->semaInfo.typeInfo;
  if (operandIsCapturable && memberAttr == SEMA_ATTR_METHOD) {
    // If we are refering to a method via an instance of the class, the type
    // of the member access should be a new function type without the first
    // parameter (implicit this)
    assert(memberType->kind == SEMA_TYPE_KIND_FN);
    SemaType *type = malloc(sizeof(SemaType));
    type->kind = SEMA_TYPE_KIND_FN;
    type->retType = memberType->retType;
    Vector *memberParams = memberType->paramTypes;
    int numParams = memberParams->size;
    Vector *params = VectorNew();
    type->paramTypes = params;
    for (int i = 1; i < numParams; ++i) {
      VectorAdd(params, memberParams->arr[i]);
    }
    typeInfo->type = type;
    typeInfo->isTypeOwner = true;
  } else {
    typeInfo->type = memberType;
    typeInfo->isTypeOwner = false;
  }
  return typeInfo->type;

CLEANUP:
  memberAccess->semaInfo.skipAnalysis = true;
  return NULL;
}

bool SemaValueIsCapturable(SyntaxAST *value) {
  // A value is capturable if and only if it is not a class or a namespace.
  // - A namespace can only be referenced by an identifier
  // - A class can be referenced in two ways:
  //   - By the class identifier itself (if it is in the current scope or
  //     imported from a wildcard import)
  //   - Referenced indirectly via a namespace
  // Therefore, we only have to identify the cases where the value is an
  // identifier or a member access. In all other cases, the value would be
  // capturable
  switch (value->kind) {
    case SYNTAX_AST_KIND_IDENTIFIER: {
      SemaType *type = value->semaInfo.typeInfo.type;
      switch (type->kind) {
        case SEMA_TYPE_KIND_CLASS:
        case SEMA_TYPE_KIND_NAMESPACE:
          return false;
        case SEMA_TYPE_KIND_ARRAY:
        case SEMA_TYPE_KIND_FN:
        case SEMA_TYPE_KIND_PRIM_TYPE:
          return true;
        default:
          printf("Type kind: %d\n", type->kind);
          assert(false);
      }
    } case SYNTAX_AST_KIND_MEMBER_ACCESS: {
      SyntaxAST *operand = value->firstChild;
      assert(operand);
      return !SemaNodeIsNamespace(operand);
    } default: {
      return true;
    }
  }
}

bool SemaNodeIsNamespace(SyntaxAST *node) {
  return node->kind == SYNTAX_AST_KIND_IDENTIFIER &&
         node->semaInfo.typeInfo.type->kind == SEMA_TYPE_KIND_NAMESPACE;
}

bool SemaCheckErrorForUncapturableValue(
    SyntaxAST *value, SyntaxAST *parentExpr, SemaFileCtx *fileCtx) {
  if (SemaValueIsCapturable(value) ||
      (parentExpr && parentExpr->kind == SYNTAX_AST_KIND_MEMBER_ACCESS)) {
    return true;
  }
  SemaPrintErrorForUncapturableValue(value, fileCtx);
  return false;
}

void SemaPrintErrorForUncapturableValue(
    SyntaxAST *value, SemaFileCtx *fileCtx) {
  SourceLocation *loc = &value->loc;
  fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
          " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
  fprintf(stderr, SOURCE_COLOR_RED"value"SOURCE_COLOR_RESET
          " is not used in an access operator\n");
  SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
}

SemaType *SemaTypeFromIndexOp(
    SyntaxAST *expr, HashTable *symbolTable, SemaFileCtx *fileCtx) {
  SyntaxAST *operand = expr->firstChild;
  SyntaxAST *index = operand->sibling;
  assert(!index->sibling);

  SemaType *operandType = SemaTypeFromExpr(operand, expr, symbolTable, fileCtx);
  if (!operandType) {
    goto OPERAND_CLEANUP;
  }
  if (operandType->kind != SEMA_TYPE_KIND_ARRAY) {
    SourceLocation *loc = &operand->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, "expected an array type, got type "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, operandType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
    goto INDEX_CLEANUP;
  }
  SemaType *indexType = SemaTypeFromExpr(index, expr, symbolTable, fileCtx);
  if (!indexType) {
    goto INDEX_CLEANUP;
  }
  if (!SemaTypeIsUnsigned(indexType)) {
    SourceLocation *loc = &index->loc;
    fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
            " %s:%d: ", fileCtx->path, loc->from.lineNo + 1);
    fprintf(stderr, "expected an unsigned type, got type "SOURCE_COLOR_RED);
    SemaTypePrint(stderr, indexType);
    fprintf(stderr, SOURCE_COLOR_RESET" instead\n");
    SourceLocationPrint(fileCtx->source, 1, SOURCE_COLOR_RED, loc);
    goto CLEANUP;
  }
  SemaTypeInfo *typeInfo = &expr->semaInfo.typeInfo;
  typeInfo->type = SemaTypeDecreaseArrayLevel(
      operandType, &typeInfo->isTypeOwner);
  return typeInfo->type;

OPERAND_CLEANUP:
  operand->semaInfo.skipAnalysis = true;
INDEX_CLEANUP:
  index->semaInfo.skipAnalysis = true;
CLEANUP:
  expr->semaInfo.skipAnalysis = true;
  return NULL;
}

SemaType *SemaTypeDecreaseArrayLevel(SemaType *arrayType, bool *isTypeOwner) {
  if (arrayType->arrayLevels == 1) {
    *isTypeOwner = false;
    return arrayType->baseType;
  }
  *isTypeOwner = true;
  SemaType *baseType = malloc(sizeof(SemaType));
  baseType->kind = SEMA_TYPE_KIND_ARRAY;
  baseType->baseType = arrayType->baseType;
  baseType->isBaseTypeOwner = false;
  baseType->arrayLevels = arrayType->arrayLevels - 1;
  return baseType;
}
