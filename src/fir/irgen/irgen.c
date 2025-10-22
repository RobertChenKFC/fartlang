#include "fir/irgen/irgen.h"
#include "sema/sema.h"
#include <assert.h>

// Generate an IR module from the AST node in "fileCtx" and add it to "program"
void IrgenFromSemaFileCtx(IrProgram *program, SemaFileCtx *fileCtx);
// Generate an IR function from the AST node "methodDecl" and add it to "module"
void IrgenFromMethodDecl(IrModule *module, SyntaxAST *methodDecl);
// Convert a SemaType "semaType" to the corresponding IR type
IrType IrgenIrTypeFromSemaType(SemaType *type);
// Generate the IR operations from the AST node "body" and make "lastBlock"
// continue execution to them. Returns the basic block that contains the
// last IR operation generated from "body"
IrBasicBlock* IrgenFromBody(IrBasicBlock *lastBlock, SyntaxAST *methodBody);
// Same as IrgenFromBody, but for stmt
IrBasicBlock* IrgenFromStmt(IrBasicBlock *lastBlock, SyntaxAST *stmt);
// Same as IrgenFromBody, but for variable declaration
IrBasicBlock* IrgenFromVarDecl(IrBasicBlock *lastBlock, SyntaxAST *varDecl);
// Same as IrgenFromBody, but for expression
IrBasicBlock* IrgenFromExpr(IrBasicBlock *lastBlock, SyntaxAST *expr);
// If the IR function for the "methodDecl" already exists, return it, otherwise
// create the IR function, add it to "module", assign it to "methodDecl" and
// return it
IrFunc* IrgenGetFuncForMethodDecl(IrModule *module, SyntaxAST *methodDecl);
// Same as IrgenFromBody, but for call expressions
IrBasicBlock* IrgenFromCall(IrBasicBlock *lastBlock, SyntaxAST *expr);

IrProgram *IrgenFromFile(const char *path) {
  SemaCtx semaCtx;
  if (!SemaCheck(&semaCtx, path, /*checkForMainFunc=*/true)) {
    goto SEMA_CTX_CLEANUP;
  }

  IrProgram *program = IrProgramNew();
  Vector *fileCtxs = semaCtx.fileCtxs;
  for (int i = 0; i < fileCtxs->size; ++i) {
    SemaFileCtx *fileCtx = fileCtxs->arr[i];
    IrgenFromSemaFileCtx(program, fileCtx);
  }

  SyntaxAST *mainFnNode = SemaCtxGetMainFn(&semaCtx);
  assert(mainFnNode);
  IrFunc *mainFn = mainFnNode->irgenInfo.func;
  IrProgramSetEntryFunc(program, mainFn);

  return program;

SEMA_CTX_CLEANUP:
  SemaCtxDelete(&semaCtx);
  return NULL;
}

void IrgenFromSemaFileCtx(IrProgram *program, SemaFileCtx *fileCtx) {
  IrModule *module = IrModuleAdd(program);

  SyntaxAST *moduleNode = fileCtx->node;
  assert(moduleNode && moduleNode->kind == SYNTAX_AST_KIND_MODULE);
  SyntaxAST *classDecls = moduleNode->lastChild;
  assert(classDecls && classDecls->kind == SYTNAX_AST_KIND_CLASS_DECLS);
  for (SyntaxAST *classDecl = classDecls->firstChild; classDecl;
       classDecl = classDecl->sibling) {
    assert(classDecl && classDecl->kind == SYNTAX_AST_KIND_CLASS_DECL);
    SyntaxAST *varDeclStmts = classDecl->firstChild;
    assert(varDeclStmts && varDeclStmts->kind == SYNTAX_AST_KIND_STMTS);
    // TODO: complete this implementation
    assert(varDeclStmts->firstChild == NULL);

    SyntaxAST *methodDecls = varDeclStmts->sibling;
    assert(methodDecls && methodDecls->kind == SYNTAX_AST_KIND_METHOD_DECLS);
    for (SyntaxAST *methodDecl = methodDecls->firstChild; methodDecl;
         methodDecl = methodDecl->sibling) {
      IrgenFromMethodDecl(module, methodDecl);
    }
  }
}

void IrgenFromMethodDecl(IrModule *module, SyntaxAST *methodDecl) {
  assert(methodDecl && methodDecl->kind == SYNTAX_AST_KIND_METHOD_DECL);

  IrFunc *func = IrgenGetFuncForMethodDecl(module, methodDecl);

  SemaType *methodType = methodDecl->semaInfo.symInfo->typeInfo.type;
  assert(methodType->kind == SEMA_TYPE_KIND_FN);
  Vector *paramTypes = methodType->paramTypes;
  Vector *params = VectorNew();
  for (int i = 0; i < paramTypes->size; ++i) {
    SemaType *paramSemaType = paramTypes->arr[i];
    IrType paramType = IrgenIrTypeFromSemaType(paramSemaType);
    IrVar *param = IrFuncAddVar(func, paramType);
    VectorAdd(params, param);
  }
  IrFuncSetParams(func, params);

  SyntaxAST *methodBody = methodDecl->lastChild;
  if (methodBody && methodBody->kind == SYNTAX_AST_KIND_BODY) {
    IrBasicBlock *lastBlock = IrBasicBlockAdd(func);
    IrFuncSetEntryBlock(func, lastBlock);
    IrgenFromBody(lastBlock, methodBody);
  }
}

IrType IrgenIrTypeFromSemaType(SemaType *type) {
  switch (type->kind) {
    case SEMA_TYPE_KIND_ARRAY:
      return IR_TYPE_ADDR;
    case SEMA_TYPE_KIND_PRIM_TYPE:
      switch (type->primType) {
        case SEMA_TYPE_KIND_U64:
          return IR_TYPE_U64;
        case SEMA_TYPE_KIND_U32:
          return IR_TYPE_U32;
        case SEMA_TYPE_KIND_I32:
          return IR_TYPE_I32;
        default:
          printf("Sema prim type: %d\n", type->primType);
      }
    default:
      printf("Sema type kind: %d\n", type->kind);
      assert(false);
  }
}

IrBasicBlock* IrgenFromBody(IrBasicBlock *lastBlock, SyntaxAST *methodBody) {
  for (SyntaxAST *stmt = methodBody->firstChild; stmt; stmt = stmt->sibling) {
    lastBlock = IrgenFromStmt(lastBlock, stmt);
  }
  return lastBlock;
}

IrBasicBlock* IrgenFromStmt(IrBasicBlock *lastBlock, SyntaxAST *stmt) {
  switch (stmt->kind) {
    case SYNTAX_AST_KIND_VAR_DECL:
      return IrgenFromVarDecl(lastBlock, stmt);
    case SYNTAX_AST_KIND_EXPR_STMT:
      return IrgenFromExprStmt(lastBlock, stmt);
  }
}

IrBasicBlock* IrgenFromVarDecl(IrBasicBlock *lastBlock, SyntaxAST *varDecl) {
  // TODO: support static variables
  assert((varDecl->varDeclModifiers & SYNTAX_VAR_DECL_STATIC) == 0);

  IrFunc *func = lastBlock->func;
  SyntaxAST *varInitList = varDecl->firstChild;
  assert(varInitList && varInitList->kind == SYNTAX_AST_KIND_VAR_INIT_LIST);
  for (SyntaxAST *varInit = varInitList->firstChild; varInit;
       varInit = varInit->sibling) {
    SemaSymInfo *symInfo = varInit->semaInfo.symInfo;
    SemaTypeInfo *typeInfo = &symInfo->typeInfo;
    SemaType *varType = IrTypeFromSemaType(typeInfo->type);
    IrVar *var = IrFuncAddVar(func, varType);

    assert(varInit && varInit->kind == SYNTAX_AST_KIND_VAR_INIT);
    SyntaxAST *varInitExpr = varInit->lastChild;
    if (varInitExpr) {
      lastBlock = IrgenFromExpr(lastBlock, varInitExpr);
      IrOpAppend(lastBlock, IrOpNewCopy(var, varInitExpr->irgenInfo.var));
    } else {
      IrOpAppend(lastBlock, IrOpNewConst(var, 0));
    }
  }
  return lastBlock;
}

IrBasicBlock* IrgenFromExpr(IrBasicBlock *lastBlock, SyntaxAST *expr) {
  // TODO: support member access
  assert(expr->kind != SYNTAX_AST_KIND_MEMBER_ACCESS);
  // TODO: support assignment
  assert(expr->kind != SYNTAX_AST_KIND_ASSIGN);

  if (expr->kind != SYNTAX_AST_KIND_OP) {
    return IrgenFromTerm(lastBlock, expr);
  }
  switch (expr->op) {
    case SYNTAX_OP_CALL:
      return IrgenFromCall(lastBlock, expr);
    default:
      printf("Op: %d\n", expr->op);
      assert(false);
  }
}

void IrgenInfoInit(IrgenInfo *info) {
  memset(info, 0, sizeof(IrgenInfo));
}

IrFunc* IrgenGetFuncForMethodDecl(IrModule *module, SyntaxAST *methodDecl) {
  IrgenInfo *irgenInfo = &methodDecl->irgenInfo;
  IrFunc *func = irgenInfo->func;
  if (!func) {
    func = irgenInfo->func = IrFuncAdd(module);
  }
  return func;
}

IrBasicBlock* IrgenFromCall(IrBasicBlock *lastBlock, SyntaxAST *expr) {
  SyntaxAST *funcNode = expr->firstChild; 
  lastBlock = IrgenFromExpr(lastBlock, funcNode);
  IrFunc *func = funcNode->irgenInfo.func;


}
