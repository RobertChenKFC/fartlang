#include "fir/irgen/irgen.h"
#include "sema/sema.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

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
// Same as IrgenFromBody, but for external C functions
IrBasicBlock* IrgenFromCFunc(IrBasicBlock *lastBlock, SyntaxAST *methodDecl);
// Same as IrgenFromBody, but for terms
IrBasicBlock* IrgenFromTerm(IrBasicBlock *lastBlock, SyntaxAST *term);
// Same as IrgenFromBody, but for member access
IrBasicBlock* IrgenFromMemberAccess(
    IrBasicBlock* lastBlock, SyntaxAST *memberAccess);

IrProgram *IrgenFromFile(const char *path) {
  IrProgram *program = NULL;
  SemaCtx semaCtx;
  if (!SemaCheck(&semaCtx, path, /*checkForMainFunc=*/true)) {
    goto SEMA_CTX_CLEANUP;
  }

  program = IrProgramNew();
  Vector *fileCtxs = semaCtx.fileCtxs;
  for (int i = 0; i < fileCtxs->size; ++i) {
    SemaFileCtx *fileCtx = fileCtxs->arr[i];
    IrgenFromSemaFileCtx(program, fileCtx);
  }

  SyntaxAST *mainFnNode = SemaCtxGetMainFn(&semaCtx);
  assert(mainFnNode);
  IrFunc *mainFn = mainFnNode->irgenInfo.func;
  IrProgramSetEntryFunc(program, mainFn);

SEMA_CTX_CLEANUP:
  SemaCtxDelete(&semaCtx);
  return program;
}

void IrgenFromSemaFileCtx(IrProgram *program, SemaFileCtx *fileCtx) {
  IrModule *module = IrModuleAdd(program);

  SyntaxAST *moduleNode = fileCtx->node;
  assert(moduleNode && moduleNode->kind == SYNTAX_AST_KIND_MODULE);
  SyntaxAST *classDecls = moduleNode->lastChild;
  assert(classDecls && classDecls->kind == SYNTAX_AST_KIND_CLASS_DECLS);
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
  Vector *params = IrFuncGetParams(func);
  for (int i = 0; i < paramTypes->size; ++i) {
    SemaType *paramSemaType = paramTypes->arr[i];
    IrType paramType = IrgenIrTypeFromSemaType(paramSemaType);
    IrVar *param = IrFuncAddVar(func, paramType);
    VectorAdd(params, param);
  }

  SyntaxAST *methodBody = methodDecl->lastChild;
  IrBasicBlock *lastBlock = IrBasicBlockAdd(func);
  IrFuncSetEntryBlock(func, lastBlock);
  if (methodBody && methodBody->kind == SYNTAX_AST_KIND_STMTS) {
    // Method has a body, generate the IR for the method body
    IrgenFromBody(lastBlock, methodBody);
  } else {
    // Method does not have a body, so this refers to an external C function.
    // Generate the IR to call the external C function
    IrgenFromCFunc(lastBlock, methodDecl);
  }
}

IrType IrgenIrTypeFromSemaType(SemaType *type) {
  switch (type->kind) {
    case SEMA_TYPE_KIND_ARRAY:
      return IR_TYPE_ADDR;
    case SEMA_TYPE_KIND_FN:
      return IR_TYPE_FN;
    case SEMA_TYPE_KIND_PRIM_TYPE:
      switch (type->primType) {
        case SEMA_PRIM_TYPE_U64:
          return IR_TYPE_U64;
        case SEMA_PRIM_TYPE_U32:
          return IR_TYPE_U32;
        case SEMA_PRIM_TYPE_I32:
          return IR_TYPE_I32;
        default:
          printf("Sema prim type: %d\n", type->primType);
          assert(false);
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
      return IrgenFromExpr(lastBlock, stmt->firstChild);
    default:
      printf("Stmt kind: %d\n", stmt->kind);
      assert(false);
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
    IrType varType = IrgenIrTypeFromSemaType(typeInfo->type);
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
  if (expr->kind == SYNTAX_AST_KIND_MEMBER_ACCESS) {
    return IrgenFromMemberAccess(lastBlock, expr);
  }
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
  IrVar *func = funcNode->irgenInfo.var;

  int numArgs = 0;
  SyntaxAST *argsNode = funcNode->sibling;
  for (SyntaxAST *argNode = argsNode->firstChild; argNode;
       argNode = argNode->sibling, ++numArgs);
  IrVar **args = malloc(sizeof(IrVar*) * numArgs);
  int argIdx = 0;
  for (SyntaxAST *argNode = argsNode->firstChild; argNode;
       argNode = argNode->sibling, ++argIdx) {
    lastBlock = IrgenFromExpr(lastBlock, argNode);
    args[argIdx] = argNode->irgenInfo.var;
  }

  SemaType *retType = expr->semaInfo.typeInfo.type;
  IrVar *ret = NULL;
  if (!SemaTypeIsPrimType(retType, SEMA_PRIM_TYPE_VOID)) {
    ret = IrFuncAddVar(lastBlock->func, IrgenIrTypeFromSemaType(retType));
  }
  IrOpAppend(lastBlock, IrOpNewCall(ret, func, numArgs, args));
  expr->irgenInfo.var = ret;
  return lastBlock;
}

IrBasicBlock* IrgenFromCFunc(IrBasicBlock *lastBlock, SyntaxAST *methodDecl) {
  IrFunc *func = lastBlock->func;
  Vector *params = func->params;
  int numArgs = params->size;
  IrVar **args = malloc(sizeof(IrVar*) * numArgs);
  for (int i = 0; i < numArgs; ++i) {
    args[i] = params->arr[i];
  }

  char *symbol = methodDecl->method.name;
  IrVar *cfunc = IrFuncAddVar(func, IR_TYPE_CFN);
  IrOpAppend(lastBlock, IrOpNewConstCfn(cfunc, strdup(symbol)));
  SemaType *retType = methodDecl->semaInfo.symInfo->typeInfo.type->retType;
  IrVar *ret = NULL;
  if (!SemaTypeIsPrimType(retType, SEMA_PRIM_TYPE_VOID)) {
    ret = IrFuncAddVar(func, IrgenIrTypeFromSemaType(retType));
  }
  IrOpAppend(lastBlock, IrOpNewCall(ret, cfunc, numArgs, args));
  return lastBlock;
}

IrBasicBlock* IrgenFromTerm(IrBasicBlock *lastBlock, SyntaxAST *term) {
  if (term->kind == SYNTAX_AST_KIND_IDENTIFIER &&
      !SemaValueIsCapturable(term)) {
    // This term is a class or namespace, don't generate any code for it
    return lastBlock;
  }

  IrType type = IrgenIrTypeFromSemaType(term->semaInfo.typeInfo.type);
  IrVar *var = IrFuncAddVar(lastBlock->func, type);
  term->irgenInfo.var = var;
  IrOp *op;
  switch (term->kind) {
    case SYNTAX_AST_KIND_LITERAL: {
      switch (term->literal.type) {
        case SYNTAX_TYPE_U64:
        case SYNTAX_TYPE_I32: {
          op = IrOpNewConst(var, term->literal.intVal);
          break;
        } case SYNTAX_TYPE_STR: {
          char *str = term->literal.strVal;
          op = IrOpNewConstAddr(var, (uint8_t*)strdup(str), strlen(str) + 1);
          break;
        } default: {
          // TODO: handle other literal types
          printf("Literal type: %d\n", term->literal.type);
          assert(false);
        }
      }
      break;
    } case SYNTAX_AST_KIND_IDENTIFIER: {
      // TODO: handle variables
      assert(false);
    } default: {
      printf("Term kind: %d\n", term->kind);
      assert(false);
    }
  }
  IrOpAppend(lastBlock, op);
  return lastBlock;
}

IrBasicBlock* IrgenFromMemberAccess(
    IrBasicBlock* lastBlock, SyntaxAST *memberAccess) {
  SyntaxAST *operand = memberAccess->firstChild;
  lastBlock = IrgenFromExpr(lastBlock, operand);
  SyntaxAST *member = memberAccess->semaInfo.member;
  switch (member->kind) {
    case SYNTAX_AST_KIND_VAR_DECL: {
      // TODO: handle variables
      assert(false);
    } case SYNTAX_AST_KIND_METHOD_DECL: {
      IrFunc *parentFunc = lastBlock->func;
      IrVar *funcVar = IrFuncAddVar(parentFunc, IR_TYPE_FN);
      IrFunc *func = IrgenGetFuncForMethodDecl(parentFunc->module, member);
      IrOpAppend(lastBlock, IrOpNewConstFn(funcVar, func));
      memberAccess->irgenInfo.var = funcVar;
      break;
    } default: {
      printf("Member kind: %d\n", member->kind);
      assert(false);
    }
  }
  return lastBlock;
}
