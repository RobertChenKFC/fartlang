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
// Same as IrgenFromBody, but for if statement
IrBasicBlock* IrgenFromIfStmt(IrBasicBlock* lastBlock, SyntaxAST *ifStmt);
// Same as IrgenFromBody, but for return statement
IrBasicBlock* IrgenFromRetStmt(IrBasicBlock* lastBlock, SyntaxAST *retStmt);
// Same as IrgenFromBody, but for binary op
IrBasicBlock* IrgenFromBinaryOp(IrBasicBlock* lastBlock, SyntaxAST *binaryOp);
// Same as IrgenFromBody, but for for statement
IrBasicBlock* IrgenFromForStmt(IrBasicBlock* lastBlock, SyntaxAST *forStmt);
// Same as IrgenFromBody, but for array access op. If "genAddr" is true,
// generated IR only computes the address; if "genAddr" is false, the
// generated IR computes the address and loads the value from the address
IrBasicBlock* IrgenFromArrayAccess(
    IrBasicBlock* lastBlock, SyntaxAST *arrayAccess, bool genAddr);
// Same as IrgenFromBody, but for increment op
IrBasicBlock* IrgenFromIncOp(IrBasicBlock *lastBlock, SyntaxAST *incOp);
// Given an expression "lhs" that is the LHS of an assignment/increment op,
// generate the IR get the variable or address of this LHS. The variable or
// address will be stored in "lhsVar", and whether it's an address or not
// will be stored in "isAddr". Returns the basic block after generating all IR
IrBasicBlock* IrgenGetLhs(
    IrBasicBlock *lastBlock, SyntaxAST *lhs, IrVar **lhsVar, bool *isAddr);
// Same as IrgenFromBody, but for unary op
IrBasicBlock* IrgenFromUnaryOp(IrBasicBlock *lastBlock, SyntaxAST *unaryOp);
// Same as IrgenFromBody, but for assignment statement
IrBasicBlock* IrgenFromAssignStmt(
    IrBasicBlock *lastBlock, SyntaxAST *assignStmt);
// Assign the value of "src" to "dst". If "isAddr" is true, the value of "src"
// is stored to the address in "dst"; if "isAddr" is false, the value of "src"
// is directly copied to "dst"
IrBasicBlock* IrgenAssign(
    IrBasicBlock *lastBlock, IrVar *dst, IrVar *src, bool isAddr);
// Get the value from "var" and assign it to a variable of type "type. The
// variable will be stored in "val". If "isAddr" is true, the value is read from
// the address stored in "var"; if "isAddr" is false, "var" is directly stored
// in "val"
IrBasicBlock* IrgenGetValue(
    IrBasicBlock *lastBlock, IrVar *var, IrVar **val, IrType type, bool isAddr);
// Same as IrgenFromBody, but for assignment expressions
IrBasicBlock* IrgenFromAssign(IrBasicBlock *lastBlock, SyntaxAST *assign);
// Same as IrgenFromBody, but for alloc expressions
IrBasicBlock* IrgenFromAlloc(IrBasicBlock *lastBlock, SyntaxAST *alloc);
// Same as IrgenFromBody, but for cast expressions
IrBasicBlock* IrgenFromCast(IrBasicBlock *lastBlock, SyntaxAST *cast);
// Same as IrgenFromBody, but for dealloc statements
IrBasicBlock* IrgenFromDeallocStmt(IrBasicBlock *lastBlock, SyntaxAST *dealloc);

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
  SyntaxAST *syntaxRetType = methodDecl->firstChild;
  assert(syntaxRetType);
  SyntaxAST *syntaxParamList = syntaxRetType->sibling;
  assert(syntaxParamList);
  SyntaxAST *syntaxParam = syntaxParamList->firstChild;
  for (int i = 0; i < paramTypes->size; ++i) {
    SemaType *paramSemaType = paramTypes->arr[i];
    IrType paramType = IrgenIrTypeFromSemaType(paramSemaType);
    IrVar *param = IrFuncAddVar(func, paramType);
    VectorAdd(params, param);
    assert(syntaxParam);
    syntaxParam->irgenInfo.var = param;
    syntaxParam = syntaxParam->sibling;
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
        case SEMA_PRIM_TYPE_BOOL:
        case SEMA_PRIM_TYPE_U8:
          return IR_TYPE_U8;
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
    case SYNTAX_AST_KIND_IF_STMT:
      return IrgenFromIfStmt(lastBlock, stmt);
    case SYNTAX_AST_KIND_RETURN_STMT:
      return IrgenFromRetStmt(lastBlock, stmt);
    case SYNTAX_AST_KIND_FOR_STMT:
      return IrgenFromForStmt(lastBlock, stmt);
    case SYNTAX_AST_KIND_ASSIGN_STMT:
      return IrgenFromAssignStmt(lastBlock, stmt);
    case SYNTAX_AST_KIND_DEALLOC_STMT:
      return IrgenFromDeallocStmt(lastBlock, stmt);
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
    varInit->irgenInfo.var = var;

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
  if (expr->kind == SYNTAX_OP_CAST_AS) {
    return IrgenFromAssign(lastBlock, expr);
  }

  if (expr->kind != SYNTAX_AST_KIND_OP) {
    return IrgenFromTerm(lastBlock, expr);
  }
  switch (expr->op) {
    case SYNTAX_OP_CALL:
      return IrgenFromCall(lastBlock, expr);
    case SYNTAX_OP_ADD:
    case SYNTAX_OP_SUB:
    case SYNTAX_OP_MUL:
    case SYNTAX_OP_DIV:
    case SYNTAX_OP_MOD:
    case SYNTAX_OP_BIT_AND:
    case SYNTAX_OP_BIT_OR:
    case SYNTAX_OP_BIT_XOR:
    case SYNTAX_OP_LT:
    case SYNTAX_OP_LE:
    case SYNTAX_OP_EQEQ:
    case SYNTAX_OP_NEQ:
    case SYNTAX_OP_GE:
    case SYNTAX_OP_GT:
    case SYNTAX_OP_LSHIFT:
    case SYNTAX_OP_RSHIFT:
      return IrgenFromBinaryOp(lastBlock, expr);
    case SYNTAX_OP_NEG:
      return IrgenFromUnaryOp(lastBlock, expr);
    case SYNTAX_OP_ARRAY_ACCESS:
      return IrgenFromArrayAccess(lastBlock, expr, /*genAddr=*/false);
    case SYNTAX_OP_INC:
    case SYNTAX_OP_DEC:
      return IrgenFromIncOp(lastBlock, expr);
    case SYNTAX_OP_ALLOC:
      return IrgenFromAlloc(lastBlock, expr);
    case SYNTAX_OP_CAST_IS:
    case SYNTAX_OP_CAST_AS:
    case SYNTAX_OP_CAST_INTO:
      return IrgenFromCast(lastBlock, expr);
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
  if (term->kind == SYNTAX_AST_KIND_IDENTIFIER) {
    if (SemaValueIsCapturable(term)) {
      SyntaxAST *decl = term->semaInfo.decl;
      term->irgenInfo.var = decl->irgenInfo.var;
    }
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
        case SYNTAX_TYPE_U32:
        case SYNTAX_TYPE_I32:
        case SYNTAX_TYPE_U8: {
          op = IrOpNewConst(var, term->literal.intVal);
          break;
        } case SYNTAX_TYPE_BOOL: {
          op = IrOpNewConst(var, term->literal.boolVal);
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

IrBasicBlock* IrgenFromIfStmt(IrBasicBlock* lastBlock, SyntaxAST *ifStmt) {
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrBasicBlock *endBlock = IrBasicBlockAdd(func);
  SyntaxAST *cond, *body;
  for (cond = ifStmt->firstChild; cond && (body = cond->sibling);
       cond = body->sibling) {
    lastBlock = IrgenFromExpr(lastBlock, cond);
    IrBasicBlock *trueBlock = IrBasicBlockAdd(func);
    IrBasicBlock *falseBlock = IrBasicBlockAdd(func);
    IrBasicBlockSetCond(lastBlock, cond->irgenInfo.var);
    IrBasicBlockSetTrueBlock(lastBlock, trueBlock);
    IrBasicBlockSetFalseBlock(lastBlock, falseBlock);
    trueBlock = IrgenFromBody(trueBlock, body);
    IrBasicBlockSetTrueBlock(trueBlock, endBlock);
    lastBlock = falseBlock;
  }
  if (cond) {
    SyntaxAST *elseBody = cond;
    lastBlock = IrgenFromBody(lastBlock, elseBody);
  }
  IrBasicBlockSetTrueBlock(lastBlock, endBlock);
  return endBlock;
}

IrBasicBlock* IrgenFromRetStmt(IrBasicBlock* lastBlock, SyntaxAST *retStmt) {
  SyntaxAST *retExpr = retStmt->firstChild;
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  if (retExpr) {
    lastBlock = IrgenFromExpr(lastBlock, retExpr);
    IrType retType = IrgenIrTypeFromSemaType(retExpr->semaInfo.typeInfo.type);
    IrVar *retVar = IrFuncAddVar(func, retType);
    IrBasicBlockSetRet(lastBlock, retVar);
    IrOpAppend(lastBlock, IrOpNewCopy(retVar, retExpr->irgenInfo.var));
  }
  lastBlock = IrBasicBlockAdd(func);
  return lastBlock;
}

IrBasicBlock* IrgenFromBinaryOp(IrBasicBlock* lastBlock, SyntaxAST *binaryOp) {
  SyntaxAST *lhs = binaryOp->firstChild;
  SyntaxAST *rhs = lhs->sibling;
  lastBlock = IrgenFromExpr(lastBlock, lhs);
  lastBlock = IrgenFromExpr(lastBlock, rhs);
  IrVar *lhsVar = lhs->irgenInfo.var;
  IrVar *rhsVar = rhs->irgenInfo.var;
  IrOpKind kind;
  switch (binaryOp->op) {
    case SYNTAX_OP_ADD:
      kind = IR_OP_KIND_ADD;
      break;
    case SYNTAX_OP_SUB:
      kind = IR_OP_KIND_SUB;
      break;
    case SYNTAX_OP_MUL:
      kind = IR_OP_KIND_MUL;
      break;
    case SYNTAX_OP_DIV:
      kind = IR_OP_KIND_DIV;
      break;
    case SYNTAX_OP_MOD:
      kind = IR_OP_KIND_MOD;
      break;
    case SYNTAX_OP_BIT_AND:
      kind = IR_OP_KIND_AND;
      break;
    case SYNTAX_OP_BIT_XOR:
      kind = IR_OP_KIND_XOR;
      break;
    case SYNTAX_OP_BIT_OR:
      kind = IR_OP_KIND_OR;
      break;
    case SYNTAX_OP_LT:
      kind = IR_OP_KIND_LT;
      break;
    case SYNTAX_OP_LE:
      kind = IR_OP_KIND_LE;
      break;
    case SYNTAX_OP_EQEQ:
      kind = IR_OP_KIND_EQ;
      break;
    case SYNTAX_OP_NEQ:
      kind = IR_OP_KIND_NE;
      break;
    case SYNTAX_OP_GE:
      kind = IR_OP_KIND_GE;
      break;
    case SYNTAX_OP_GT:
      kind = IR_OP_KIND_GT;
      break;
    case SYNTAX_OP_LSHIFT:
      kind = IR_OP_KIND_LSHIFT;
      break;
    case SYNTAX_OP_RSHIFT:
      kind = IR_OP_KIND_RSHIFT;
      break;
    default:
      printf("Op: %d\n", binaryOp->op);
      assert(false);
  }
  IrType resType;
  switch (binaryOp->op) {
    case SYNTAX_OP_ADD:
    case SYNTAX_OP_SUB:
    case SYNTAX_OP_MUL:
    case SYNTAX_OP_DIV:
    case SYNTAX_OP_MOD:
      // TODO: handling implicit casting
      assert(lhsVar->type == rhsVar->type);
    case SYNTAX_OP_BIT_AND:
    case SYNTAX_OP_BIT_XOR:
    case SYNTAX_OP_BIT_OR:
    case SYNTAX_OP_LSHIFT:
    case SYNTAX_OP_RSHIFT:
      resType = lhsVar->type;
      break;
    case SYNTAX_OP_LT:
    case SYNTAX_OP_LE:
    case SYNTAX_OP_EQEQ:
    case SYNTAX_OP_NEQ:
    case SYNTAX_OP_GE:
    case SYNTAX_OP_GT:
      resType = IR_TYPE_U8;
      break;
    default:
      printf("Op: %d\n", binaryOp->op);
      assert(false);
  }
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrVar *resVar = IrFuncAddVar(func, resType);
  binaryOp->irgenInfo.var = resVar;
  IrOpAppend(lastBlock, IrOpNewBinaryOp(kind, resVar, lhsVar, rhsVar));
  return lastBlock;
}

IrBasicBlock* IrgenFromForStmt(IrBasicBlock* lastBlock, SyntaxAST *forStmt) {
  SyntaxAST *init = forStmt->firstChild;
  SyntaxAST *cond = init->sibling;
  SyntaxAST *iter = cond->sibling;
  SyntaxAST *body = iter->sibling;
  SyntaxAST *label = body->sibling;

  // TODO: support labels
  assert(!label);

  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrBasicBlock *condBlock = IrBasicBlockAdd(func);
  IrBasicBlock *bodyBlock = IrBasicBlockAdd(func);
  IrBasicBlock *nextBlock = IrBasicBlockAdd(func);

  switch (init->kind) {
    case SYNTAX_AST_KIND_VAR_DECL:
      lastBlock = IrgenFromVarDecl(lastBlock, init);
      break;
    case SYNTAX_AST_KIND_PLACEHOLDER:
      break;
    default:
      lastBlock = IrgenFromExpr(lastBlock, init);
      break;
  }
  IrBasicBlockSetTrueBlock(lastBlock, condBlock);

  IrVar *condVar;
  if (cond->kind == SYNTAX_AST_KIND_PLACEHOLDER) {
    // TODO: generate an unconditional jump instead
    condVar = IrFuncAddVar(func, IR_TYPE_U8);
    IrOpAppend(condBlock, IrOpNewConst(condVar, true));
  } else {
    condBlock = IrgenFromExpr(condBlock, cond);
    condVar = cond->irgenInfo.var;
  }
  IrBasicBlockSetCond(condBlock, condVar);
  IrBasicBlockSetTrueBlock(condBlock, bodyBlock);
  IrBasicBlockSetFalseBlock(condBlock, nextBlock);

  bodyBlock = IrgenFromBody(bodyBlock, body);
  if (iter->kind != SYNTAX_AST_KIND_PLACEHOLDER) {
    bodyBlock = IrgenFromExpr(bodyBlock, iter);
  }
  IrBasicBlockSetTrueBlock(bodyBlock, condBlock);

  return nextBlock;
}

IrBasicBlock* IrgenFromArrayAccess(
    IrBasicBlock* lastBlock, SyntaxAST *arrayAccess, bool genAddr) {
  SyntaxAST *operand = arrayAccess->firstChild;
  SyntaxAST *index = operand->sibling;
  lastBlock = IrgenFromExpr(lastBlock, operand);
  lastBlock = IrgenFromExpr(lastBlock, index);

  IrVar *operandVar = operand->irgenInfo.var;
  IrVar *indexVar = index->irgenInfo.var;
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrVar *addrVar = IrFuncAddVar(func, IR_TYPE_ADDR);
  if (indexVar->type == IR_TYPE_ADDR) {
    IrOpAppend(lastBlock, IrOpNewCopy(addrVar, indexVar));
  } else {
    IrOpAppend(lastBlock, IrOpNewUnaryOp(IR_OP_KIND_AS, addrVar, indexVar));
  }
  assert(operandVar->type == IR_TYPE_ADDR);
  SemaType *elemSemaType = arrayAccess->semaInfo.typeInfo.type;
  IrType elemType = IrgenIrTypeFromSemaType(elemSemaType);
  int elemSize = IrTypeGetSize(elemType);
  if (elemSize > 1) {
    IrVar *elemSizeVar = IrFuncAddVar(func, IR_TYPE_ADDR);
    IrOpAppend(lastBlock, IrOpNewConst(elemSizeVar, elemSize));
    IrOpAppend(
        lastBlock,
        IrOpNewBinaryOp(IR_OP_KIND_MUL, addrVar, addrVar, elemSizeVar));
  }
  IrOpAppend(
      lastBlock,
      IrOpNewBinaryOp(IR_OP_KIND_ADD, addrVar, addrVar, operandVar));

  if (genAddr) {
    arrayAccess->irgenInfo.var = addrVar;
  } else {
    IrVar *elemVar = IrFuncAddVar(func, elemType);
    IrOpAppend(lastBlock, IrOpNewUnaryOp(IR_OP_KIND_LOAD, elemVar, addrVar));
    arrayAccess->irgenInfo.var = elemVar;
  }
  return lastBlock;
}

IrBasicBlock* IrgenFromIncOp(IrBasicBlock *lastBlock, SyntaxAST *incOp) {
  SyntaxAST *lhs = incOp->firstChild;
  IrVar *lhsVar;
  bool isAddr;
  lastBlock = IrgenGetLhs(lastBlock, lhs, &lhsVar, &isAddr);
  // TODO: support addresses
  assert(!isAddr);
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrVar *oneVar = IrFuncAddVar(func, lhsVar->type);
  IrOpAppend(lastBlock, IrOpNewConst(oneVar, 1));
  IrOpKind kind;
  switch (incOp->op) {
    case SYNTAX_OP_INC:
      kind = IR_OP_KIND_ADD;
      break;
    case SYNTAX_OP_DEC:
      kind = IR_OP_KIND_SUB;
      break;
    default:
      assert(false);
  }
  IrOpAppend(
      lastBlock, IrOpNewBinaryOp(kind, lhsVar, lhsVar, oneVar));
  return lastBlock;
}

IrBasicBlock* IrgenGetLhs(
    IrBasicBlock *lastBlock, SyntaxAST *lhs, IrVar **lhsVar, bool *isAddr) {
  switch (lhs->kind) {
    case SYNTAX_AST_KIND_IDENTIFIER:
      lastBlock = IrgenFromExpr(lastBlock, lhs);
      *lhsVar = lhs->irgenInfo.var;
      *isAddr = false;
      break;
    case SYNTAX_AST_KIND_OP:
      switch (lhs->op) {
        case SYNTAX_OP_ARRAY_ACCESS:
          lastBlock = IrgenFromArrayAccess(lastBlock, lhs, /*genAddr=*/true);
          *lhsVar = lhs->irgenInfo.var;
          *isAddr = true;
          break;
        default:
          printf("Op: %d\n", lhs->op);
          assert(false);
      }
      break;
    default:
      printf("Kind: %d\n", lhs->kind);
      assert(false);
  }
  return lastBlock;
}

IrBasicBlock* IrgenFromUnaryOp(IrBasicBlock *lastBlock, SyntaxAST *unaryOp) {
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrType dstType = IrgenIrTypeFromSemaType(unaryOp->semaInfo.typeInfo.type);
  IrVar *dst = IrFuncAddVar(func, dstType);
  SyntaxAST *expr = unaryOp->firstChild;
  lastBlock = IrgenFromExpr(lastBlock, expr);
  IrVar *src = expr->irgenInfo.var;
  IrOpKind kind;
  switch (unaryOp->op) {
    case SYNTAX_OP_NEG:
      kind = IR_OP_KIND_NEG;
      break;
    default:
      printf("Op: %d\n", unaryOp->op);
      assert(false);
  }
  IrOpAppend(lastBlock, IrOpNewUnaryOp(kind, dst, src));
  unaryOp->irgenInfo.var = dst;
  return lastBlock;
}

IrBasicBlock* IrgenFromAssignStmt(
    IrBasicBlock *lastBlock, SyntaxAST *assignStmt) {
  SyntaxAST *assign = assignStmt->firstChild;
  return IrgenFromAssign(lastBlock, assign);
}

IrBasicBlock* IrgenFromAssign(
    IrBasicBlock *lastBlock, SyntaxAST *assignStmt) {
  SyntaxAST *lhs = assignStmt->firstChild;
  SyntaxAST *rhs = lhs->sibling;
  lastBlock = IrgenFromExpr(lastBlock, rhs);
  IrVar *src = rhs->irgenInfo.var;
  IrVar *dst;
  bool isAddr;
  lastBlock = IrgenGetLhs(lastBlock, assignStmt->firstChild, &dst, &isAddr);

  IrOpKind kind;
  bool isPureAssign = false;
  switch (assignStmt->op) {
    case SYNTAX_OP_EQ:
      isPureAssign = true;
      break;
    case SYNTAX_OP_DIV_EQ:
      kind = IR_OP_KIND_DIV;
      break;
    default:
      printf("Op: %d\n", assignStmt->op);
      assert(false);
  }
  if (!isPureAssign) {
    IrVar *dstVal;
    IrType dstType = IrgenIrTypeFromSemaType(lhs->semaInfo.typeInfo.type);
    lastBlock = IrgenGetValue(lastBlock, dst, &dstVal, dstType, isAddr);
    IrOpAppend(lastBlock, IrOpNewBinaryOp(kind, src, dstVal, src));
  }

  lastBlock = IrgenAssign(lastBlock, dst, src, isAddr);
  assignStmt->irgenInfo.var = src;

  return lastBlock;
}

IrBasicBlock* IrgenAssign(
    IrBasicBlock *lastBlock, IrVar *dst, IrVar *src, bool isAddr) {
  if (isAddr) {
    IrOpAppend(lastBlock, IrOpNewUnaryOp(IR_OP_KIND_STORE, dst, src));
  } else {
    IrOpAppend(lastBlock, IrOpNewCopy(dst, src));
  }
  return lastBlock;
}

IrBasicBlock* IrgenGetValue(
    IrBasicBlock *lastBlock, IrVar *var, IrVar **val, IrType type,
    bool isAddr) {
  IrVar *dst;
  if (isAddr) {
    IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
    dst = IrFuncAddVar(func, type);
    IrOpAppend(lastBlock, IrOpNewUnaryOp(IR_OP_KIND_LOAD, dst, var));
  } else {
    dst = var;
  }
  *val = dst;
  return lastBlock;
}

IrBasicBlock* IrgenFromAlloc(IrBasicBlock *lastBlock, SyntaxAST *alloc) {
  SyntaxAST *countExpr = alloc->firstChild;
  lastBlock = IrgenFromExpr(lastBlock, countExpr);
  IrVar *size = countExpr->irgenInfo.var;
  SemaType *arraySemaType = alloc->semaInfo.typeInfo.type;
  bool isTypeOwner;
  SemaType *elemSemaType = SemaTypeDecreaseArrayLevel(
      arraySemaType, &isTypeOwner);
  IrType elemType = IrgenIrTypeFromSemaType(elemSemaType);
  if (isTypeOwner) {
    SemaTypeDelete(elemSemaType);
  }
  int elemSize = IrTypeGetSize(elemType);
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  if (elemSize > 1) {
    IrVar *elemSizeVar = IrFuncAddVar(func, IR_TYPE_U64);
    IrOpAppend(lastBlock, IrOpNewConst(elemSizeVar, elemSize));
    IrOpAppend(
        lastBlock, IrOpNewBinaryOp(IR_OP_KIND_MUL, size, size, elemSizeVar));
  }
  IrVar *ptr = IrFuncAddVar(func, IR_TYPE_ADDR);
  IrOpAppend(lastBlock, IrOpNewUnaryOp(IR_OP_KIND_ALLOC, ptr, size));
  alloc->irgenInfo.var = ptr;
  return lastBlock;
}

IrBasicBlock* IrgenFromCast(IrBasicBlock *lastBlock, SyntaxAST *cast) {
  IrOpKind kind;
  switch (cast->op) {
    case SYNTAX_OP_CAST_AS:
      kind = IR_OP_KIND_AS;
      break;
    case SYNTAX_OP_CAST_INTO:
      kind = IR_OP_KIND_INTO;
      break;
    default:
      assert(false);
      printf("Op: %d\n", cast->op);
  }
  SyntaxAST *src = cast->firstChild;
  lastBlock = IrgenFromExpr(lastBlock, src);
  IrVar *srcVar = src->irgenInfo.var;
  IrFunc *func = IrBasicBlockGetParentFunc(lastBlock);
  IrType dstType = IrgenIrTypeFromSemaType(cast->semaInfo.typeInfo.type);
  IrVar *dstVar = IrFuncAddVar(func, dstType);
  IrOpAppend(lastBlock, IrOpNewUnaryOp(kind, dstVar, srcVar));
  cast->irgenInfo.var = dstVar;
  return lastBlock;
}

IrBasicBlock* IrgenFromDeallocStmt(
    IrBasicBlock *lastBlock, SyntaxAST *dealloc) {
  SyntaxAST *varNode = dealloc->firstChild;
  lastBlock = IrgenFromExpr(lastBlock, varNode);
  IrVar *var = varNode->irgenInfo.var;
  IrOpAppend(lastBlock, IrOpNewNullaryOp(IR_OP_KIND_DEALLOC, var));
  return lastBlock;
}
