#include "util/source/source.h"
#include "util/bigint/bigint.h"
#include <assert.h>
#include <math.h>
#include <float.h>

// Creates a new SyntaxAST node with no children, no sibling and kind "kind"
SyntaxAST *SyntaxASTNew(int kind);
// Prepend a AST "child" node to "node" and update "node"'s source location
void SyntaxASTPrepend(SyntaxAST *node, SyntaxAST *child);
// Same as SyntaxASTPrepend, except appends the last child rather than prepend
// to the first
void SyntaxASTAppend(SyntaxAST *node, SyntaxAST *child);
// Extract string from source of "token" into an AST node with kind "kind";
// The returned AST node contains a malloced string stored in its string
// field, thus must be freed in SyntaxASTDestructor. Also, this function
// deletes "token", so no need to delete it after calling
SyntaxAST *SyntaxTokenToAST(LexerToken *token, int kind);
// Parser handler of lists in the CFG. Lists are of the form
//   S -> S x T | T
// or
//   S -> S x T |
// where the following things hold
// - the handler of S should return an AST node of kind "parentKind" that
//   contains a list of its children (the T's)
// - if separator == -1, then x is the empty string (no separator)
// - if separator != -1, then x is a token of ID "seperator"
// - if childKind == -1, then T is a CFG varaible
// - if childKind != -1, then T is a token, which will be converted to an AST
//   node of kind "childKind"
// Below are a list of helper macros in the case where the separator or
// childKind are set to -1
SyntaxAST *SyntaxHandlerSeparatedTokenList(
    ParserRHS *rhs, int separator, int parentKind, int childKind);
#define SyntaxHandlerList(rhs, parentKind) \
  SyntaxHandlerSeparatedTokenList(rhs, -1, parentKind, -1)
#define SyntaxHandlerSeparatedList(rhs, separator, parentKind) \
  SyntaxHandlerSeparatedTokenList(rhs, separator, parentKind, -1)
#define SyntaxHandlerTokenList(rhs, parentKind) \
  SyntaxHandlerSeparatedTokenList(rhs, -1, parentKind, -1)
// Parser handler for binary operations. Expects the operator token ID to be
// "opTokenID", and generates an AST node of kind SYNTAX_AST_KIND_OP, with op
// set to "op" and two children
SyntaxAST *SyntaxHandlerBinaryOp(ParserRHS *rhs, int opTokenID, SyntaxOp op);
// Same as SyntaxHandlerBinaryOp, except for unary operations. The expected
// operator position depends on "isPrefix": if "isPrefix" is true, then the
// operator is expected to appear before the operand, otherwise it is expected
// to appear after the operand. Below are helper macros to set "prefix"
// accordingly
SyntaxAST *SyntaxHandlerUnaryOp(
    ParserRHS *rhs, bool isPrefix, int opTokenID, SyntaxOp op);
#define SyntaxHandlerPrefixOp(rhs, opTokenID, op) \
  SyntaxHandlerUnaryOp(rhs, true, opTokenID, op)
#define SyntaxHandlerPostfixOp(rhs, opTokenID, op) \
  SyntaxHandlerUnaryOp(rhs, false, opTokenID, op)
// Parser handler for literals from a single token with no processing required;
// checks if the token ID matches "literalTokenID", and returns a SyntaxAST node
// with its literal.type set to "type"
SyntaxAST *SyntaxHandlerLiteral(
    ParserRHS *rhs, int literalTokenID, SyntaxType type);
// Given a pointer "p" to a string, returns the next character considering
// escape sequences; if successfully found next character, advances the string
// pointed by "p" to the next position of a new character, and returns the next
// character; otherwise, if cannot successfully find next character, the string
// pointed by "p" is unchanged, and returns -1
char SyntaxNextCharacter(char **p);

ParserDeclareHandler(SyntaxHandlerModule, rhs) {
  assert(rhs->size == 2);
  SyntaxAST *importDecls = rhs->arr[0], *classDecls = rhs->arr[1];
  assert(importDecls);
  assert(classDecls);

  SyntaxAST *module = SyntaxASTNew(SYNTAX_AST_KIND_MODULE);
  SyntaxASTAppend(module, importDecls);
  SyntaxASTAppend(module, classDecls);
  return module;
}

ParserDeclareHandler(SyntaxHandlerImportDecls, rhs) {
  return SyntaxHandlerList(rhs, SYNTAX_AST_KIND_IMPORT_DECLS);
}

ParserDeclareHandler(SyntaxHandlerImportDecl, rhs) {
  assert(rhs->size == 4);
  LexerToken *import_ = rhs->arr[0];
  SyntaxAST *modulePath = rhs->arr[1];
  SyntaxAST *importDecl = rhs->arr[2];
  LexerToken *semicol_ = rhs->arr[3];
  assert(import_ && import_->tokenID == IMPORT);
  assert(modulePath);
  assert(importDecl);
  assert(semicol_ && semicol_->tokenID == SEMICOL);

  SyntaxASTAppend(importDecl, modulePath);
  importDecl->loc.from = import_->loc.from;
  importDecl->loc.to = semicol_->loc.to;

  LexerTokenDelete(import_);
  LexerTokenDelete(semicol_);
  return importDecl;
}

ParserDeclareHandler(SyntaxHandlerModulePath, rhs) {
  return SyntaxHandlerSeparatedTokenList(
      rhs, DOT, SYNTAX_AST_KIND_MODULE_PATH, SYNTAX_AST_KIND_IDENTIFIER);
}

ParserDeclareHandler(SyntaxHandlerModulePathExt, rhs) {
  SyntaxAST *importDecl = SyntaxASTNew(SYNTAX_AST_KIND_IMPORT_DECL);
  if (rhs->size == 2) {
    LexerToken *token = rhs->arr[0];
    assert(token);
    if (token->tokenID == AS) {
      LexerToken *identifier_ = rhs->arr[1];
      assert(identifier_ && identifier_->tokenID == IDENTIFIER);

      char c = identifier_->str[identifier_->length];
      identifier_->str[identifier_->length] = '\0';
      importDecl->import.namespace = strdup(identifier_->str);
      importDecl->import.isWildcard = false;

      identifier_->str[identifier_->length] = c;
      LexerTokenDelete(identifier_);
    } else {
      assert(token->tokenID == DOT);
      LexerToken *mul_ = rhs->arr[1];
      assert(mul_ && mul_->tokenID == MUL);

      importDecl->import.namespace = NULL;
      importDecl->import.isWildcard = true;

      LexerTokenDelete(mul_);
    }
    LexerTokenDelete(token);
  } else {
    assert(rhs->size == 0);

    importDecl->import.namespace = NULL;
    importDecl->import.isWildcard = false;
  }
  return importDecl;
}

ParserDeclareHandler(SyntaxHandlerClassDecls, rhs) {
  return SyntaxHandlerList(rhs, SYNTAX_AST_KIND_CLASS_DECLS);
}

ParserDeclareHandler(SyntaxHandlerClassDecl, rhs) {
  assert(rhs->size == 6);

  LexerToken *class_ = rhs->arr[0];
  LexerToken *identifier_ = rhs->arr[1];
  LexerToken *lbrace_ = rhs->arr[2];
  SyntaxAST *classVarDeclStmts = rhs->arr[3];
  SyntaxAST *methodDecls = rhs->arr[4];
  LexerToken *rbrace_ = rhs->arr[5];
  assert(class_ && class_->tokenID == CLASS);
  assert(identifier_ && identifier_->tokenID == IDENTIFIER);
  assert(lbrace_ && lbrace_->tokenID == LBRACE);
  assert(classVarDeclStmts);
  assert(methodDecls);
  assert(rbrace_ && rbrace_->tokenID == RBRACE);

  SyntaxAST *classDecl = SyntaxASTNew(SYNTAX_AST_KIND_CLASS_DECL);
  SyntaxASTAppend(classDecl, classVarDeclStmts);
  SyntaxASTAppend(classDecl, methodDecls);
  classDecl->loc.from = class_->loc.from;
  classDecl->loc.to = rbrace_->loc.to;
  classDecl->string = strndup(identifier_->str, identifier_->length);

  LexerTokenDelete(class_);
  LexerTokenDelete(identifier_);
  LexerTokenDelete(lbrace_);
  LexerTokenDelete(rbrace_);
  return classDecl;
}

ParserDeclareHandler(SyntaxHandlerVarDeclStmts, rhs) {
  return SyntaxHandlerList(rhs, SYNTAX_AST_KIND_STMTS);
}

ParserDeclareHandler(SyntaxHandlerVarDeclStmt, rhs) {
  SyntaxAST *varDecl = rhs->arr[0];
  LexerToken *semicol_ = rhs->arr[1];
  assert(varDecl);
  assert(semicol_ && semicol_->tokenID == SEMICOL);

  varDecl->loc.to = semicol_->loc.to;

  LexerTokenDelete(semicol_);
  return varDecl;
}

ParserDeclareHandler(SyntaxHandlerVarDecl, rhs) {
  SyntaxAST *varDecl = rhs->arr[0];
  SyntaxAST *type = rhs->arr[1];
  SyntaxAST *varInitList = rhs->arr[2];
  assert(varDecl);
  assert(type);
  assert(varInitList);

  SyntaxASTAppend(varDecl, type);
  SyntaxASTAppend(varDecl, varInitList);

  return varDecl;
}

ParserDeclareHandler(SyntaxHandlerVarDeclModifiers, rhs) {
  SyntaxAST *varDecl;
  if (rhs->size == 2) {
    varDecl = rhs->arr[0];
    LexerToken *modifier = rhs->arr[1];
    assert(varDecl);
    assert(modifier);

    varDecl->loc.from = SourcePointMin(&varDecl->loc.from, &modifier->loc.from);
    varDecl->loc.to = SourcePointMax(&varDecl->loc.to, &modifier->loc.to);
    switch (modifier->tokenID) {
      case STATIC:
        varDecl->varDeclModifiers |= SYNTAX_VAR_DECL_STATIC;
        break;
      case CONST:
        varDecl->varDeclModifiers |= SYNTAX_VAR_DECL_CONST;
        break;
      default:
        abort();
    }
    LexerTokenDelete(modifier);
  } else {
    assert(rhs->size == 0);
    varDecl = SyntaxASTNew(SYNTAX_AST_KIND_VAR_DECL);
    varDecl->varDeclModifiers = 0;
  }
  return varDecl;
}

ParserDeclareHandler(SyntaxHandlerMove, rhs) {
  assert(rhs->size == 1);
  return rhs->arr[0];
}

ParserDeclareHandler(SyntaxHandlerVar, rhs) {
  assert(rhs->size == 1);
  LexerToken *var_ = rhs->arr[0];

  SyntaxAST *type = SyntaxASTNew(SYNTAX_AST_KIND_TYPE);
  type->type.baseType = SYNTAX_TYPE_VAR;
  type->type.arrayLevels = 0;
  type->loc = var_->loc;
  LexerTokenDelete(var_);
  return type;
}

ParserDeclareHandler(SyntaxHandlerType, rhs) {
  SyntaxAST *type;
  if (rhs->size == 1) {
    SyntaxAST *modulePath = rhs->arr[0];
    assert(modulePath);

    type = SyntaxASTNew(SYNTAX_AST_KIND_TYPE);
    SyntaxASTAppend(type, modulePath);
    type->type.baseType = SYNTAX_TYPE_MODULE_PATH;
    type->type.arrayLevels = 0;
  } else if (rhs->size == 3) {
    type = rhs->arr[0];
    LexerToken *lbrack_ = rhs->arr[1];
    LexerToken *rbrack_ = rhs->arr[2];
    assert(lbrack_ && lbrack_->tokenID == LBRACK);
    assert(rbrack_ && rbrack_->tokenID == RBRACK);
    ++type->type.arrayLevels;
    type->loc.to = rbrack_->loc.to;

    LexerTokenDelete(lbrack_);
    LexerTokenDelete(rbrack_);
  } else {
    assert(rhs->size == 4);
    SyntaxAST *retType = rhs->arr[0];
    LexerToken *lparen_ = rhs->arr[1];
    SyntaxAST *typeList = rhs->arr[2];
    LexerToken *rparen_ = rhs->arr[3];
    assert(retType);
    assert(lparen_ && lparen_->tokenID == LPAREN);
    assert(typeList);
    assert(rparen_ && rparen_->tokenID == RPAREN);

    type = SyntaxASTNew(SYNTAX_AST_KIND_TYPE);
    SyntaxASTAppend(type, retType);
    SyntaxASTAppend(type, typeList);
    type->type.baseType = SYNTAX_TYPE_FUNC;
    type->type.arrayLevels = 0;
    type->loc.to = rparen_->loc.to;
    LexerTokenDelete(lparen_);
    LexerTokenDelete(rparen_);
  }

  return type;
}

ParserDeclareHandler(SyntaxHandlerPrimitiveType, rhs) {
  SyntaxType baseType;
  assert(rhs->size == 1);
  LexerToken *token = rhs->arr[0];
  switch (token->tokenID) {
    case I64:
      baseType = SYNTAX_TYPE_I64;
      break;
    case U64:
      baseType = SYNTAX_TYPE_U64;
      break;
    case I32:
      baseType = SYNTAX_TYPE_I32;
      break;
    case U32:
      baseType = SYNTAX_TYPE_U32;
      break;
    case I16:
      baseType = SYNTAX_TYPE_I16;
      break;
    case U16:
      baseType = SYNTAX_TYPE_U16;
      break;
    case I8:
      baseType = SYNTAX_TYPE_I8;
      break;
    case U8:
      baseType = SYNTAX_TYPE_U8;
      break;
    case F64:
      baseType = SYNTAX_TYPE_F64;
      break;
    case F32:
      baseType = SYNTAX_TYPE_F32;
      break;
    case BOOL:
      baseType = SYNTAX_TYPE_BOOL;
      break;
    case ANY:
      baseType = SYNTAX_TYPE_ANY;
      break;
    case VOID:
      baseType = SYNTAX_TYPE_VOID;
      break;
    default:
      abort();
  }

  SyntaxAST *type = SyntaxASTNew(SYNTAX_AST_KIND_TYPE);
  type->type.baseType = baseType;
  type->type.arrayLevels = 0;
  type->loc = token->loc;
  LexerTokenDelete(token);
  return type;
}

ParserDeclareHandler(SyntaxHandlerTypeList, rhs) {
  return SyntaxHandlerSeparatedList(rhs, COMMA, SYNTAX_AST_KIND_TYPE_LIST);
}

ParserDeclareHandler(SyntaxHandlerVarInitList, rhs) {
  return SyntaxHandlerSeparatedList(rhs, COMMA, SYNTAX_AST_KIND_VAR_INIT_LIST);
}

ParserDeclareHandler(SyntaxHandlerVarInit, rhs) {
  SyntaxAST *varInit = SyntaxASTNew(SYNTAX_AST_KIND_VAR_INIT);
  LexerToken *identifier_ = rhs->arr[0];
  varInit->loc = identifier_->loc;
  varInit->string = strndup(identifier_->str, identifier_->length);

  if (rhs->size == 3) {
    LexerToken *eq_ = rhs->arr[1];
    SyntaxAST *expr = rhs->arr[2];
    assert(eq_);
    assert(expr);

    SyntaxASTAppend(varInit, expr);
    LexerTokenDelete(eq_);
  } else {
    assert(rhs->size == 1);
  }

  LexerTokenDelete(identifier_);
  return varInit;
}

ParserDeclareHandler(SyntaxHandlerExprTernary, rhs) {
  assert(rhs->size == 5);
  SyntaxAST *cond = rhs->arr[0];
  LexerToken *if_ = rhs->arr[1];
  SyntaxAST *exprIf = rhs->arr[2];
  LexerToken *else_ = rhs->arr[3];
  SyntaxAST *exprElse = rhs->arr[4];
  assert(cond);
  assert(if_ && if_->tokenID == IF);
  assert(exprIf);
  assert(else_ && else_->tokenID == ELSE);
  assert(exprElse);

  SyntaxAST *expr = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  expr->op = SYNTAX_OP_TERNARY;
  SyntaxASTAppend(expr, cond);
  SyntaxASTAppend(expr, exprIf);
  SyntaxASTAppend(expr, exprElse);
  LexerTokenDelete(if_);
  LexerTokenDelete(else_);
  return expr;
}

ParserDeclareHandler(SyntaxHandlerExprLogicOr, rhs) {
  return SyntaxHandlerBinaryOp(rhs, OR, SYNTAX_OP_LOGIC_OR);
}

ParserDeclareHandler(SyntaxHandlerExprLogicAnd, rhs) {
  return SyntaxHandlerBinaryOp(rhs, AND, SYNTAX_OP_LOGIC_AND);
}

ParserDeclareHandler(SyntaxHandlerExprBitOr, rhs) {
  return SyntaxHandlerBinaryOp(rhs, BIT_OR, SYNTAX_OP_BIT_OR);
}

ParserDeclareHandler(SyntaxHandlerExprBitXor, rhs) {
  return SyntaxHandlerBinaryOp(rhs, BIT_XOR, SYNTAX_OP_BIT_XOR);
}

ParserDeclareHandler(SyntaxHandlerExprBitAnd, rhs) {
  return SyntaxHandlerBinaryOp(rhs, BIT_AND, SYNTAX_OP_BIT_AND);
}

ParserDeclareHandler(SyntaxHandlerExprLt, rhs) {
  return SyntaxHandlerBinaryOp(rhs, LT, SYNTAX_OP_LT);
}

ParserDeclareHandler(SyntaxHandlerExprLeq, rhs) {
  return SyntaxHandlerBinaryOp(rhs, LEQ, SYNTAX_OP_LE);
}

ParserDeclareHandler(SyntaxHandlerExprEq, rhs) {
  return SyntaxHandlerBinaryOp(rhs, EQEQ, SYNTAX_OP_EQ);
}

ParserDeclareHandler(SyntaxHandlerExprNeq, rhs) {
  return SyntaxHandlerBinaryOp(rhs, NEQ, SYNTAX_OP_NEQ);
}

ParserDeclareHandler(SyntaxHandlerExprGt, rhs) {
  return SyntaxHandlerBinaryOp(rhs, GT, SYNTAX_OP_GT);
}

ParserDeclareHandler(SyntaxHandlerExprGeq, rhs) {
  return SyntaxHandlerBinaryOp(rhs, GEQ, SYNTAX_OP_GE);
}

ParserDeclareHandler(SyntaxHandlerExprLshift, rhs) {
  return SyntaxHandlerBinaryOp(rhs, LSHIFT, SYNTAX_OP_LSHIFT);
}

ParserDeclareHandler(SyntaxHandlerExprRshift, rhs) {
  return SyntaxHandlerBinaryOp(rhs, RSHIFT, SYNTAX_OP_RSHIFT);
}

ParserDeclareHandler(SyntaxHandlerExprAdd, rhs) {
  return SyntaxHandlerBinaryOp(rhs, ADD, SYNTAX_OP_ADD);
}

ParserDeclareHandler(SyntaxHandlerExprSub, rhs) {
  return SyntaxHandlerBinaryOp(rhs, SUB, SYNTAX_OP_SUB);
}

ParserDeclareHandler(SyntaxHandlerExprMul, rhs) {
  return SyntaxHandlerBinaryOp(rhs, MUL, SYNTAX_OP_MUL);
}

ParserDeclareHandler(SyntaxHandlerExprDiv, rhs) {
  return SyntaxHandlerBinaryOp(rhs, DIV, SYNTAX_OP_DIV);
}

ParserDeclareHandler(SyntaxHandlerExprMod, rhs) {
  return SyntaxHandlerBinaryOp(rhs, MOD, SYNTAX_OP_MOD);
}

ParserDeclareHandler(SyntaxHandlerExprNeg, rhs) {
  return SyntaxHandlerPrefixOp(rhs, SUB, SYNTAX_OP_NEG);
}

ParserDeclareHandler(SyntaxHandlerExprNot, rhs) {
  return SyntaxHandlerPrefixOp(rhs, NOT, SYNTAX_OP_NOT);
}

ParserDeclareHandler(SyntaxHandlerExprBitNot, rhs) {
  return SyntaxHandlerPrefixOp(rhs, BIT_NOT, SYNTAX_OP_BIT_NOT);
}

ParserDeclareHandler(SyntaxHandlerExprCast, rhs) {
  assert(rhs->size == 3);
  SyntaxAST *expr = rhs->arr[0];
  LexerToken *as_ = rhs->arr[1];
  SyntaxAST *type = rhs->arr[2];
  assert(expr);
  assert(as_ && as_->tokenID == AS);
  assert(type);

  SyntaxAST *op = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  op->op = SYNTAX_OP_CAST;
  SyntaxASTAppend(op, expr);
  SyntaxASTAppend(op, type);
  LexerTokenDelete(as_);
  return op;
}

ParserDeclareHandler(SyntaxHandlerExprCall, rhs) {
  assert(rhs->size == 4);
  SyntaxAST *function = rhs->arr[0];
  LexerToken *lParen_ = rhs->arr[1];
  SyntaxAST *args = rhs->arr[2];
  LexerToken *rParen_ = rhs->arr[3];
  assert(function);
  assert(lParen_ && lParen_->tokenID == LPAREN);
  assert(args);
  assert(rParen_ && rParen_->tokenID == RPAREN);

  SyntaxAST *call = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  call->op = SYNTAX_OP_CALL;
  call->loc.to = rParen_->loc.to;
  SyntaxASTAppend(call, function);
  SyntaxASTAppend(call, args);
  LexerTokenDelete(lParen_);
  LexerTokenDelete(rParen_);
  return call;
}

ParserDeclareHandler(SyntaxHandlerExprArrayAccess, rhs) {
  assert(rhs->size == 4);
  SyntaxAST *array = rhs->arr[0];
  LexerToken *lbrack_ = rhs->arr[1];
  SyntaxAST *idx = rhs->arr[2];
  LexerToken *rbrack_ = rhs->arr[3];
  assert(array);
  assert(lbrack_ && lbrack_->tokenID == LBRACK);
  assert(idx);
  assert(rbrack_ && rbrack_->tokenID == RBRACK);

  SyntaxAST *access = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  access->op = SYNTAX_OP_ARRAY_ACCESS;
  access->loc.to = rbrack_->loc.to;
  SyntaxASTAppend(access, array);
  SyntaxASTAppend(access, idx);
  LexerTokenDelete(lbrack_);
  LexerTokenDelete(rbrack_);
  return access;
}

ParserDeclareHandler(SyntaxHandlerExprArrayType, rhs) {
  assert(rhs->size == 3);
  SyntaxAST *array = rhs->arr[0];
  LexerToken *lbrack_ = rhs->arr[1];
  LexerToken *rbrack_ = rhs->arr[2];
  assert(array);
  assert(lbrack_ && lbrack_->tokenID == LBRACK);
  assert(rbrack_ && rbrack_->tokenID == RBRACK);

  SyntaxAST *access = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  access->op = SYNTAX_OP_ARRAY_TYPE;
  access->loc.to = rbrack_->loc.to;
  SyntaxASTAppend(access, array);
  LexerTokenDelete(lbrack_);
  LexerTokenDelete(rbrack_);
  return access;
}

ParserDeclareHandler(SyntaxHandlerExprMemberAccess, rhs) {
  assert(rhs->size == 3);
  SyntaxAST *expr = rhs->arr[0];
  LexerToken *dot_ = rhs->arr[1];
  LexerToken *identifier_ = rhs->arr[2];
  assert(expr);
  assert(dot_ && dot_->tokenID == DOT);
  assert(identifier_ && identifier_->tokenID == IDENTIFIER);

  SyntaxAST *access = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  access->op = SYNTAX_OP_MEMBER_ACCESS;
  access->loc.to = identifier_->loc.to;
  access->string = strndup(identifier_->str, identifier_->length);
  SyntaxASTAppend(access, expr);
  LexerTokenDelete(dot_);
  return access;
}

ParserDeclareHandler(SyntaxHandlerExprInc, rhs) {
  return SyntaxHandlerPostfixOp(rhs, ADD_ADD, SYNTAX_OP_INC);
}

ParserDeclareHandler(SyntaxHandlerExprDec, rhs) {
  return SyntaxHandlerPostfixOp(rhs, SUB_SUB, SYNTAX_OP_DEC);
}

ParserDeclareHandler(SyntaxHandlerExprList, rhs) {
  return SyntaxHandlerSeparatedList(rhs, COMMA, SYNTAX_AST_KIND_EXPR_LIST);
}

ParserDeclareHandler(SyntaxHandlerIntLiteral, rhs) {
  assert(rhs->size == 1);
  LexerToken *intToken = rhs->arr[0];
  assert(intToken && intToken->tokenID == INT_LITERAL);

  // TODO: handle out-of-bound integer literals separately when prefixed with
  //       a unary negative operator. For instance, 2^31 is not a valid 32-bit
  //       signed integer, but -2^31 is
  char *str = intToken->str;
  uint64_t val = 0, digit;
  int i;
  bool outOfRange = false;
  if (str[0] == '0') {
    if (str[1] == 'b') {
      // Binary
      for (i = 2; '0' <= str[i] && str[i] <= '1'; ++i) {
        if ((val >> 63) & 1) {
          outOfRange = true;
          break;
        }
        val *= 2;
        digit = str[i] - '0';
        if (val > UINT64_MAX - digit) {
          outOfRange = true;
          break;
        }
        val += digit;
      }
    } else {
      // Hexadecimal
      assert(str[1] == 'x');
      for (i = 2;
           ('0' <= str[i] && str[i] <= '9') ||
           ('A' <= str[i] && str[i] <= 'F') ||
           ('a' <= str[i] && str[i] <= 'f'); ++i) {
        if ((val >> 60) & 1) {
          outOfRange = true;
          break;
        }
        val *= 16;
        if ('0' <= str[i] && str[i] <= '9')
          digit = str[i] - '0';
        else if ('A' <= str[i] && str[i] <= 'F')
          digit = str[i] - 'A' + 10;
        else
          digit = str[i] - 'a' + 10;
        if (val > UINT64_MAX - digit) {
          outOfRange = true;
          break;
        }
        val += digit;
      }
    }
  } else {
    // Decimal
    for (i = 0; '0' <= str[i] && str[i] <= '9'; ++i) {
      if (val > UINT64_MAX / 10) {
        outOfRange = true;
        break;
      }
      val *= 10;
      digit = str[i] - '0';
      if (val > UINT64_MAX - digit) {
        outOfRange = true;
        break;
      }
      val += digit;
    }
  }

  // Extracting the type postfix, default is i32 if empty
  char postfix[4] = "i32";
  int length = intToken->length;
  if (i < intToken->length) {
    char c = str[length];
    str[length] = '\0';
    strcpy(postfix, str + i);
    str[length] = c;
  }

  // Use the type postfix to determine the maximum value of this type
  uint64_t maxVal = INT32_MAX;
  SyntaxType type = SYNTAX_TYPE_I32;
  static const char *postfixes[] = {
    "i64", "u64", "i32", "u32", "i16", "u16", "i8", "u8", NULL
  };
  static const uint64_t maxVals[] = {
    INT64_MAX, UINT64_MAX, INT32_MAX, UINT32_MAX, INT16_MAX, UINT16_MAX,
    INT8_MAX, UINT8_MAX
  };
  static const SyntaxType types[] = {
    SYNTAX_TYPE_I64, SYNTAX_TYPE_U64, SYNTAX_TYPE_I32, SYNTAX_TYPE_U32,
    SYNTAX_TYPE_I16, SYNTAX_TYPE_U16, SYNTAX_TYPE_I8, SYNTAX_TYPE_U8
  };
  for (i = 0; postfixes[i]; ++i) {
    if (strcmp(postfix, postfixes[i]) == 0) {
      maxVal = maxVals[i];
      type = types[i];
      break;
    }
  }

  if (outOfRange || val > maxVal) {
    // TODO: perhaps make this into an error, but would have to handle parser
    //       errors that are generated in the handler
    Parser *parser = rhs->parser;
    Lexer *lexer = parser->lexer;
    SourceLocation *loc = &intToken->loc;
    fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET" %s:%d: ",
            lexer->filename, loc->from.lineNo + 1);
    fprintf(stderr, "integer literal value out of range of %s, clamping to "
                    "maximum value %llu\n", postfix,
                    (unsigned long long)maxVal);
    SourceLocationPrint(lexer->source, 1, SOURCE_COLOR_RED, loc);
    val = maxVal;
  }

  SyntaxAST *intLiteral = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
  intLiteral->literal.type = type;
  intLiteral->literal.intVal = val;
  intLiteral->loc = intToken->loc;
  LexerTokenDelete(intToken);
  return intLiteral;
}

ParserDeclareHandler(SyntaxHandlerFloatLiteral, rhs) {
  assert(rhs->size == 1);
  LexerToken *floatToken = rhs->arr[0];
  assert(floatToken && floatToken->tokenID == FLOAT_LITERAL);

  char *str = floatToken->str;
  int length = floatToken->length;

  // Extract the exponent of the floating point number. We store the exponent
  // in an int, as we do not expect the user to use an exponent that cannot
  // be stored in an int. The only way a user can use an out-of-range exponent
  // but still have a number representable in floating point is if the floating
  // point literal preceding the exponent is 0 or very long (for instance,
  // 1000...000e-100...00)
  int exponent = 0;
  bool outOfRange = false;
  int expPos;
  for (expPos = 0; expPos < length; ++expPos) {
    if (str[expPos] == 'e') {
      int j = expPos + 1;
      bool neg = false;
      if (j == '+') {
        ++j;
      } else if (j == '-') {
        neg = true;
        ++j;
      }
      for (; j < length; ++j) {
        if (exponent > INT_MAX / 10) {
          outOfRange = true;
          break;
        }
        exponent *= 10;
        int d = str[j] - '0';
        if (exponent > INT_MAX - d) {
          outOfRange = true;
          break;
        }
      }
      if (neg)
        exponent = -exponent;
      break;
    }
  }

  // DEBUG
  printf("Exponent: %d\n", exponent);

  // Extract the type postfix, default is f32 if empty
  char postfix[4] = "f32";
  for (int i = 0; i < length; ++i) {
    if (str[i] == 'f') {
      char c = str[length];
      str[length] = '\0';
      strcpy(postfix, str + i);
      str[length] = c;
      break;
    }
  }

  // Use the type postfix to determine the maximum value of this type
  double maxVal = FLT_MAX;
  SyntaxType type = SYNTAX_TYPE_F32;
  static const char *postfixes[] = { "f64", "f32", NULL };
  static const double maxVals[] = { DBL_MAX, FLT_MAX };
  static const SyntaxType types[] = { SYNTAX_TYPE_F64, SYNTAX_TYPE_F32 };
  for (int i = 0; postfixes[i]; ++i) {
    if (strcmp(postfix, postfixes[i]) == 0) {
      maxVal = maxVals[i];
      type = types[i];
      break;
    }
  }

  // Extract the integer part and the fractional part of the floating point
  // literal
  // TODO: use big integer implementations to store the integer and fractional
  // part. We only have to be able to store at most
  //   2^1023+2^1022+...+2^(1023-52),
  // since out of this range means out of double precision range
  // TODO: take the exponent into consideration and shift the dot position
  // before converting
  // TEST
  // uint64_t intBin = 0, fracPart = 0, one = 1, fracBin = 0;
  enum {
    NUM_MANTISSA_BITS = 52,
    EXPONENT_OFFSET = 1023,
    MAX_EXPONENT = 2046
  };
  Bigint intBin, fracPart, one, fracBin;
  // log(10)/log(64) = 0.553... < 0.6 = 3/5, so "length"*3/5 is an upper bound
  // on the number of 64-bit integers required to encode a decimal integer of
  // length "length". 
  int bigintSize = length * 3 / 5;
  BigintNew(&intBin, bigintSize, 0);
  BigintNew(&fracPart, bigintSize, 0);
  BigintNew(&one, bigintSize, 1);
  // Similarly, for the binary of the fraction part, we only need to store at
  // most NUM_MANTISSA_BITS - 1 bits after the first set bit in the fraction
  // part, and the first set bit (2^(-x) for some integer x) must satisfy
  // 10^(-length) >= 2^(-x), so we also only need at most "length"*3/5 64-bit
  // integers for the first set bit to appear, and then at most one more 64-bit
  // integer to store the at most NUM_MANTISSA_BITS - 1 remaining bits after,
  // so we allocate a bigint of size "bigintSize" + 1
  int fracBinSize = bigintSize + 1;
  BigintNew(&fracBin, fracBinSize, 0);
  int i;
  for (i = 0; i < length; ++i) {
    int d = str[i] - '0';
    if (d < 0 || d > 9)
      break;
    // TEST
    // intBin = intBin * 10 + d;
    assert(BigintMulInt(&intBin, &intBin, 10));
  }
  if (str[i] == '.')
    ++i;
  for (; i < length; ++i) {
    int d = str[i] - '0';
    if (d < 0 || d > 9)
      break;
    // TEST
    // fracPart = fracPart * 10 + d;
    assert(BigintMulInt(&fracPart, &fracPart, 10));
    assert(BigintAddInt(&fracPart, &fracPart, d));
    // TEST
    // one *= 10;
    assert(BigintMulInt(&one, &one, 10));
  }

  // DEBUG
  // TEST
  // printf("Int Part: %llu, Frac Part: %llu, One: %llu\n", intBin, fracPart, one);
  ///*
  printf("Int Part: ");
  BigintPrintHex(&intBin);
  printf("\nFrac Part: ");
  BigintPrintHex(&fracPart);
  printf("\nOne: ");
  BigintPrintHex(&one);
  //*/

  // Convert the fractional part into its binary representation. The most
  // significant bit of the last 64-bit integer represents 1/2, then the bit
  // to the right of it represents 1/4, then the next 1/8, ... etc
  // TEST
  // uint64_t mask = 1LL << (NUM_MANTISSA_BITS - 1);
  // while (mask) {
  for (int bitIdx = 64 * fracBinSize - 1; bitIdx >= 0; --bitIdx) {
    // TEST
    // fracPart *= 2;
    BigintMulInt(&fracPart, &fracPart, 2);
    // TEST
    // if (fracPart >= one) {
    if (BigintCmp(&fracPart, &one) >= 0) {
      // TEST
      // fracBin |= mask;
      BigintSetBit(&fracBin, bitIdx, 1);
      // TEST
      // fracPart -= one;
      assert(BigintSub(&fracPart, &fracPart, &one));
    }
    // TEST
    // mask >>= 1;

    // DEBUG
    // printf("Frac Part: %llu, Mask: %016lx\n", fracPart, mask);
  }

  // DEBUG
  // TEST
  // printf("Frac Bin: %016lx\n", fracBin);
  printf("Frac Bin: ");
  BigintPrintHex(&fracBin);

  // Combine the integer and the fractional binary representations to form the
  // mantissa, updating the exponent along the process
  // TODO: consider rounding bits
  uint64_t mantissaBits = 0, exponentBits = 0;
  // TEST
  /*
  if (intBin != 0) {
    for (int i = 63; i >= 0; --i) {
      if ((intBin >> i) & 1) {
        mantissaBits = (intBin << (63 - i + 1)) | (fracBin >> i);
        exponentBits = i + 1023;
        break;
      }
    }
  } else if (fracBin == 0) {
    exponentBits = 0;
  } else {
    for (int i = 63; i >= 0; --i) {
      if ((fracBin >> i) & 1) {
        mantissaBits = fracBin << (63 - i + 1);
        exponentBits = i - 64 + 1023;
        break;
      }
    }
  }
  */

  // Find the most significant non-zero bit from the integer and fraction parts
  Bigint *bin;
  int lastIdx;
  bool intPartIs0 = BigintCmpInt(&intBin, 0) == 0;
  if (intPartIs0) {
    bin = &fracBin;
    lastIdx = fracBinSize - 1;
  } else {
    bin = &intBin;
    lastIdx = bigintSize - 1;
  }
  if (BigintCmpInt(bin, 0) == 0) {
    // Both the integer and fraction parts are 0, so the value must be 0.
    // We treat this as a special case and return early
    SyntaxAST *floatLiteral = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
    floatLiteral->literal.type = type;
    floatLiteral->literal.floatVal = 0;
    floatLiteral->loc = floatToken->loc;
    LexerTokenDelete(floatToken);
    BigintDelete(&intBin);
    BigintDelete(&fracPart);
    BigintDelete(&one);
    BigintDelete(&fracBin);
    return floatLiteral;
  }
  // The value is nonzero, so we can find the first set bit
  int extractIdx, firstSetBit;
  for (extractIdx = lastIdx; bin->arr[extractIdx] == 0; --extractIdx);
  for (firstSetBit = 63; ((bin->arr[extractIdx] >> firstSetBit) & 1) == 0;
       --firstSetBit);

  // Calculate the exponent
  // TODO: add exponent from literal
  bool tooSmall = false;
  if (intPartIs0) {
    // Only fractional part, exponent is negative
    exponentBits = EXPONENT_OFFSET - (lastIdx - extractIdx) * 64 -
                   (64 - firstSetBit);
    if (exponentBits < 0) {
      // Exponent too small, set value to 0
      // TODO: subnormal numbers
      tooSmall = true;
    }
  } else {
    // Has integer part, exponent is positive
    exponentBits = EXPONENT_OFFSET + extractIdx * 64 + firstSetBit;
    if (exponentBits > MAX_EXPONENT) {
      // Exponent too large, out of range
      outOfRange = true;
    }
  }

  // Calculate the remaining number of mantissa bits to set after extracting
  // from the first set bit of the current 64 bits
  int remMantissaBits = NUM_MANTISSA_BITS - 1 - firstSetBit;
  int numRoundedBits;
  if (remMantissaBits > 0) {
    // If the current 64 bits is not enough to fill the mantissa, then
    // we fill in what we can here, and the rest of the mantissa bits should
    // be extracted from the next 64 bits
    mantissaBits |= bin->arr[extractIdx] << remMantissaBits;
    numRoundedBits = 64 - remMantissaBits;
    if (--extractIdx < 0) {
      // If this is the last 64 bits, then we must be extracting from the
      // integer part, because we guaranteed that the floating point part
      // has more than enough precision bits (as long as it is not zero,
      // which we handled as a special case)
      assert (bin == &intBin);
      bin = &fracBin;
      extractIdx = fracBinSize - 1;
    }
  } else {
    // If the current 64 bits is enough to fill in the mantissa, then they
    // will be extracted later
    numRoundedBits = -remMantissaBits;
  }

  // Extract the last bits of the mantissa
  uint64_t last64Bits = bin->arr[extractIdx];
  mantissaBits |= last64Bits >> numRoundedBits;

  // Calculate the rounding
  bool roundUp;
  if (numRoundedBits == 0 && bin == &fracBin) {
    // Mantissa extracted from the last 64 bits of the fraction part, so we
    // should check if the remaining fraction part >/=/< 0.5. In other words,
    // check if the twice the remaining fraction part >/=/< 1
    BigintMulInt(&fracPart, &fracPart, 2);
    int cmp = BigintCmp(&fracPart, &one) == 0;
    if (cmp > 0)
      roundUp = true;
    else if (cmp < 0)
      roundUp = false;
    else
      // Round to even
      roundUp = mantissaBits & 1;
  } else {
    if (numRoundedBits == 0) {
      // No bits are rounded off in the current 64 bits, so the bits that are
      // rounded off must be from the next 64 bits
      if (--extractIdx < 0) {
        // Mantissa extracted from the last 64 bits of the integer part, so the
        // rounding starts from the fraction part
        assert(bin == &intBin);
        bin = &fracBin;
        extractIdx = fracBinSize - 1;
      }
      last64Bits = bin->arr[extractIdx];
    }
    uint64_t roundedBits = last64Bits & ((1LL << numRoundedBits) - 1);
    uint64_t half = 1LL << (numRoundedBits - 1);

    // Compare the rounded off bits to 0.5
    if (roundedBits < half) {
      roundUp = false;
    } else if (roundedBits > half) {
      roundUp = true;
    } else {
      // If the remaining bits are all zero, then the rounded off bits equate
      // to exactly 0.5, so we should round to even, otherwise the rounded off
      // bits is greater than 0.5, and we should round up
      bool allZeros = BigintCmpInt(&fracPart, 0) == 0;
      for (int i = extractIdx - 1; i >= 0; --i) {
        if (bin->arr[i] != 0) {
          allZeros = false;
          break;
        }
      }
      if (bin == &intBin) {
        for (int i = fracBinSize - 1; i >= 0; --i) {
          if (fracBin.arr[i] != 0) {
            allZeros = false;
            break;
          }
        }
      }
      if (allZeros)
        // Round to even
        roundUp = mantissaBits & 1;
      else
        // Round up
        roundUp = true;
    }
  }
  

  // Round the mantissa
  if (roundUp) {
    if (++mantissaBits == (1LL << NUM_MANTISSA_BITS)) {
      // If rounding up makes the mantissa exceed the number of mantissa bits,
      // increase the exponent and make the mantissa 0
      mantissaBits = 0;
      if (++exponentBits > MAX_EXPONENT)
        // Exponent too big
        outOfRange = true;
    }
  }

  // DEBUG
  printf("mantissa: %016llx, exponent: %016llx\n", mantissaBits, exponentBits);

  // Set the floating point value to the bits we calculated
  uint64_t valBin = (exponentBits << 52) | mantissaBits;
  double val;
  memcpy(&val, &valBin, sizeof(val));

  // DEBUG
  printf("val: %016llx (%lf)\n", valBin, val); 

  if (val > maxVal || outOfRange) {
    // TODO: perhaps make this into an error, but would have to handle parser
    //       errors that are generated in the handler
    Parser *parser = rhs->parser;
    Lexer *lexer = parser->lexer;
    SourceLocation *loc = &floatToken->loc;
    fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET" %s:%d: ",
            lexer->filename, loc->from.lineNo + 1);
    fprintf(stderr, "float literal value out of range of %s, converting value"
                    "to inf\n", postfix);
    SourceLocationPrint(lexer->source, 1, SOURCE_COLOR_RED, loc);
    val = INFINITY;
  } else if (tooSmall) {
    // TODO: perhaps make this into an error, but would have to handle parser
    //       errors that are generated in the handler
    Parser *parser = rhs->parser;
    Lexer *lexer = parser->lexer;
    SourceLocation *loc = &floatToken->loc;
    fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET" %s:%d: ",
            lexer->filename, loc->from.lineNo + 1);
    fprintf(stderr, "float literal value out of range of %s, converting value"
                    "to 0\n", postfix);
    SourceLocationPrint(lexer->source, 1, SOURCE_COLOR_RED, loc);
    val = 0;
  }

  // Create the floating point literal node
  SyntaxAST *floatLiteral = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
  floatLiteral->literal.type = type;
  floatLiteral->literal.floatVal = val;
  floatLiteral->loc = floatToken->loc;
  LexerTokenDelete(floatToken);
  BigintDelete(&intBin);
  BigintDelete(&fracPart);
  BigintDelete(&one);
  BigintDelete(&fracBin);
  return floatLiteral;
}

ParserDeclareHandler(SyntaxHandlerTrueLiteral, rhs) {
  SyntaxAST *literal = SyntaxHandlerLiteral(
      rhs, TRUE_LITERAL, SYNTAX_TYPE_BOOL);
  literal->literal.boolVal = true;
  return literal;
}

ParserDeclareHandler(SyntaxHandlerFalseLiteral, rhs) {
  SyntaxAST *literal = SyntaxHandlerLiteral(
      rhs, FALSE_LITERAL, SYNTAX_TYPE_BOOL);
  literal->literal.boolVal = false;
  return literal;
}

ParserDeclareHandler(SyntaxHandlerThisLiteral, rhs) {
  return SyntaxHandlerLiteral(rhs, THIS, SYNTAX_TYPE_THIS);
}

ParserDeclareHandler(SyntaxHandlerNullLiteral, rhs) {
  return SyntaxHandlerLiteral(rhs, NULL_LITERAL, SYNTAX_TYPE_NULL);
}

ParserDeclareHandler(SyntaxHandlerStringLiteral, rhs) {
  assert(rhs->size == 1);
  LexerToken *strToken = rhs->arr[0];
  assert(strToken && strToken->tokenID == STRING_LITERAL);

  char *str = strToken->str + 1;
  int length = strToken->length;
  assert(str[-1] == '"' && str[length - 2] == '"');
  str[length - 2] = '\0';
  char *strVal = malloc(length + 1);
  int i = 0;
  bool isInvalid = false;
  while (*str != '\0') {
    char curChar = SyntaxNextCharacter(&str);
    if (curChar == -1) {
      isInvalid = true;
      ++str;
    } else {
      strVal[i++] = curChar;
    }
  }
  strToken->str[length - 1] = '"';

  if (isInvalid) {
    // TODO: turn this into an error (similar to int literal); furthermore,
    //       point out where the invalid escape sequence is in the error message
    Parser *parser = rhs->parser;
    Lexer *lexer = parser->lexer;
    SourceLocation *loc = &strToken->loc;
    fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET" %s:%d: ",
            lexer->filename, loc->from.lineNo + 1);
    fprintf(stderr, "string literal contains one or more invalid escape "
                    "sequences, ignoring the escape character\n");
    SourceLocationPrint(lexer->source, 1, SOURCE_COLOR_RED, loc);
  }

  SyntaxAST *strLiteral = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
  strLiteral->literal.type = SYNTAX_TYPE_STR;
  strLiteral->literal.strVal = strVal;
  strLiteral->loc = strToken->loc;
  LexerTokenDelete(strToken);
  return strLiteral;
}

ParserDeclareHandler(SyntaxHandlerCharLiteral, rhs) {
  assert(rhs->size == 1);
  LexerToken *charToken = rhs->arr[0];
  assert(charToken && charToken->tokenID == CHAR_LITERAL);

  char *str = charToken->str + 1;
  int length = charToken->length;
  assert(str[-1] == '\'' && str[length - 2] == '\'');
  str[length - 2] = '\0';
  bool isInvalid = false;
  char charVal = SyntaxNextCharacter(&str);
  if (charVal == -1) {
    isInvalid = true;
    charVal = '\0';
  }
  charToken->str[length - 1] = '\'';

  if (isInvalid) {
    // TODO: turn this into an error (similar to int literal); furthermore,
    //       point out where the invalid escape sequence is in the error message
    Parser *parser = rhs->parser;
    Lexer *lexer = parser->lexer;
    SourceLocation *loc = &charToken->loc;
    fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET" %s:%d: ",
            lexer->filename, loc->from.lineNo + 1);
    fprintf(stderr, "string literal contains an invalid escape sequence, "
                    "treating it as a null character\n");
    SourceLocationPrint(lexer->source, 1, SOURCE_COLOR_RED, loc);
  }

  SyntaxAST *charLiteral = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
  charLiteral->literal.type = SYNTAX_TYPE_I8;
  charLiteral->literal.intVal = charVal;
  charLiteral->loc = charToken->loc;
  LexerTokenDelete(charToken);
  return charLiteral;
}

ParserDeclareHandler(SyntaxHandlerVariable, rhs) {
  assert(rhs->size == 1);
  LexerToken *identifier_ = rhs->arr[0];
  assert(identifier_->tokenID == IDENTIFIER);
  return SyntaxTokenToAST(identifier_, SYNTAX_AST_KIND_IDENTIFIER);
}

ParserDeclareHandler(SyntaxHandlerParenExpr, rhs) {
  assert(rhs->size == 3);
  LexerToken *lparen_ = rhs->arr[0];
  SyntaxAST *expr = rhs->arr[1];
  LexerToken *rparen_ = rhs->arr[2];
  assert(lparen_ && lparen_->tokenID == LPAREN);
  assert(expr);
  assert(rparen_ && rparen_->tokenID == RPAREN);

  LexerTokenDelete(lparen_);
  LexerTokenDelete(rparen_);
  return expr;
}

SyntaxAST *SyntaxASTNew(int kind) {
  SyntaxAST *node = malloc(sizeof(SyntaxAST));
  node->firstChild = node->lastChild = node->sibling = NULL;
  node->kind = kind;
  node->loc.from.lineNo = node->loc.from.charNo = INT_MAX;
  node->loc.to.lineNo = node->loc.to.charNo = INT_MIN;
  return node;
}

void SyntaxASTPrepend(SyntaxAST *node, SyntaxAST *child) {
  child->sibling = node->firstChild;
  if (!node->firstChild) {
    node->lastChild = child;
    node->loc.to = SourcePointMax(&node->loc.to, &child->loc.to);
  }
  node->firstChild = child;
  node->loc.from = SourcePointMin(&node->loc.from, &child->loc.from);
}

void SyntaxASTAppend(SyntaxAST *node, SyntaxAST *child) {
  if (node->lastChild)
    node->lastChild->sibling = child;
  else
    node->firstChild = child;
  node->lastChild = child;
  node->loc.from = SourcePointMin(&node->loc.from, &child->loc.from);
  node->loc.to = SourcePointMax(&node->loc.to, &child->loc.to);
}

SyntaxAST *SyntaxTokenToAST(LexerToken *token, int kind) {
  SyntaxAST *node = SyntaxASTNew(kind);
  node->loc = token->loc;
  node->string = strndup(token->str, token->length);
  LexerTokenDelete(token);
  return node;
}

SyntaxAST *SyntaxHandlerSeparatedTokenList(
    ParserRHS *rhs, int separator, int parentKind, int childKind) {
  SyntaxAST *list;
  void *child = NULL;
  if (rhs->size > 1) {
    if (separator == -1) {
      assert(rhs->size == 2);
      child = rhs->arr[1];
    } else {
      assert(rhs->size == 3);
      LexerToken *separatorToken = rhs->arr[1];
      assert(separatorToken && separatorToken->tokenID == separator);
      LexerTokenDelete(separatorToken);
      child = rhs->arr[2];
    }
    list = rhs->arr[0];
  } else {
    list = SyntaxASTNew(parentKind);
    if (rhs->size == 1)
      child = rhs->arr[0];
  }

  if (child) {
    assert(list);
    assert(child);

    if (childKind != -1)
      child = SyntaxTokenToAST(child, childKind);
    SyntaxASTAppend(list, child);
  }
  return list;
}

SyntaxAST *SyntaxHandlerBinaryOp(ParserRHS *rhs, int opTokenID, SyntaxOp op) {
  assert(rhs->size == 3);
  SyntaxAST *expr1 = rhs->arr[0];
  LexerToken *opToken = rhs->arr[1];
  SyntaxAST *expr2 = rhs->arr[2];
  assert(expr1);
  assert(opToken && opToken->tokenID == opTokenID);
  assert(expr2);

  SyntaxAST *opNode = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  opNode->op = op;
  SyntaxASTAppend(opNode, expr1);
  SyntaxASTAppend(opNode, expr2);
  LexerTokenDelete(opToken);
  return opNode;
}

SyntaxAST *SyntaxHandlerUnaryOp(
    ParserRHS *rhs, bool isPrefix, int opTokenID, SyntaxOp op) {
  assert(rhs->size == 2);
  LexerToken *opToken;
  SyntaxAST *expr;
  if (isPrefix) {
    opToken = rhs->arr[0];
    expr = rhs->arr[1];
  } else {
    expr = rhs->arr[0];
    opToken = rhs->arr[1];
  }
  assert(opToken && opToken->tokenID == opTokenID);
  assert(expr);

  SyntaxAST *opNode = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  opNode->op = op;
  SyntaxASTAppend(opNode, expr);
  LexerTokenDelete(opToken);
  return opNode;
}

SyntaxAST *SyntaxHandlerLiteral(
    ParserRHS *rhs, int literalTokenID, SyntaxType type) {
  assert(rhs->size == 1);
  LexerToken *literalToken = rhs->arr[0];
  assert(literalToken && literalToken->tokenID == literalTokenID);

  SyntaxAST *literal = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
  literal->literal.type = type;
  literal->loc = literalToken->loc;
  LexerTokenDelete(literalToken);
  return literal;
}

ParserDeclareHandler(SyntaxHandlerMethodDecls, rhs) {
  return SyntaxHandlerList(rhs, SYNTAX_AST_KIND_METHOD_DECLS);
}

char SyntaxNextCharacter(char **p) {
  char *str = *p;
  if (*str != '\\') {
    char c = *str;
    ++(*p);
    return c;
  }
  char c;
  switch (*(str + 1)) {
    case 'a':
      c = '\a';
      *p += 2;
      break;
    case 'b':
      c = '\b';
      *p += 2;
      break;
    case 'f':
      c = '\f';
      *p += 2;
      break;
    case 'n':
      c = '\n';
      *p += 2;
      break;
    case 'r':
      c = '\r';
      *p += 2;
      break;
    case 't':
      c = '\t';
      *p += 2;
      break;
    case 'v':
      c = '\v';
      *p += 2;
      break;
    case '\\':
      c = '\\';
      *p += 2;
      break;
    case '\'':
      c = '\'';
      *p += 2;
      break;
    case '"':
      c = '"';
      *p += 2;
      break;
    case '0':
      c = '\0';
      *p += 2;
      break;
    default:
      c = -1;
      break;
  }
  return c;
}
