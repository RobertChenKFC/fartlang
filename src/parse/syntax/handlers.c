#include "util/source/source.h"
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
  LexerToken *rbrace_ = rhs->arr[4];
  assert(class_ && class_->tokenID == CLASS);
  assert(identifier_ && identifier_->tokenID == IDENTIFIER);
  assert(lbrace_ && identifier_->tokenID == LBRACE);
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

ParserDeclareHandler(SyntaxHandlerType, rhs) {
  SyntaxAST *type;
  if (rhs->size == 1) {
    type = rhs->arr[0];
    assert(type);
  } else if (rhs->size == 3) {
    type = rhs->arr[0];
    LexerToken *lbrack_ = rhs->arr[1];
    LexerToken *rbrack_ = rhs->arr[2];
    assert(lbrack_ && lbrack_->tokenID == LBRACK);
    assert(rbrack_ && rbrack_->tokenID == RBRACK);
    ++type->type.arrayLevels;

    LexerTokenDelete(lbrack_);
    LexerTokenDelete(rbrack_);
  } else {
    assert(rhs->size == 4);
    SyntaxAST *retType = rhs->arr[0];
    LexerToken *lbrace_ = rhs->arr[1];
    SyntaxAST *typeList = rhs->arr[2];
    LexerToken *rbrace_ = rhs->arr[3];
    assert(retType);
    assert(lbrace_ && lbrace_->tokenID == LBRACE);
    assert(typeList);
    assert(rbrace_ && rbrace_->tokenID == RBRACE);

    type = SyntaxASTNew(SYNTAX_AST_KIND_TYPE);
    SyntaxASTAppend(type, retType);
    SyntaxASTAppend(type, typeList);
    type->loc.to = rbrace_->loc.to;
    LexerTokenDelete(lbrace_);
    LexerTokenDelete(rbrace_);
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
  assert(rhs->size == 4);
  LexerToken *lParen_ = rhs->arr[0];
  SyntaxAST *type = rhs->arr[1];
  LexerToken *rParen_ = rhs->arr[2];
  SyntaxAST *expr = rhs->arr[3];
  assert(lParen_ && lParen_->tokenID == LPAREN);
  assert(type);
  assert(rParen_ && rParen_->tokenID == RPAREN);
  assert(expr);

  SyntaxAST *op = SyntaxASTNew(SYNTAX_AST_KIND_OP);
  op->loc.from = lParen_->loc.from;
  op->op = SYNTAX_OP_CAST;
  SyntaxASTAppend(op, type);
  SyntaxASTAppend(op, expr);
  LexerTokenDelete(lParen_);
  LexerTokenDelete(rParen_);
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
  uint64_t maxVal;
  SyntaxType type;
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
                    "maximum value %lu\n", postfix, maxVal);
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
  bool outOfRange = false;
  bool exponentOutOfRange = false;

  // Extract the position of the dot; if the dot doesn't exist, then it is
  // considered to be at the end of the base of the float literal
  int dotPos;
  for (dotPos = 0; dotPos < length && '0' <= str[dotPos] && str[dotPos] <= '9';
       ++dotPos);

  // Extract the position of the exponent; if the exponent doesn't exist, then
  // it is considered to be at the end of the base of the float literal
  int expPos;
  for (expPos = (dotPos < length && str[dotPos] == '.') ? dotPos + 1 : dotPos;
       expPos < length && '0' <= str[expPos] && str[expPos] <= '9'; ++expPos);
  int exponent = 0;
  if (expPos < length && str[expPos] == 'e') {
    // Exponent exists, store the exponent as an integer
    int j = expPos + 1;
    bool neg = false;
    if (str[j] == '-') {
      neg = true;
      ++j;
    } else if (str[j] == '+') {
      ++j;
    }
    for (; j < length; ++j) {
      if (exponent > INT_MAX / 10) {
        exponentOutOfRange = true;
        exponent = 0;
        break;
      }
      exponent *= 10;
      int digit = str[j] - '0';
      if (exponent > INT_MAX - digit) {
        exponentOutOfRange = true;
        exponent = 0;
        break;
      }
    }
    if (neg)
      exponent = -exponent;
  }

  // Shift the position of the dot based on the exponent. If after shifting the
  // dot, it is no longer in the base of the float literal, then we shift the
  // dot as far left/right as possible, and store the remaining difference in
  // "exponentDiff"
  int shiftedDotPos = dotPos + exponent, exponentDiff = 0;
  if (shiftedDotPos < 0) {
    exponentDiff = shiftedDotPos;
    shiftedDotPos = 0;
  } else if (shiftedDotPos > expPos) {
    exponentDiff = shiftedDotPos - expPos;
    shiftedDotPos = expPos;
  }

  // Accumulate the value before the dot
  double val = 0;
  for (int i = 0; i < shiftedDotPos; ++i) {
    if (i == dotPos)
      continue;
    val = val * 10 + (str[i] - '0');
    if (isinf(val)) {
      outOfRange = true;
      break;
    }
  }

  // Accumulate the value after the dot
  double power = 0.1;
  for (int i = shiftedDotPos; i < expPos; ++i) {
    if (i == dotPos)
      continue;
    val += power * (str[i] - '0');
    power *= 0.1;
  }

  // Multiply by the exponent if the "shiftedDotPos" was too far left/right
  for (; exponentDiff > 0; --exponentDiff) {
    val *= 10;
    if (isinf(val)) {
      outOfRange = true;
      break;
    }
  }
  for (; exponentDiff < 0; ++exponentDiff)
    val *= 0.1;

  // Extract the type postfix, default if f32 if empty
  char postfix[4] = "f32";
  for (int i = expPos; i < length; ++i) {
    if (str[i] == 'f') {
      char c = str[length];
      str[length] = '\0';
      strcpy(postfix, str + i);
      str[length] = c;
      break;
    }
  }

  // Use the type postfix to determine the maximum value of this type
  double maxVal;
  SyntaxType type;
  static const char *postfixes[] = { "f32", "f64", NULL };
  static const double maxVals[] = { FLT_MAX, DBL_MAX };
  static const SyntaxType types[] = { SYNTAX_TYPE_F32, SYNTAX_TYPE_F64 };
  for (int i = 0; postfixes[i]; ++i) {
    if (strcmp(postfix, postfixes[i]) == 0) {
      maxVal = maxVals[i];
      type = types[i];
      break;
    }
  }

  bool warning = outOfRange || val > maxVal || exponentOutOfRange;
  if (warning) {
    // TODO: perhaps make this into an error (same as int literal)
    Parser *parser = rhs->parser;
    Lexer *lexer = parser->lexer;
    SourceLocation *loc = &floatToken->loc;
    fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET" %s:%d: ",
            lexer->filename, loc->from.lineNo + 1);
    if (exponentOutOfRange) {
      fprintf(stderr, "exponent too big, ignoring exponent\n");
    } else {
      fprintf(stderr, "float literal value out of range of %s, clamping to "
                      "maximum value %lf\n", postfix, maxVal);
      val = maxVal;
    }
    SourceLocationPrint(lexer->source, 1, SOURCE_COLOR_RED, loc);
  }

  SyntaxAST *floatLiteral = SyntaxASTNew(SYNTAX_AST_KIND_LITERAL);
  floatLiteral->literal.type = type;
  floatLiteral->literal.floatVal = val;
  floatLiteral->loc = floatToken->loc;
  LexerTokenDelete(floatToken);
  return floatLiteral;
}

ParserDeclareHandler(SyntaxHandlerTrueLiteral, rhs) {
  SyntaxAST *literal = SyntaxHandlerLiteral(rhs, TRUE, SYNTAX_TYPE_BOOL);
  literal->literal.boolVal = true;
  return literal;
}

ParserDeclareHandler(SyntaxHandlerFalseLiteral, rhs) {
  SyntaxAST *literal = SyntaxHandlerLiteral(rhs, TRUE, SYNTAX_TYPE_BOOL);
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
  str[length - 1] = '\0';
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
  str[length - 1] = '"';

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
  LexerTokenDelete(strToken);
  return strLiteral;
}

ParserDeclareHandler(SyntaxHandlerCharLiteral, rhs) {
  assert(rhs->size == 1);
  LexerToken *charToken = rhs->arr[0];
  assert(charToken && charToken->tokenID == CHAR_LITERAL);

  char *str = charToken->str + 1;
  int length = charToken->length;
  str[length - 1] = '\0';
  bool isInvalid = false;
  char charVal = SyntaxNextCharacter(&str);
  if (charVal == -1) {
    isInvalid = true;
    charVal = '\0';
  }
  str[length - 1] = '\'';

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
  charLiteral->literal.charVal = charVal;
  LexerTokenDelete(charToken);
  return charLiteral;
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
  if (node->lastChild) {
    node->lastChild->sibling = child;
  } else {
    node->firstChild = child;
    node->loc.from = SourcePointMin(&node->loc.from, &child->loc.from);
  }
  node->lastChild = child;
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
  LexerTokenDelete(literalToken);
  return literal;
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
