#include "util/source/source.h"
#include <assert.h>

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
    Vector *rhs, int separator, int parentKind, int childKind);
#define SyntaxHandlerList(rhs, parentKind) \
  SyntaxHandlerSeparatedTokenList(rhs, -1, parentKind, -1)
#define SyntaxHandlerSeparatedList(rhs, separator, parentKind) \
  SyntaxHandlerSeparatedTokenList(rhs, separator, parentKind, -1)
#define SyntaxHandlerTokenList(rhs, parentKind) \
  SyntaxHandlerSeparatedTokenList(rhs, -1, parentKind, -1)

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
    Vector *rhs, int separator, int parentKind, int childKind) {
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
