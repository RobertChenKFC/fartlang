#include <assert.h>

// Creates a new SyntaxAST node and sets the fields to what is provided
SyntaxAST *SyntaxASTNew(
    SyntaxAST *firstChild, SyntaxAST *lastChild, SyntaxAST *sibling, int kind);
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

ParserDeclareHandler(SyntaxHandlerModule, rhs) {
  assert(rhs->size == 2);
  ParserObj *rhs0 = rhs->arr[0], *rhs1 = rhs->arr[1];
  assert(rhs && rhs0->type == PARSER_OBJECT_OBJECT);
  assert(rhs1 && rhs1->type == PARSER_OBJECT_OBJECT);
  SyntaxAST *importDecls = rhs0->object, *classDecls = rhs1->object;
  assert(importDecls);
  assert(classDecls);

  importDecls->sibling = classDecls;
  SyntaxAST *module = SyntaxASTNew(importDecls, NULL, SYNTAX_AST_KIND_MODULE);
  module->loc.from = importDecls->loc.from;
  module->loc.to = classDecls->loc.to;
  return module;
}

ParserDeclareHandler(SyntaxHandlerImportDecls, rhs) {
  if (rhs->size == 2) {
    ParserObject *rhs0 = rhs->arr[0], *rhs1 = rhs->arr[1];
    assert(rhs0 && rhs0->type == PARSER_OBJECT_OBJECT);
    assert(rhs1 && rhs1->type == PARSER_OBJECT_OBJECT);
    SyntaxAST *importDecls = rhs0->object, *importDecl = rhs1->object;
    assert(importDecls);
    assert(importDecl);

    SyntaxASTPrepend(importDecls, importDecl);
  } else {
    assert(rhs->size == 0);
    return SyntaxASTNew(NULL, NULL, SYNTAX_AST_KIND_IMPORT_DECLS);
  }
}

ParserDeclareHandler(SyntaxHandlerImportDecl, rhs) {
  assert(rhs->size == 4);
  ParserObject *rhs0 = rhs->arr[0], *rhs1 = rhs->arr[1], *rhs2 = rhs->arr[2],
      *rhs3 = rhs->arr[3];
  assert(rhs0 && rhs0->type == PARSER_OBJECT_TOKEN);
  assert(rhs1 && rhs1->type == PARSER_OBJECT_OBJECT);
  assert(rhs2 && rhs2->type == PARSER_OBJECT_OBJECT);
  assert(rhs3 && rhs3->type == PARSER_OBJECT_TOKEN);
  LexerToken *import_ = rhs0->token, *semicol_ = rhs3->token;
  SyntaxAST *modulePath = rhs1->object, *modulePathExt = rhs->object;
  assert(import_);
  assert(modulePath);
  assert(modulePathExt);
  assert(semicol_);

  modulePath->sibling = modulePathExt;
  SyntaxAST *importDecl = SyntaxASTNew(
      modulePath, NULL, SYNTAX_AST_KIND_IMPORT_DECL);

  LexerTokenDelete(import_);
  LexerTokenDelete(semicol_);
  return importDecl;
}

ParserDeclareHandler(SyntaxHandlerModulePath, rhs) {
  SyntaxAST *node;
  if (rhs->size == 3) {
    ParserObject *rhs0 = rhs->arr[0], *rhs1 = rhs->arr[1], *rhs2 = rhs->arr[2];
    assert(rhs0 && rhs0->type == PARSER_OBJECT_OBJECT);
    SyntaxAST *modulePath = rhs0->object;
    assert(rhs1 && rhs1->type == PARSER_OBJECT_TOKEN);
    LexerToken *token = rhs1->token;
    assert(token && token->tokenID == DOT);
    assert(rhs2 && rhs2->type == PARSER_OBJECT_TOKEN);
    LexerToken *identifier = rhs2->token;
    assert(identifier && identifier->tokenID == IDENTIFIER);

    SyntaxASTAppend(modulePath, SyntaxTokenToAST(identifier));
    node = modulePath;
  } else {
    assert(rhs->size == 1);
    ParserObject *rhs0 = rhs->arr[0];
    assert(rhs0 && rhs0->type == PARSER_OBJECT_TOKEN);
    Token *identifier = rhs0->token;
    assert(identifier && identifier->tokenID == IDENTIFIER);

    SyntaxASTNode *child = SyntaxTokenToAST(identifier);
    node = SyntaxASTNew(child, child, NULL, SYNTAX_AST_KIND_MODULE_PATH);
  }
  return node;
}

ParserDeclareHandler(SyntaxHandlerModulePathExt, rhs) {
  SyntaxAST *node = SyntaxASTNew(NULL, NULL, NULL, SYNTAX_AST_KIND_IMPORT_DECL);
  if (rhs->size == 2) {
    ParserObject *rhs0 = rhs->arr[0], *rhs1 = rhs->arr[1];
    assert(rhs0 && rhs0->type == PARSER_OBJECT_TOKEN);
    Token *token = rhs0->token;
    if (token->tokenID == AS) {
    } else {
      assert(token->tokenID == DOT);
      LexerTokenDelete()
    }

    // TODO: continue here: finish the implementation for ModulePathExt, which
    // is supposed to return an importDecl AST node so that this node can be
    // updated by the importDecl handler to contain the modulePath in its child;
    // HOWEVER, before doing this, there seems to be a HUGE problem with
    // ParserObject, and the parser test seem to be wrong, since it does not
    // delete the ParserObject created by ParserParse, even though it should.
    // Furthermore, the handlers should be returning ParserObject so that the
    // ParserParse function can distinguish between tokens and user objects
    // and can call destructors correctly in the event of a syntax error, but
    // this is not what is done here, nor is it done properly in the parser
    // test. Therefore, I think the best way to handle this is to just scratch
    // ParserObject entirely, and have ParserParse handle the object deletion
    // in another way, so do this before continuing here.
    
      ParserAddRuleAndHander(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH, 2, AS, IDENTIFIER);
      ParserAddRuleAndHander(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH, 2, DOT, MUL);
  } else {
    assert(rhs->size == 0);
    node->import.alias = NULL;
    node->import.isWildcard = false;
  }
  return node;
}

SyntaxAST *SyntaxASTNew(
    SyntaxAST *firstChild, SyntaxAST *lastChild, SyntaxAST *sibling, int kind) {
  SyntaxAST *node = malloc(sizeof(SyntaxAST));
  node->firstChild = firstChild;
  node->lastChild = lastChild;
  node->sibling = sibling;
  node->kind = kind;
  return node;
}

void SyntaxASTPrepend(SyntaxAST *node, SyntaxAST *child) {
  child->sibling = node->firstChild;
  if (!node->firstChild) {
    node->lastChild = child;
    node->loc.to = child->loc.to;
  }
  node->firstChild = child;
  node->loc.from = child->loc.from;
}

void SyntaxASTAppend(SyntaxAST *node, SyntaxAST *child) {
  if (node->lastChild) {
    node->lastChild->sibling = child;
  } else {
    node->firstChild = child;
    node->loc.from = child->loc.from;
  }
  node->lastChild = child;
  node->loc.to = child->loc.to;
}

SyntaxAST *SyntaxTokenToAST(LexerToken *token, int kind) {
  SyntaxAST *node = SyntaxASTNew(NULL, NULL, kind);
  char *string = malloc(token->length + 1);
  strncpy(string, token->str, token->length);
  string[token->length] = '\0';
  node->string = string;
  LexerTokenDelete(token);
  return node;
}
