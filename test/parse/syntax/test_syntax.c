#include "parse/syntax/syntax.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

void PrintChar(FILE *file, char c) {
  switch (c) {
    case '\a':
      fprintf(file, "\\a");
      break;
    case '\b':
      fprintf(file, "\\b");
      break;
    case '\f':
      fprintf(file, "\\f");
      break;
    case '\n':
      fprintf(file, "\\n");
      break;
    case '\r':
      fprintf(file, "\\r");
      break;
    case '\t':
      fprintf(file, "\\t");
      break;
    case '\v':
      fprintf(file, "\\v");
      break;
    case '\\':
      fprintf(file, "\\\\");
      break;
    case '\'':
      fprintf(file, "\\'");
      break;
    case '\"':
      fprintf(file, "\\\"");
      break;
    case '\0':
      fprintf(file, "\\0");
      break;
    default:
      fprintf(file, "%c", c);
      break;
  }
}

void PrintStr(FILE *file, char *str) {
  char c;
  fprintf(file, "\"");
  while ((c = *str)) {
    PrintChar(file, c);
    ++str;
  }
  fprintf(file, "\"");
}

void PrintLiteral(FILE *file, SyntaxAST *node) {
  fprintf(file, "type %s, value ", SYNTAX_TYPE_STRS[node->literal.type]);
  switch (node->literal.type) {
    case SYNTAX_TYPE_U64:
    case SYNTAX_TYPE_I64:
    case SYNTAX_TYPE_U32:
    case SYNTAX_TYPE_I32:
    case SYNTAX_TYPE_U16:
    case SYNTAX_TYPE_I16:
    case SYNTAX_TYPE_U8:
    case SYNTAX_TYPE_I8:
      fprintf(file, "%llu", node->literal.intVal);
      break;
    case SYNTAX_TYPE_F64:
    case SYNTAX_TYPE_F32:
      fprintf(file, "%.*e", LDBL_DECIMAL_DIG - 1, node->literal.floatVal);
      break;
    case SYNTAX_TYPE_BOOL:
      if (node->literal.boolVal)
        fprintf(file, "true");
      else
        fprintf(file, "false");
      break;
    case SYNTAX_TYPE_STR:
      PrintStr(file, node->literal.strVal);
      break;
  }
}

void PrintAST(FILE *file, SyntaxAST *node, int indentation) {
  if (!node)
    return;
  for (int i = 0; i < indentation; ++i)
    fprintf(file, " ");
  fprintf(file, "%s: %d:%d~%d:%d: ", SYNTAX_AST_KIND_STRS[node->kind],
          node->loc.from.lineNo + 1, node->loc.from.charNo + 1,
          node->loc.to.lineNo + 1, node->loc.to.charNo + 1);

  switch (node->kind) {
    case SYNTAX_AST_KIND_IDENTIFIER:
      fprintf(file, "%s: ", node->string);
      break;
    case SYNTAX_AST_KIND_IMPORT_DECL:
      if (node->import.isWildcard)
        fprintf(file, "wildcard: ");
      else if (node->import.namespace)
        fprintf(file, "under namespace \"%s\": ", node->import.namespace);
      break;
    case SYNTAX_AST_KIND_VAR_DECL:
      if (node->varDeclModifiers & SYNTAX_VAR_DECL_STATIC) {
        if (node->varDeclModifiers & SYNTAX_VAR_DECL_CONST)
          fprintf(file, "static and const: ");
        else
          fprintf(file, "static: ");
      } else if (node->varDeclModifiers & SYNTAX_VAR_DECL_CONST) {
        fprintf(file, "const: ");
      }
      break;
    case SYNTAX_AST_KIND_TYPE:
      fprintf(file, "base type: %s, array levels: %d, child types: ",
              SYNTAX_TYPE_STRS[node->type.baseType], node->type.arrayLevels);
      break;
    case SYNTAX_AST_KIND_VAR_INIT:
      fprintf(file, "variable: %s: ", node->string);
      break;
    case SYNTAX_AST_KIND_LITERAL:
      PrintLiteral(file, node);
      break;
  }
  fprintf(file, "\n");
  for (SyntaxAST *child = node->firstChild; child; child = child->sibling)
    PrintAST(file, child, indentation + 4);
}

void PrintFloat(FILE *file, SyntaxAST *node, int indentation) {
  if (!node)
    return;
  if (node->kind == SYNTAX_AST_KIND_LITERAL &&
      (node->literal.type == SYNTAX_TYPE_F64 ||
       node->literal.type == SYNTAX_TYPE_F32)) {
    uint64_t bits;
    memcpy(&bits, &node->literal.floatVal, sizeof(bits));
    fprintf(file, "%llx\n", bits);
    return;
  }
  for (SyntaxAST *child = node->firstChild; child; child = child->sibling)
    PrintFloat(file, child, indentation);
}

typedef void (*PrintFunc)(FILE*, SyntaxAST*, int);
void runTest(const char *testName, PrintFunc print) {
  char *str = malloc(strlen(testName) + 6);
  strcpy(str, testName);
  strcat(str, ".fart");
  FILE *file = fopen(str, "r");
  SyntaxAST *node = SyntaxParseFile(file, str);
  fclose(file);

  strcpy(str, testName);
  strcat(str, ".out");
  file = fopen(str, "w");
  print(file, node, 0);
  fclose(file);

  SyntaxASTDelete(node);
  free(str);
}

void astTest(const char *filename) {
  runTest(filename, PrintAST);
}

void floatTest(const char *filename) {
  runTest(filename, PrintFloat);
}

int main(void) {
  // DEBUG
  // astTest("import");
  // astTest("vardecl");
  floatTest("float");
  // astTest("term");
}
