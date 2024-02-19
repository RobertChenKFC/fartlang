#include "parse/syntax/syntax.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  }

  fprintf(file, "\n");
  for (SyntaxAST *child = node->firstChild; child; child = child->sibling)
    PrintAST(file, child, indentation + 4);
}

void runTest(const char *testName) {
  char *str = malloc(strlen(testName) + 6);
  strcpy(str, testName);
  strcat(str, ".fart");
  FILE *file = fopen(str, "r");
  SyntaxAST *node = SyntaxParseFile(file, str);
  fclose(file);

  strcpy(str, testName);
  strcat(str, ".out");
  file = fopen(str, "w");
  PrintAST(file, node, 0);
  fclose(file);

  SyntaxASTDelete(node);
  free(str);
}

int main() {
  runTest("import");
}
