#include "parse/syntax/syntax.h"
#include <stdio.h>

int main() {
  FILE *file = fopen("test.fart", "r");
  SyntaxParseFile(file, "test.fart");
  fclose(file);
}
