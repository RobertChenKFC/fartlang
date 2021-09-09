#include "parse/cfg/cfg.h"
#include <assert.h>

int main() {
  CFG *cfg = CFGNew();

  // 1. Test EOF token ID and adding variables
  CFGSetNumTokens(cfg, 3);
  int v1 = CFGAddVariable(cfg);
  assert(v1 == 4);
  int v2 = CFGAddVariable(cfg);
  assert(v2 == 5);
  int v3 = CFGAddVariable(cfg);
  assert(v3 == 6);
  int v4 = CFGAddVariable(cfg);
  assert(v4 == 7);
  assert(cfg->eofTokenID == 3);
  assert(cfg->startVariable == v1);
  assert(cfg->numVariables == 4);

  // 2. Test adding rules
  CFGAddRule(cfg, v4, 2, 1, v4);
  CFGAddRule(cfg, v1, 3, v2, 1, v2);
  CFGAddRule(cfg, v3, 2, v3, 0);
  CFGAddRule(cfg, v2, 2, v3, v4);
  CFGAddRule(cfg, v4, 1, 1);
  CFGAddRule(cfg, v1, 3, v2, 0, v2);
  CFGAddRule(cfg, v3, 0);
  CFGFinalize(cfg);
  assert(cfg->variableToRule[0] == 0);
  assert(cfg->variableToRule[1] == 2);
  assert(cfg->variableToRule[2] == 3);
  assert(cfg->variableToRule[3] == 5);
  Vector *tokens = VectorNew();
  VectorAdd(tokens, "a");
  VectorAdd(tokens, "b");
  VectorAdd(tokens, "c");
  Vector *variables = VectorNew();
  VectorAdd(variables, "S");
  VectorAdd(variables, "A");
  VectorAdd(variables, "B");
  VectorAdd(variables, "C");
  FILE *file = fopen("1.out", "w");
  CFGPrint(cfg, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
}
