#include "parse/cfg/cfg.h"
#include <stdlib.h>
#include <stdarg.h>

// Helper function
// Deletes "rule" created from CFGRuleNew
void CFGRuleDelete(CFGRule *rule);
// Compares two CFGRules "a" and "b", first by their LHS variable IDs, then
// by the length of their RHS, then, from left to right, by the RHS token or
// variable ID
int CFGRuleCmp(const void *a, const void *b);

CFG *CFGNew() {
  CFG *cfg = malloc(sizeof(CFG));
  cfg->rules = VectorNew();
  cfg->numVariables = 0;
  cfg->variableToRule = NULL;
  return cfg;
}

void CFGDelete(CFG *cfg) {
  Vector *rules = cfg->rules;
  int numRules = rules->size;
  for (int i = 0; i < numRules; ++i)
    CFGRuleDelete(rules->arr[i]);
  VectorDelete(rules);
  free(cfg->variableToRule);
  free(cfg);
}

void CFGSetNumTokens(CFG *cfg, int numTokens) {
  cfg->numTokens = numTokens;
}

int CFGAddVariable(CFG *cfg) {
  int variable = cfg->numTokens + (++cfg->numVariables);
  if (cfg->numVariables == 1)
    cfg->startVariable = variable;
  return variable;
}

CFGRule *CFGAddRule(CFG *cfg, int lhs, int n, ...) {
  CFGRule *rule = malloc(sizeof(CFGRule));
  rule->lhs = lhs;
  if (n > 0)
    rule->rhs = malloc(sizeof(int) * n);
  else
    rule->rhs = NULL;
  rule->numRHS = n;
  va_list list;
  va_start(list, n);
  for (int i = 0; i < n; ++i)
    rule->rhs[i] = va_arg(list, int);
  va_end(list);
  VectorAdd(cfg->rules, rule);
  return rule;
}

void CFGRuleDelete(CFGRule *rule) {
  free(rule->rhs);
  free(rule);
}

int CFGRuleCmp(const void *a, const void *b) {
  const CFGRule *rule1 = *((const CFGRule **)a);
  const CFGRule *rule2 = *((const CFGRule **)b);
  if (rule1->lhs < rule2->lhs)
    return -1;
  if (rule1->lhs > rule2->lhs)
    return 1;
  if (rule1->numRHS < rule2->numRHS)
    return -1;
  if (rule1->numRHS > rule2->numRHS)
    return 1;
  int numRHS = rule1->numRHS;
  int *rhs1 = rule1->rhs, *rhs2 = rule2->rhs;
  for (int i = 0; i < numRHS; ++i) {
    if (rhs1[i] < rhs2[i])
      return -1;
    if (rhs1[i] > rhs2[i])
      return 1;
  }
  return 0;
}

void CFGFinalize(CFG *cfg) {
  Vector *rules = cfg->rules;
  qsort(rules->arr, rules->size, sizeof(rules->arr[0]), CFGRuleCmp);

  int *variableToRule = malloc(sizeof(int) * cfg->numVariables);
  int numRules = rules->size;
  int prevLHS = -1;
  for (int i = 0, j = 0; i < numRules; ++i) {
    CFGRule *rule = rules->arr[i];
    int lhs = CFG_VARIABLE_ID_TO_IDX(cfg, rule->lhs);
    if (lhs != prevLHS) {
      while (j < lhs) {
        variableToRule[j] = 0;
        ++j;
      }
      variableToRule[j++] = i;
      prevLHS = lhs;
    }
  }
  cfg->variableToRule = variableToRule;
}

void CFGRulePrint(
    CFG *cfg, CFGRule *rule, Vector *tokens, Vector *variables, FILE *file) {
  fprintf(file, "%s ->",
          (const char*)variables->arr[CFG_VARIABLE_ID_TO_IDX(cfg, rule->lhs)]);
  int *rhs = rule->rhs;
  if (rhs) {
    int numRHS = rule->numRHS;
    for (int j = 0; j < numRHS; ++j) {
      int id = rhs[j];
      if (CFG_IS_TOKEN_ID(cfg, id))
        fprintf(file, " %s", (const char*)tokens->arr[id]);
      else
        fprintf(file, " %s",
                (const char*)variables->arr[CFG_VARIABLE_ID_TO_IDX(cfg, id)]);
    }
  } else {
    fprintf(file, " ε");
  }
}

void CFGPrint(CFG *cfg, Vector *tokens, Vector *variables, FILE *file) {
  Vector *rules = cfg->rules;
  int numRules = rules->size;
  for (int i = 0; i < numRules; ++i) {
    CFGRule *rule = rules->arr[i];
    CFGRulePrint(cfg, rule, tokens, variables, file);
    fprintf(file, "\n");
  }
}

