#ifndef CFG_H
#define CFG_H

#include "util/vector/vector.h"
#include <stdio.h>

// Constants
// Check if "id" is a token ID or a variable ID in "cfg"
#define CFG_IS_TOKEN_ID(cfg, id) ((id) <= (cfg)->numTokens)
// Change variable ID to the index of the variable ID (when all variable indices
// are arranged in ascending order of their variable IDs)
#define CFG_VARIABLE_ID_TO_IDX(cfg, id) ((id) - (cfg)->numTokens - 1)

// Forward declarations
typedef struct CFGRule CFGRule;
typedef struct CFG CFG;

// A rule in the CFG, which represents a way to replace its LHS with its RHS
struct CFGRule {
  int lhs;
  int *rhs;
  int numRHS;
};

// A CFG, which contains an array of CFG rules, as well as keeping track of the
// number of variables and the number of tokens; note that the token ID of the
// EOF token is the same as the number of tokens
struct CFG {
  Vector *rules;
  int numVariables;
  union {
    int numTokens;
    int eofTokenID;
  };
  int startVariable;
  int *variableToRule;
};

// Creates a new CFG with no rules
CFG *CFGNew();
// Deletes "cfg" created with CFGNew
void CFGDelete(CFG *cfg);
// Sets the number of tokens of "cfg" to "numTokens"; note that this function
// must be called before any variables are created
void CFGSetNumTokens(CFG *cfg, int numTokens);
// Returns a new variable ID to corresponding to a newly added variable in
// "cfg"; note that the first added variable will also be the start variable
int CFGAddVariable(CFG *cfg);
// Adds a new rule created from "lhs" and "n" RHS items to "cfg"; note that
// an RHS item can be a variable ID or a token ID; returns the newly added rule
CFGRule *CFGAddRule(CFG *cfg, int lhs, int n, ...);
// Finalizes "cfg" by:
// (1) Sorting the rules according to their LHS varaible IDs, then by the length
// of the RHS, then, from left to right, by the RHS token or variable IDs
// (2) Creating a mapping from the variable ID to the first rule in the array
// whose LHS is that variable ID
// Note that this function should only be called once on a given "cfg"
void CFGFinalize(CFG *cfg);
// Print "rule" in "cfg" to file by replacing each token (in ascending order of
// token ID) with a string in "tokens", and each variable (in ascending order of
// variable ID) with a string in "variables"; note that this function does
// not print a file newline character
void CFGRulePrint(
    CFG *cfg, CFGRule *rule, Vector *tokens, Vector *variables, FILE *file);
// Print all CFG rules in "cfg", each on a single line, to "file"
void CFGPrint(CFG *cfg, Vector *tokens, Vector *variables, FILE *file);

#endif // CFG_H
