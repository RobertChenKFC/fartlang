#include "parse/parser/parser.h"
#include "parse/lr1/lr1.h"
#include "util/hashtable/hashtable.h"
#include "util/source/source.h"
#include <limits.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

// Constants
// Returns true if and only if "action" is a shift action
#define PARSER_IS_SHIFT_ACTION(action) ((action) > 0)
// Convert state "number" to shift action
#define PARSER_STATE_NO_TO_SHIFT_ACTION(stateNo) ((stateNo) + 1)
// Convert shift "action" to state number
#define PARSER_SHIFT_ACTION_TO_STATE_NO(action) ((action) - 1)
// Convert rule "number" to reduction action
#define PARSER_RULE_NO_TO_REDUCE_ACTION(ruleNo) (-(ruleNo) - 1)
// Convert reduce "action" to the rule number to reduce
#define PARSER_REDUCE_ACTION_TO_RULE_NO(action) (-1 - (action))
int PARSER_OBJECT_FAILURE_INSTANCE;
void *PARSER_OBJECT_FAILURE = &PARSER_OBJECT_FAILURE_INSTANCE;

// Forward declarations
typedef struct ParserLookaheadAction ParserLookaheadAction;

// A struct that packs the parser "action" (shift or reduce action) and its
// corresponding "lookahead" together
struct ParserLookaheadAction {
  int lookahead;
  int action;
};

// Helper functions
// Construct a parser object from a "token"
ParserObject *ParserObjectFromToken(LexerToken *token);
// Construct a parser object from a custom user "object"
ParserObject *ParserObjectFromObject(void *object);
// Deletes a parser "object" using different methods depending on its type: if
// it is a token type, then deletes its contents with LexerTokenDelete; if
// it is a custom user object type, then deletes its contents with "destructor"
void ParserObjectDelete(
    ParserObject *object, ParserObjectDestructor destructor);
// A hash function for a pointer "a"; used in hash tables
uint64_t ParserPtrHash(void *a);
// An equal function for pointers "a" and "b"; used in hash tables
bool ParserPtrEqual(void *a, void *b);
// A hash function for LR1State "a", where each state with the same LR(0)
// items returns the same hash value
uint64_t ParserLALR1StateHash(void *a);
// An equal function for LR1State "a" and "b", where "a" and "b" are considered
// equal when "a" and "b" contain the same set of LR(0) items
bool ParserLALR1StateEqual(void *a, void *b);
// Prints an error message regarding the current "token", and cleanup all
// custom objects in "stack" with "destructor", and all tokens in "stack"
// with LexerTokenDelete, then deletes "stack" and "currentRHS"; requires giving
// the "lexer" that lexed the "token"
void ParserSyntaxError(
    Lexer *lexer, LexerToken *token, Vector *stack,
    ParserObjectDestructor destructor, Vector *currentRHS);
// Creates a packed ParserLookaheadAction containing "lookahead" and "action"
ParserLookaheadAction *ParserLookaheadActionNew(int lookahead, int action);
// Compare function for ParserLookaheadAction, which compares two of them
// first using their lookaheads, then using their actions; can be used in qsort
int ParserLookaheadActionCmp(const void *a, const void *b);

ParserObject *ParserObjectFromToken(LexerToken *token) {
  ParserObject *parserObj = malloc(sizeof(ParserObject));
  parserObj->token = token;
  parserObj->type = PARSER_OBJECT_TOKEN;
  return parserObj;
}

ParserObject *ParserObjectFromObject(void *object) {
  ParserObject *parserObj = malloc(sizeof(ParserObject));
  parserObj->object = object;
  parserObj->type = PARSER_OBJECT_OBJECT;
  return parserObj;
}

void ParserObjectDelete(
    ParserObject *object, ParserObjectDestructor destructor) {
  switch (object->type) {
    case PARSER_OBJECT_TOKEN:
      LexerTokenDelete(object->token);
      break;
    case PARSER_OBJECT_OBJECT:
      destructor(object->object);
      break;
  }
  free(object);
}

uint64_t ParserPtrHash(void *a) {
  return (uint64_t)a;
}

bool ParserPtrEqual(void *a, void *b) {
  return a == b;
}

uint64_t ParserLALR1StateHash(void *a) {
  LR1State *lr1State = a;
  HashTable *lr1Items = lr1State->items;
  uint64_t hash = 0;
  HashTableHashFunction symbolStringHash = lr1Items->hash;
  for (HashTableEntry *entry = lr1Items->head; entry;
       entry = entry->nextInTable) {
    LR1SymbolString *symbolString = entry->key;
    hash ^= symbolStringHash(symbolString);
  }
  return hash;
}

bool ParserLALR1StateEqual(void *a, void *b) {
  LR1State *lr1State1 = a;
  LR1State *lr1State2 = b;
  HashTable *lr1Items1 = lr1State1->items;
  HashTable *lr1Items2 = lr1State2->items;
  if (lr1Items1->size != lr1Items2->size)
    return false;
  for (HashTableEntry *entry1 = lr1Items1->head; entry1;
       entry1 = entry1->nextInTable) {
    LR1SymbolString *symbolString = entry1->key;
    HashTableEntry *entry2 = HashTableEntryRetrieve(lr1Items2, symbolString);
    if (!entry2)
      return false;
  }
  return true;
}

ParserConfig *ParserConfigNew(
    Lexer *lexer, CFG *cfg, bool useLALR1, ParserObjectDestructor destructor) {
  ParserConfig *config = malloc(sizeof(ParserConfig));
  config->lexer = lexer;
  config->cfg = cfg;
  config->handlers = VectorNew();
  config->handlerNames = VectorNew();
  config->useLALR1 = useLALR1;
  config->destructor = destructor;
  config->lhsStrings = VectorNew();
  config->rhsStrings = VectorNew();
  config->conflictResolution = VectorNew();
  return config;
}

void ParserConfigAddResolution(
    ParserConfig *config, int conflictNum, int option) {
  Vector *conflictResolution = config->conflictResolution;
  while (conflictNum >= conflictResolution->size)
    VectorAdd(conflictResolution, (void*)(long long)-1);
  conflictResolution->arr[conflictNum] = (void*)(long long)option;
}

Parser *ParserFromConfig(ParserConfig *config) {
  // Extract everything from the parser config and delete it
  Lexer *lexer = config->lexer;
  CFG *cfg = config->cfg;
  Vector *handlers = config->handlers;
  Vector *handlerNames = config->handlerNames;
  bool useLALR1 = config->useLALR1;
  ParserObjectDestructor destructor = config->destructor;
  Vector *lhsStrings = config->lhsStrings;
  Vector *rhsStrings = config->rhsStrings;
  Vector *conflictResolution = config->conflictResolution;
  free(config);

  // Parse the CFG RHS and RHS strings to extract the token strings
  int totalLength = 0;
  int numRhsStrings = rhsStrings->size;
  for (int i = 0; i < numRhsStrings; ++i) {
    char *rhsString = rhsStrings->arr[i];
    totalLength += strlen(rhsString) + 1;
  }
  char *concatRhsString = malloc(sizeof(char) * totalLength);
  char **varStrings = malloc(sizeof(char*) * (cfg->numVariables + 1));
  char **tokenStrings = malloc(sizeof(char*) * (cfg->numTokens + 1));
  bool inExpectedFormat = true;
  Vector *rules = cfg->rules;
  for (int i = 0, offset = 0; i < numRhsStrings; ++i) {
    CFGRule *rule = rules->arr[i];
    int lhs = rule->lhs;
    int lhsId = CFG_VARIABLE_ID_TO_IDX(cfg, lhs);
    varStrings[lhsId] = lhsStrings->arr[i];
    int *rhs = rule->rhs;
    int numRhs = rule->numRHS;
    char *rhsString = rhsStrings->arr[i], c;
    int idx = -1;
    rhsStrings->arr[i] = concatRhsString + offset;
    for (int j = 0; (c = rhsString[j]) != '\0'; ++j) {
      if (j == 0 || rhsString[j - 1] == ',') {
        ++idx;
        if (j != 0)
          concatRhsString[offset++] = '\0';
        if (idx >= numRhs) {
          inExpectedFormat = false;
        } else {
          int id = rhs[idx];
          if (CFG_IS_TOKEN_ID(cfg, id))
            tokenStrings[id] = concatRhsString + offset;
        }
      }
      if (c != ',' && !isspace(c))
        concatRhsString[offset++] = c;
    }
    concatRhsString[offset++] = '\0';
  }

  // Assigning each rule in the CFG to a number. The rule numbers must be
  // assigned first before calling LR1StateGraphFromCFG, which finalizes the cfg
  // and sorts its rules, thus changing the order of the rules and thus ruining
  // its relation with the order of the handlers.
  HashTable *toRuleNo = HashTableNew(
      ParserPtrHash, ParserPtrEqual, NULL, NULL);
  int numRules = rules->size + 1;
  int *lhses = malloc(sizeof(int) * numRules);
  int *numRHSes = malloc(sizeof(int) * numRules);
  for (int i = 0; i < numRules - 1; ++i) {
    CFGRule *rule = rules->arr[i];
    HashTableEntryAdd(toRuleNo, rule, (void*)(long long)i);
    lhses[i] = rule->lhs;
    numRHSes[i] = rule->numRHS;
  }

  // Generating the LR(1) state graph from the cfg
  LR1StateGraph *graph = LR1StateGraphFromCFG(cfg);

  // However, since the auxiliary rule $accept -> start_variable $end is also
  // added after the call to LR1StateGraphFromCFG, we must also assign its rule
  // number after the call. Also, we must update "lhsStrings" and "rhsStrings"
  // to reflect this rule as well.
  char *auxiliaryRuleRhsString;
  for (int i = 0; i < numRules; ++i) {
    CFGRule *rule = rules->arr[i];
    HashTableEntry *ruleEntry = HashTableEntryRetrieve(toRuleNo, rule);
    if (!ruleEntry) {
      HashTableEntryAdd(toRuleNo, rule, (void*)(long long)(numRules - 1));
      int lhs = rule->lhs;
      lhses[numRules - 1] = lhs;
      varStrings[CFG_VARIABLE_ID_TO_IDX(cfg, lhs)] = "$accept";

      assert(rule->numRHS == 2);
      numRHSes[numRules - 1] = rule->numRHS;
      int *rhs = rule->rhs;
      int startVar = rhs[0];
      int startVarId = CFG_VARIABLE_ID_TO_IDX(cfg, startVar);
      char *startVarStr = varStrings[startVarId];
      int startVarStrLen = strlen(startVarStr);
      // start_variable + '\0' + $end + '\0'
      auxiliaryRuleRhsString = malloc(startVarStrLen + 6);
      strcpy(auxiliaryRuleRhsString, startVarStr);
      strcpy(auxiliaryRuleRhsString + startVarStrLen + 1, "$end");
      VectorAdd(lhsStrings, "$accept");
      VectorAdd(rhsStrings, auxiliaryRuleRhsString);
      tokenStrings[cfg->eofTokenID] = "$end";
      break;
    }
  }

  // Assigning each state in the graph to a number. Note that multiple states
  // may be assigned to the same number (such as in an LALR(1) state graph,
  // where all states with the same kernel (LR(0) items; LR(1) items without
  // lookaheads) are considered the same state, thus they will be assigned the
  // same state number as well). To traverse all the states with the same state
  // number, a vector "stateNoToStateEntry" acts as a table to map each state
  // number to a HashTableEntry, whose key is the state, and value is the next
  // entry. We are basically reusing the unused values in the "states" hash
  // table of the LR(1) state "graph".
  HashTable *states = graph->states;
  HashTable *toStateNo;
  if (useLALR1) {
    toStateNo = HashTableNew(
        ParserLALR1StateHash, ParserLALR1StateEqual, NULL, NULL);
  } else {
    toStateNo = HashTableNew(
        ParserPtrHash, ParserPtrEqual, NULL, NULL);
  }
  Vector *stateNoToStateEntry = VectorNew();
  for (HashTableEntry *entry = states->head; entry;
       entry = entry->nextInTable) {
    LR1State *state = entry->key;
    HashTableEntry *stateNoEntry = HashTableEntryRetrieve(toStateNo, state);
    if (stateNoEntry) {
      int stateNo = (int)(long long)stateNoEntry->value;
      HashTableEntry *prevEntry = stateNoToStateEntry->arr[stateNo];
      entry->value = prevEntry;
      stateNoToStateEntry->arr[stateNo] = entry;
    } else {
      int stateNo = stateNoToStateEntry->size;
      HashTableEntryAdd(toStateNo, state, (void*)(long long)stateNo);
      entry->value = NULL;
      VectorAdd(stateNoToStateEntry, entry);
    }
  }
  int numStates = stateNoToStateEntry->size;

  Parser *parser = NULL;
  Vector *transitionStateFroms = VectorNew();
  Vector *transitionStateTos = VectorNew();
  bool errorOccurred = false;
  // To avoid constantly allocating and freeing ParserLookaheadAction, we save
  // the ParserLookaheadAction in "lookaheadActions" without deleting them,
  // while keeping track of how many we are actually used in the current
  // iteration using "numLookaheadActions"
  Vector *lookaheadActions = VectorNew();
  int *transitionOffsets = malloc(sizeof(int) * numStates);
  // "freePos" is the first position in the state table that is free
  int freePos = 0;
  int conflictNum = 0;
  for (int from = 0; from < numStates; ++from) {
    // Collect all (1) transitions and (2) reductions
    int numLookaheadActions = 0;
    for (HashTableEntry *stateEntry = stateNoToStateEntry->arr[from];
         stateEntry; stateEntry = stateEntry->value) {
      LR1State *stateFrom = stateEntry->key;
      // (1) Transitions
      for (LR1Transition *transition = stateFrom->transition; transition;
           transition = transition->next) {
        int lookahead = transition->id;
        LR1State *stateTo = transition->state;
        HashTableEntry *stateToEntry = HashTableEntryRetrieve(
            toStateNo, stateTo);
        assert(stateToEntry);
        int to = (int)(long long)stateToEntry->value;
        int action = PARSER_STATE_NO_TO_SHIFT_ACTION(to);
        ParserLookaheadAction *lookaheadAction;
        if (numLookaheadActions < lookaheadActions->size) {
          lookaheadAction = lookaheadActions->arr[numLookaheadActions];
          lookaheadAction->lookahead = lookahead;
          lookaheadAction->action = action;
        } else {
          lookaheadAction = ParserLookaheadActionNew(lookahead, action);
          VectorAdd(lookaheadActions, lookaheadAction);
        }
        ++numLookaheadActions;
      }
      HashTable *items = stateFrom->items;
      // (2) Reductions
      for (HashTableEntry *itemEntry = items->head; itemEntry;
           itemEntry = itemEntry->nextInTable) {
        LR1SymbolString *string = itemEntry->key;
        CFGRule *rule = string->rule;
        if (string->dot == rule->numRHS) {
          Vector *lookaheads = itemEntry->value;
          int numLookaheads = lookaheads->size;
          for (int i = 0; i < numLookaheads; ++i) {
            int lookahead = (int)(long long)lookaheads->arr[i];
            HashTableEntry *ruleEntry = HashTableEntryRetrieve(toRuleNo, rule);
            assert(ruleEntry);
            int ruleNo = (int)(long long)ruleEntry->value;
            int action = PARSER_RULE_NO_TO_REDUCE_ACTION(ruleNo);
            ParserLookaheadAction *lookaheadAction;
            if (numLookaheadActions < lookaheadActions->size) {
              lookaheadAction = lookaheadActions->arr[numLookaheadActions];
              lookaheadAction->lookahead = lookahead;
              lookaheadAction->action = action;
            } else {
              lookaheadAction = ParserLookaheadActionNew(lookahead, action);
              VectorAdd(lookaheadActions, lookaheadAction);
            }
            ++numLookaheadActions;
          }
        }
      }
    }

    // If there are no transitions and no reductions from this state, skip to
    // the next state
    if (numLookaheadActions == 0) {
      transitionOffsets[from] = 0;
      continue;
    }

    // Sort the actions according to their lookaheads so that actions with the
    // same lookaheads are bunched together
    qsort(
        lookaheadActions->arr, numLookaheadActions,
        sizeof(lookaheadActions->arr[0]), ParserLookaheadActionCmp);

    // Remove any duplicate lookahead actions that might have come from merging
    // states when constructing an LALR(1) parser
    ParserLookaheadAction *prevLookaheadAction = lookaheadActions->arr[0];
    int newNumLookaheadActions = 1;
    for (int i = 1; i < numLookaheadActions; ++i) {
      ParserLookaheadAction *lookaheadAction = lookaheadActions->arr[i];
      if (ParserLookaheadActionCmp(&lookaheadAction,
                                   &prevLookaheadAction) != 0) {
        lookaheadActions->arr[i] =
            lookaheadActions->arr[newNumLookaheadActions];
        lookaheadActions->arr[newNumLookaheadActions++] = lookaheadAction;
        prevLookaheadAction = lookaheadAction;
      }
    }
    numLookaheadActions = newNumLookaheadActions;

    // Check each position in the state table, starting from the position
    // "max(freePos - minID, 0)", which position is "free". A position is free
    // if the position offset by each lookahead is free in the state table
    ParserLookaheadAction *lookaheadAction = lookaheadActions->arr[0];
    int minID = lookaheadAction->lookahead;
    int curFreePos = freePos - minID;
    if (curFreePos < 0)
      curFreePos = 0;
    for (;; ++curFreePos) {
      bool posIsFree = true;
      for (int i = 0; i < numLookaheadActions; ++i) {
        lookaheadAction = lookaheadActions->arr[i];
        int pos = curFreePos + lookaheadAction->lookahead;
        if (pos < transitionStateTos->size &&
            (int)(long long)transitionStateTos->arr[pos] !=
            PARSER_TRANSITION_STATE_NONE) {
          posIsFree = false;
          break;
        }
      }
      if (posIsFree)
        break;
    }

    // Add each action to the state table; "start" records the first action that
    // uses the current lookahead
    int start = 0;
    bool hasConflicts = false;
    for (int i = 0; i < numLookaheadActions; ++i) {
      lookaheadAction = lookaheadActions->arr[i];
      int lookahead = lookaheadAction->lookahead;
      int chosenAction;
      // If the i-th lookahead action is the last action corresponding to this
      // lookahead
      if (i == numLookaheadActions - 1 ||
          ((ParserLookaheadAction*)lookaheadActions->arr[i + 1])->lookahead !=
          lookahead) {
        if (i == start) {
          // Only one action corresponding to this lookahead, so no conflicts
          chosenAction = lookaheadAction->action;
        } else {
          // Multiple actions corresponding to this lookahead, so there is a
          // conflict
          if (!hasConflicts) {
            if (conflictNum == 0 && !inExpectedFormat) {
              fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET
                      " There are conflicts in the parser, so there will be "
                      "additional warning messages describing the conflicts. "
                      "However, the warning messages will only output properly "
                      "if the source code is in the correct format (the LHS "
                      "and RHS arguments to ParserAddRuleAndHandler are single "
                      "variables), and we have detected an incorrect format, "
                      "so the following warning messages may be garbled.\n");
            }
            // The first conflict, so print the state item set first
            fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET
                    " Conflict on state %d. This state contains the following "
                    "items:\n", from);
            HashTableEntry *stateEntry = stateNoToStateEntry->arr[from];
            LR1State *stateFrom = stateEntry->key;
            for (HashTableEntry *itemEntry = stateFrom->items->head; itemEntry;
                 itemEntry = itemEntry->nextInTable) {
              LR1SymbolString *string = itemEntry->key;
              CFGRule *rule = string->rule;
              HashTableEntry *ruleEntry = HashTableEntryRetrieve(
                  toRuleNo, rule);
              assert(ruleEntry);
              int ruleNo = (int)(long long)ruleEntry->value;
              int numRhs = rule->numRHS;
              int dot = string->dot;
              char *lhsString = lhsStrings->arr[ruleNo];
              char *rhsString = rhsStrings->arr[ruleNo];
              fprintf(stderr, "  %s ->", lhsString);
              for (int j = 0, offset = 0; j < numRhs; ++j) {
                if (j == dot)
                  fprintf(stderr, " •");
                fprintf(stderr, " %s", rhsString + offset);
                offset += strlen(rhsString + offset) + 1;
              }
              if (dot == numRhs)
                fprintf(stderr, " •");
              fprintf(stderr, "\n");
            }
            hasConflicts = true;
          }
          // Then, print the conflicting actions
          fprintf(stderr, "(%d) Conflict on lookahead token \"%s\":\n",
                  conflictNum, tokenStrings[lookahead]);
          int numOptions = i - start + 1;
          for (int option = 0; option < numOptions; ++option) {
            lookaheadAction = lookaheadActions->arr[start + option];
            int action = lookaheadAction->action;
            if (PARSER_IS_SHIFT_ACTION(action)) {
              // Note that there can only be shift/reduce or reduce/reduce
              // conflicts, so there will only be one shift action, and thus
              // we (probably) don't have to provide too much information about
              // the state that this goes to.
              fprintf(stderr, "  (%d) shift and go to state %d\n",
                      option, PARSER_SHIFT_ACTION_TO_STATE_NO(action));
            } else {
              int action = lookaheadAction->action;
              int ruleNo = PARSER_REDUCE_ACTION_TO_RULE_NO(action);
              fprintf(stderr, "  (%d) reduce using rule %d: %s ->",
                      option, ruleNo, (char*)lhsStrings->arr[ruleNo]);
              int numRhs = numRHSes[ruleNo];
              char *rhsString = rhsStrings->arr[ruleNo];
              for (int j = 0, offset = 0; j < numRhs; ++j) {
                fprintf(stderr, " %s", rhsString + offset);
                offset += strlen(rhsString + offset) + 1;
              }
              fprintf(stderr, "\n");
            }
          }

          chosenAction = ((ParserLookaheadAction*)
              lookaheadActions->arr[0])->action;
          if (conflictNum >= conflictResolution->size) {
            fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                    " Missing conflict resolution for conflict %d\n",
                    conflictNum);
            errorOccurred = true;
          } else {
            int option = (int)(long long)conflictResolution->arr[conflictNum];
            if (option < 0 || option >= numOptions) {
              fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                      " Invalid option %d for conflict %d (must be between "
                      "0 and %d, inclusive)", option, conflictNum,
                      numOptions - 1);
              errorOccurred = true;
            } else {
              chosenAction = ((ParserLookaheadAction*)
                  lookaheadActions->arr[option])->action;
            }
          }

          ++conflictNum;
        }

        // Add the chosen action to the state table
        int pos = curFreePos + lookahead;
        while (pos >= transitionStateTos->size) {
          VectorAdd(transitionStateFroms,
                    (void*)(long long)PARSER_TRANSITION_STATE_NONE);
          VectorAdd(transitionStateTos,
                    (void*)(long long)PARSER_TRANSITION_STATE_NONE);
        }
        transitionStateFroms->arr[pos] = (void*)(long long)from;
        transitionStateTos->arr[pos] = (void*)(long long)chosenAction;

        start = i + 1;
      }
    }
    transitionOffsets[from] = curFreePos;

    // Update "freePos" to the first position in the state table that is free
    // (this position can only increase)
    while (freePos < transitionStateTos->size &&
           (int)(long long)transitionStateTos->arr[freePos] !=
           PARSER_TRANSITION_STATE_NONE)
      ++freePos;
  }

  if (errorOccurred) {
    free(lhses);
    free(numRHSes);
    free(transitionOffsets);
    VectorDelete(handlers);
    int numHandlerNames = handlerNames->size;
    for (int i = 0; i < numHandlerNames; ++i)
      free(handlerNames->arr[i]);
    VectorDelete(handlerNames);
    goto CLEANUP;
  }

  parser = malloc(sizeof(Parser));
  {
    // This code is in the nested scope simply to avoid the error with gotos
    // and defining new variables
    int numTransitions = transitionStateFroms->size;
    parser->numTransitions = numTransitions;
    parser->transitionStateFroms = malloc(sizeof(int) * numTransitions);
    parser->transitionStateTos = malloc(sizeof(int) * numTransitions);
    for (int i = 0; i < numTransitions; ++i) {
      parser->transitionStateFroms[i] =
          (int)(long long)transitionStateFroms->arr[i];
      parser->transitionStateTos[i] =
          (int)(long long)transitionStateTos->arr[i];
    }
    parser->numStates = numStates;
    parser->transitionOffsets = transitionOffsets;
    parser->maxTokenID = cfg->eofTokenID;
    // "parser->maxTokenID + 1" represents the ID of the first variable in the
    // CFG, which is the start variable
    int offset = parser->transitionOffsets[0] + parser->maxTokenID + 1;
    parser->acceptingState = PARSER_SHIFT_ACTION_TO_STATE_NO(
        parser->transitionStateTos[offset]);
    parser->numRules = numRules;
    parser->lhses = lhses;
    parser->numRHSes = numRHSes;
    parser->lexer = lexer;
    parser->handlers = handlers;
    parser->handlerNames = handlerNames;
    parser->destructor = destructor;
  }

CLEANUP:
  LR1StateGraphDelete(graph);
  HashTableDelete(toStateNo);
  VectorDelete(stateNoToStateEntry);
  HashTableDelete(toRuleNo);

  VectorDelete(transitionStateFroms);
  VectorDelete(transitionStateTos);
  int numLookaheadActions = lookaheadActions->size;
  for (int i = 0; i < numLookaheadActions; ++i)
    free(lookaheadActions->arr[i]);
  VectorDelete(lookaheadActions);

  VectorDelete(lhsStrings);
  VectorDelete(rhsStrings);
  VectorDelete(conflictResolution);
  free(concatRhsString);
  free(tokenStrings);
  free(varStrings);
  free(auxiliaryRuleRhsString);

  return parser;
}

void ParserToFile(Parser *parser, FILE *file) {
  // TODO: transform to binary format
  fprintf(file, "%d\n", parser->numTransitions);
  for (int i = 0; i < parser->numTransitions; ++i)
    fprintf(file, "%d ", parser->transitionStateFroms[i]);
  fprintf(file, "\n");
  for (int i = 0; i < parser->numTransitions; ++i)
    fprintf(file, "%d ", parser->transitionStateTos[i]);
  fprintf(file, "\n%d\n", parser->numStates);
  for (int i = 0; i < parser->numStates; ++i)
    fprintf(file, "%d ", parser->transitionOffsets[i]);
  fprintf(file, "\n%d\n", parser->maxTokenID);
  fprintf(file, "%d\n", parser->acceptingState);
  fprintf(file, "%d\n", parser->numRules);
  int numRules = parser->numRules;
  for (int i = 0; i < numRules; ++i)
    fprintf(file, "%d ", parser->lhses[i]);
  fprintf(file, "\n");
  for (int i = 0; i < numRules; ++i)
    fprintf(file, "%d ", parser->numRHSes[i]);
  fprintf(file, "\n");
  Vector *handlerNames = parser->handlerNames;
  int numHandlerNames = handlerNames->size;
  fprintf(file, "%d\n", numHandlerNames);
  for (int i = 0; i < numHandlerNames; ++i) {
    char *handlerName = handlerNames->arr[i];
    fprintf(file, "%ld %s\n", strlen(handlerName), handlerName);
  }
}

Parser *ParserFromFileImpl(
    FILE *file, Lexer *lexer, HashTable *handlerTable,
    ParserObjectDestructor destructor) {
  Parser *parser = malloc(sizeof(Parser));
  fscanf(file, "%d", &parser->numTransitions);
  parser->transitionStateFroms = malloc(sizeof(int) * parser->numTransitions);
  for (int i = 0; i < parser->numTransitions; ++i)
    fscanf(file, "%d", &parser->transitionStateFroms[i]);
  parser->transitionStateTos = malloc(sizeof(int) * parser->numTransitions);
  for (int i = 0; i < parser->numTransitions; ++i)
    fscanf(file, "%d", &parser->transitionStateTos[i]);
  fscanf(file, "%d", &parser->numStates);
  parser->transitionOffsets = malloc(sizeof(int) * parser->numStates);
  for (int i = 0; i < parser->numStates; ++i)
    fscanf(file, "%d", &parser->transitionOffsets[i]);
  fscanf(file, "%d", &parser->maxTokenID);
  fscanf(file, "%d", &parser->acceptingState);
  fscanf(file, "%d", &parser->numRules);
  parser->lhses = malloc(sizeof(int) * parser->numRules);
  for (int i = 0; i < parser->numRules; ++i)
    fscanf(file, "%d", &parser->lhses[i]);
  parser->numRHSes = malloc(sizeof(int) * parser->numRules);
  for (int i = 0; i < parser->numRules; ++i)
    fscanf(file, "%d", &parser->numRHSes[i]);
  parser->lexer = lexer;
  parser->destructor = destructor;

  Vector *handlers = VectorNew();
  Vector *handlerNames = VectorNew();
  int numHandlerNames;
  fscanf(file, "%d", &numHandlerNames);
  for (int i = 0; i < numHandlerNames; ++i) {
    int handlerNameLength;
    fscanf(file, "%d", &handlerNameLength);
    char *handlerName = malloc(handlerNameLength + 1);
    fscanf(file, "%s", handlerName);
    HashTableEntry *entry = HashTableEntryRetrieve(handlerTable, handlerName);

    if (!entry) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET
                      " parser handler %s not found\n", handlerName);
      abort();
    }
    
    ParserHandler *handler = entry->value;
    VectorAdd(handlers, handler);
    VectorAdd(handlerNames, handlerName);
  }
  parser->handlers = handlers;
  parser->handlerNames = handlerNames;

  return parser;
}

void ParserDelete(Parser *parser) {
  free(parser->transitionStateFroms);
  free(parser->transitionStateTos);
  free(parser->transitionOffsets);
  free(parser->lhses);
  free(parser->numRHSes);
  VectorDelete(parser->handlers);

  Vector *handlerNames = parser->handlerNames;
  int numHandlers = handlerNames->size;
  for (int i = 0; i < numHandlers; ++i)
    free(handlerNames->arr[i]);
  VectorDelete(parser->handlerNames);

  free(parser);
}

#define PARSER_HASH_MOD 78885403583278429ULL
uint64_t ParserStringHash(void *key) {
  char *s = key, c;
  uint64_t hash = 0;
  for (int i = 0; (c = s[i]) != '\0'; ++i)
    hash = (((hash * 129) % PARSER_HASH_MOD) + (s[i] + 1)) % PARSER_HASH_MOD;
  return hash;
}

bool ParserStringEqual(void *key1, void *key2) {
  return strcmp(key1, key2) == 0;
}

void ParserSyntaxError(
    Lexer *lexer, LexerToken *token, Vector *stack,
    ParserObjectDestructor destructor, Vector *currentRHS) {
  // TODO: at least add tokens on stack to error message
  // TODO: improve syntax error and recovery
  Source *source = lexer->source;
  SourceLocation *loc = &token->loc;
  fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
          lexer->filename, loc->from.lineNo + 1);
  if (token == LEXER_TOKEN_EOF) {
    fprintf(stderr, "Unexpected end of file.\n");
  } else {
    fprintf(stderr, "Unexpected token '"SOURCE_COLOR_RED);
    char *tokenString = token->str;
    int tokenLength = token->length;
    for (int i = 0; i < tokenLength; ++i)
      fputc(tokenString[i], stderr);
    fprintf(stderr, SOURCE_COLOR_RESET"'.\n");
    SourceLocationPrint(source, 1, SOURCE_COLOR_RED, loc);
    LexerTokenDelete(token);
  }
  int numElements = stack->size;
  for (int i = 1; i < numElements; i += 2)
    ParserObjectDelete(stack->arr[i], destructor);
  VectorDelete(stack);
  VectorDelete(currentRHS);
}


ParserLookaheadAction *ParserLookaheadActionNew(int lookahead, int action) {
  ParserLookaheadAction *lookaheadAction = malloc(
      sizeof(ParserLookaheadAction));
  lookaheadAction->lookahead = lookahead;
  lookaheadAction->action = action;
  return lookaheadAction;
}

int ParserLookaheadActionCmp(const void *a, const void *b) {
  ParserLookaheadAction* const *pa = a;
  ParserLookaheadAction* const *pb = b;
  ParserLookaheadAction* const lookaheadAction1 = *pa;
  ParserLookaheadAction* const lookaheadAction2 = *pb;
  if (lookaheadAction1->lookahead < lookaheadAction2->lookahead)
    return -1;
  if (lookaheadAction1->lookahead > lookaheadAction2->lookahead)
    return 1;
  if (lookaheadAction1->action < lookaheadAction2->action)
    return -1;
  if (lookaheadAction1->action > lookaheadAction2->action)
    return 1;
  return 0;
}

void *ParserParse(Parser *parser) {
  int currentState = 0;
  Vector *stack = VectorNew();
  VectorAdd(stack, (void*)(long long)currentState);

  int numTransitions = parser->numTransitions;
  int *transitionStateFroms = parser->transitionStateFroms;
  int *transitionStateTos = parser->transitionStateTos;
  int *transitionOffsets = parser->transitionOffsets;
  int acceptingState = parser->acceptingState;
  int *lhses = parser->lhses;
  int *numRHSes = parser->numRHSes;
  Lexer *lexer = parser->lexer;
  Vector *handlers = parser->handlers;
  ParserHandler *handlersArr = (ParserHandler*)handlers->arr;
  ParserObjectDestructor destructor = parser->destructor;
  Vector *currentRHS = VectorNew();

  ParserRHS parserRHS;
  parserRHS.parser = parser;
  int eofTokenID = parser->maxTokenID;
  LexerToken *token = LexerNextToken(lexer);
  if (!token)
    return PARSER_OBJECT_FAILURE;
  int tokenID;
  if (token == LEXER_TOKEN_EOF)
    tokenID = eofTokenID;
  else
    tokenID = token->tokenID;
  int pos = transitionOffsets[currentState] + tokenID;
  while (true) {
    if (pos >= numTransitions ||
        transitionStateFroms[pos] != currentState) {
      ParserSyntaxError(lexer, token, stack, destructor, currentRHS);
      return PARSER_OBJECT_FAILURE;
    }
    int action = transitionStateTos[pos];
    if (PARSER_IS_SHIFT_ACTION(action)) {
      ParserObject *tokenObj = ParserObjectFromToken(token);
      currentState = PARSER_SHIFT_ACTION_TO_STATE_NO(action);
      VectorAdd(stack, tokenObj);
      VectorAdd(stack, (void*)(long long)currentState);

      if (currentState == acceptingState && tokenID == eofTokenID)
        break;

      token = LexerNextToken(lexer);
      if (!token)
        return PARSER_OBJECT_FAILURE;
      if (token == LEXER_TOKEN_EOF)
        tokenID = eofTokenID;
      else
        tokenID = token->tokenID;
    } else {
      int ruleNo = PARSER_REDUCE_ACTION_TO_RULE_NO(action);
      int lhs = lhses[ruleNo];
      int numRHS = numRHSes[ruleNo];
      currentRHS->size = numRHS;
      VectorReserve(currentRHS, numRHS);
      for (int i = numRHS - 1, j = stack->size - 2; i >= 0; --i, j -= 2) {
        ParserObject *parserObj = stack->arr[j];
        switch (parserObj->type) {
          case PARSER_OBJECT_TOKEN:
            currentRHS->arr[i] = parserObj->token;
            break;
          case PARSER_OBJECT_OBJECT:
            currentRHS->arr[i] = parserObj->object;
            break;
        }
        free(parserObj);
      }
      stack->size -= numRHS * 2;
      currentState = (int)(long long)stack->arr[stack->size - 1];

      ParserHandler handler = handlersArr[ruleNo];
      parserRHS.arr = currentRHS->arr;
      parserRHS.size = currentRHS->size;
      void *obj = handler(&parserRHS);
      ParserObject *parserObj = ParserObjectFromObject(obj);
      pos = transitionOffsets[currentState] + lhs;

      if (pos >= numTransitions ||
          transitionStateFroms[pos] != currentState) {
        ParserSyntaxError(lexer, token, stack, destructor, currentRHS);
        if (parserObj)
          ParserObjectDelete(parserObj, destructor);
        return PARSER_OBJECT_FAILURE;
      }

      currentState = PARSER_SHIFT_ACTION_TO_STATE_NO(transitionStateTos[pos]);
      VectorAdd(stack, parserObj);
      VectorAdd(stack, (void*)(long long)currentState);

      if (currentState == acceptingState && tokenID == eofTokenID)
        break;
    }
    pos = transitionOffsets[currentState] + tokenID;
  }

  ParserObject *parserObj = stack->arr[stack->size - 2];
  void *userObj = parserObj->object;
  free(parserObj);
  VectorDelete(stack);
  VectorDelete(currentRHS);
  return userObj;
}

