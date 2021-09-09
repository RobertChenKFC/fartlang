#include "parse/lr1/lr1.h"
#include <stdlib.h>
#include <assert.h>

// Helper functions
// Create a new LR(1) symbol string from a CFG "rule" and the "dot" position
LR1SymbolString *LR1SymbolStringNew(CFGRule *rule, int dot);
// Delete a LR(1) symbol string "a" created by LR1SymbolStringNew; can be used
// as a hash table destructor
void LR1SymbolStringDelete(void *a);
// Copies the content of "set" to a newly created set
Vector *LR1SetCopy(Vector *set);
// Wrapper function for VectorDelete to be suitable as a hash table destructor
void LR1SetDelete(void *a);
// Creates an array of booleans that represents whether each variable in "cfg"
// (sorted in ascending order by their variable IDs) is nullable or not
bool *LR1Nullable(CFG *cfg);
// Constructs the first set of a LR(1) symbol "string" from the CFG "cfg" and
// "nullable" of each variable in "cfg"; note that this function deletes
// "string"
Vector *LR1FirstSet(CFG *cfg, bool *nullable, LR1SymbolString *string);
// The implementation of LR1FirstSet for recursive calls, which takes extra
// arguments "set" and "table"; note that it deletes "string" if "string" is
// already in "table"
void LR1FirstSetImpl(
    Vector *set, HashTable *table, CFG *cfg, bool *nullable,
    LR1SymbolString *string);
// Constructs the follow set from a string "string", as well as a default "set",
// using the CFG "cfg" and "nullable" of each variable in "cfg"; note that this
// function deletes "string"
Vector *LR1FollowSet(
    CFG *cfg, bool *nullable, LR1SymbolString *string, Vector *set);
// If "complete" is true, constructs the closure of a LR(1) item from a LR(1)
// symbol "initString" and a set of "initLookahead", using the CFG "cfg" and
// "nullable" of each variable in "cfg"; otherwise, only the item
// ("initString", "initLookahead") is added to "state"; if "state" is NULL,
// a new LR(1) state is created and returned, otherwise all items are added to
// "state", and "state" is returned
LR1State *LR1ItemClosure(
    CFG *cfg, bool *nullable, LR1SymbolString *initString,
    Vector *initLookahead, LR1State *state, bool complete);
// Delete a LR(1) state "a" created from LR1Closure; can be used as a hash
// table destructor
void LR1StateDelete(void *a);
// Add an LR(1) symbol "string" with "lookahead" to "table"; if "string"
// does not exist in table, then the entry ("string", "lookahead") is added to
// the "table", otherwise "lookahead" is unioned with the existing lookahead in
// "table", and then "string" and "lookahead" is deleted; returns true if and
// only if "table" changed
bool LR1StateAddItem(
    HashTable *table, LR1SymbolString *string, Vector *lookahead);
// Add a LR(1) "transition" to "state"
void LR1StateAddTransition(LR1State *state, LR1Transition *transition);
// Creates a new LR(1) transition to "state" via "id"
LR1Transition *LR1TransitionNew(int id, LR1State *state);
// Deletes an LR(1) "transition" created from LR1TransitionNew
void LR1TransitionDelete(LR1Transition *transition);
// Creates a new LR(1) state graph with no states
LR1StateGraph *LR1StateGraphNew();
// Add an LR(1) "state" to an LR(1) state "graph"; note that it deletes "state"
// if "state" is already in "graph"; returns the LR(1) state that is in "graph";
// also sets the initial state of "graph" to "state" if "state" is the first
// state added
LR1State *LR1StateGraphAddState(LR1StateGraph *graph, LR1State *state);
// Compare function for IDs "a" and "b" stored in a vector; used for sorting
int LR1IDCmp(const void *a, const void *b);
// A hash function for a LR(1) symbol string "a"; used in hash tables
uint64_t LR1SymbolStringHash(void *a);
// An equal function for LR(1) symbol strings "a" and "b"; used in hash tables
bool LR1SymbolStringEqual(void *a, void *b);
// Compare LR(1) symbol strings "a" and "b" by the ID of the RHS after the dot;
// if the dot is at the end of its RHS, it is considered the largest; used for
// sorting
int LR1SymbolStringCmp(const void *a, const void *b);
// A hash function for a LR(1) item, which consists of a LR(1) symbol "string"
// and a set of "lookahead"
uint64_t LR1ItemHash(LR1SymbolString *string, Vector *lookahead);
// An equal function for a LR(1) items with LR(1) symbol strings "string1" and
// "string2", and lookahead sets "lookahead1" and "lookahead2"
bool LR1ItemEqual(
    LR1SymbolString *string1, Vector *lookahead1,
    LR1SymbolString *string2, Vector *lookahead2);
// A hash function for a LR1State "a"; used in hash tables
uint64_t LR1StateHash(void *a);
// An equal function for a LR1States "a" and "b"; used in hash tables
bool LR1StateEqual(void *a, void *b);

LR1SymbolString *LR1SymbolStringNew(CFGRule *rule, int dot) {
  LR1SymbolString *string = malloc(sizeof(LR1SymbolString));
  string->rule = rule;
  string->dot = dot;
  return string;
}

void LR1SymbolStringDelete(void *a) {
  LR1SymbolString *string = a;
  free(string);
}

Vector *LR1SetCopy(Vector *set) {
  int size = set->size;
  Vector *newSet = VectorNewWithCapacity(size);
  for (int i = 0; i < size; ++i)
    VectorAdd(newSet, set->arr[i]);
  return newSet;
}

void LR1SetDelete(void *a) {
  Vector *set = a;
  VectorDelete(set);
}

bool *LR1Nullable(CFG *cfg) {
  bool *nullable = calloc(cfg->numVariables, sizeof(bool));
  Vector *rules = cfg->rules;
  int numRules = rules->size;
  bool changed = false;
  for (int i = 0; i < numRules; ++i) {
    CFGRule *rule = rules->arr[i];
    if (rule->numRHS == 0) {
      nullable[CFG_VARIABLE_ID_TO_IDX(cfg, rule->lhs)] = true;

      // DEBUG
      printf("Variable %d is initially nullable\n",
             CFG_VARIABLE_ID_TO_IDX(cfg, rule->lhs));

      changed = true;
    }
  }
  while (changed) {
    changed = false;
    for (int i = 0; i < numRules; ++i) {
      CFGRule *rule = rules->arr[i];
      int lhsIdx = CFG_VARIABLE_ID_TO_IDX(cfg, rule->lhs);
      if (nullable[lhsIdx])
        continue;
      int *rhs = rule->rhs;
      int numRHS = rule->numRHS;
      bool curNullable = true;
      for (int j = 0; j < numRHS; ++j) {
        int id = rhs[j];
        if (!CFG_IS_TOKEN_ID(cfg, id)) {
          int idx = CFG_VARIABLE_ID_TO_IDX(cfg, id);
          if (!nullable[idx]) {
            curNullable = false;
            break;
          }
        } else {
          curNullable = false;
          break;
        }
      }
      if (curNullable) {
        nullable[lhsIdx] = true;
        changed = true;
      }
    }
  }
  return nullable;
}

Vector *LR1FirstSet(CFG *cfg, bool *nullable, LR1SymbolString *string) {
  Vector *set = VectorNewWithCapacity(1);
  HashTable *table = HashTableNew(
      LR1SymbolStringHash, LR1SymbolStringEqual, LR1SymbolStringDelete, NULL);
  LR1FirstSetImpl(set, table, cfg, nullable, string);
  HashTableDelete(table);
  return set;
}

void LR1FirstSetImpl(
    Vector *set, HashTable *table, CFG *cfg, bool *nullable,
    LR1SymbolString *string) {
  HashTableEntry *entry = HashTableEntryRetrieve(table, string);
  if (entry) {
    LR1SymbolStringDelete(string);
    return;
  }
  HashTableEntryAdd(table, string, NULL);

  int *w = string->rule->rhs;
  int i = string->dot;
  int n = string->rule->numRHS;
  if (i < n) {
    if (CFG_IS_TOKEN_ID(cfg, w[i])) {
      VectorAdd(set, (void*)(long long)w[i]);
    } else {
      int variableID = w[i];
      int variableIdx = CFG_VARIABLE_ID_TO_IDX(cfg, variableID);
      Vector *rules = cfg->rules;
      int numRules = rules->size;
      for (int j = cfg->variableToRule[variableIdx]; j < numRules; ++j) {
        CFGRule *rule = rules->arr[j];
        if (rule->lhs != variableID)
          break;
        LR1SymbolString *newString = LR1SymbolStringNew(rule, 0);
        LR1FirstSetImpl(set, table, cfg, nullable, newString);
      }
      if (nullable[variableIdx]) {
        LR1SymbolString *newString = LR1SymbolStringNew(
            string->rule, string->dot + 1);
        LR1FirstSetImpl(set, table, cfg, nullable, newString);
      }
    }
  }
}

Vector *LR1FollowSet(
    CFG *cfg, bool *nullable, LR1SymbolString *string, Vector *set) {
  int *w = string->rule->rhs;
  int i = string->dot;
  int n = string->rule->numRHS;
  if (i == n) {
    LR1SymbolStringDelete(string);
    return LR1SetCopy(set);
  }

  Vector *newSet = LR1FirstSet(cfg, nullable, string);
  if (!CFG_IS_TOKEN_ID(cfg, w[i])) {
    int idx = CFG_VARIABLE_ID_TO_IDX(cfg, w[i]);
    if (nullable[idx]) {
      int numLookahead = set->size;
      for (int i = 0; i < numLookahead; ++i)
        VectorAdd(newSet, set->arr[i]);
    }
  }

  qsort(newSet->arr, newSet->size, sizeof(newSet->arr[0]), LR1IDCmp);

  // Remove potential repeated lookahead tokens
  int j;
  int numLookahead = newSet->size;
  for (i = 0, j = 0; i < numLookahead; ++i) {
    if (i == 0 || newSet->arr[i] != newSet->arr[i - 1])
      newSet->arr[j++] = newSet->arr[i];
  }
  newSet->size = j;

  // DEBUG
  // printf("before follow\n");
  for (int i = 1; i < newSet->size; ++i)
    assert(newSet->arr[i - 1] < newSet->arr[i]);
  // printf("done follow\n");

  return newSet;
}

LR1State *LR1ItemClosure(
    CFG *cfg, bool *nullable, LR1SymbolString *initString,
    Vector *initLookahead, LR1State *state, bool complete) {
  if (!state) {
    state = malloc(sizeof(LR1State));
    state->items = HashTableNew(
        LR1SymbolStringHash, LR1SymbolStringEqual,
        LR1SymbolStringDelete, LR1SetDelete);
    state->transition = NULL;
  }
  LR1StateAddItem(state->items, initString, initLookahead);
  if (!complete)
    return state;

  Vector *rules = cfg->rules;
  int numRules = rules->size;
  int *variableToRule = cfg->variableToRule;
  HashTable *items = state->items;
  bool changed = true;
  while (changed) {
    changed = false;
    for (HashTableEntry *entry = items->head; entry;
         entry = entry->nextInTable) {
      // DEBUG
      if (items->size > 500) {
        printf("Too many items\n");
        break;
      }

      LR1SymbolString *string = entry->key;
      Vector *lookahead = entry->value;
      CFGRule *rule = string->rule;
      int *rhs = rule->rhs;
      int numRHS = rule->numRHS;
      int dot = string->dot;
      if (dot < numRHS && !CFG_IS_TOKEN_ID(cfg, rhs[dot])) {
        LR1SymbolString *curString = LR1SymbolStringNew(rule, dot + 1);
        Vector *followSet = LR1FollowSet(cfg, nullable, curString, lookahead);
        int variableID = rhs[dot];
        int variableIdx = CFG_VARIABLE_ID_TO_IDX(cfg, variableID);
        for (int i = variableToRule[variableIdx]; i < numRules; ++i) {
          CFGRule *curRule = rules->arr[i];
          if (curRule->lhs != variableID)
            break;
          LR1SymbolString *newString = LR1SymbolStringNew(curRule, 0);
          changed = LR1StateAddItem(items, newString, LR1SetCopy(followSet)) ||
                    changed;
        }
        // "followSet" must be deleted since all added items use a copy of it
        VectorDelete(followSet);
      }
    }
  }
  return state;
}

void LR1StateDelete(void *a) {
  LR1State *state = a;
  HashTableDelete(state->items);
  for (LR1Transition *transition = state->transition, *nextTransition;
       transition; transition = nextTransition) {
    nextTransition = transition->next;
    LR1TransitionDelete(transition);
  }
  free(state);
}

bool LR1StateAddItem(
    HashTable *table, LR1SymbolString *string, Vector *lookahead) {
  bool changed = false;
  HashTableEntry *entry = HashTableEntryRetrieve(table, string);
  if (entry) {
    Vector *curLookahead = entry->value;
    int curNumLookahead = curLookahead->size;
    int numLookahead = lookahead->size;
    for (int i = 0; i < numLookahead; ++i) {
      void *id = lookahead->arr[i];

      // DEBUG
      // printf("before\n");
      /*
      for (int k = 1; k < curNumLookahead; ++k)
        assert(curLookahead->arr[k - 1] < curLookahead->arr[k]);
      */

      int j;
      for (j = 0; j < curNumLookahead; ++j) {
        if (curLookahead->arr[j] >= id)
          break;
      }
      if (j >= curNumLookahead || id != curLookahead->arr[j]) {
        VectorAdd(curLookahead, id);
        // TODO: improve this
        for (int k = curNumLookahead - 1; k >= j; --k)
          curLookahead->arr[k + 1] = curLookahead->arr[k];
        curLookahead->arr[j] = id;
        ++curNumLookahead;
        changed = true;
      }

      // DEBUG
      // printf("after\n");
      /*
      for (int k = 1; k < curNumLookahead; ++k)
        assert(curLookahead->arr[k - 1] < curLookahead->arr[k]);
      */
      // printf("done\n");
    }
    LR1SymbolStringDelete(string);
    VectorDelete(lookahead);
  } else {
    HashTableEntryAdd(table, string, lookahead);
    changed = true;
  }
  return changed;
}

void LR1StateAddTransition(LR1State *state, LR1Transition *transition) {
  transition->next = state->transition;
  state->transition = transition;
}

LR1Transition *LR1TransitionNew(int id, LR1State *state) {
  LR1Transition *transition = malloc(sizeof(LR1Transition));
  transition->id = id;
  transition->state = state;
  transition->next = NULL;
  return transition;
}

void LR1TransitionDelete(LR1Transition *transition) {
  free(transition);
}

LR1StateGraph *LR1StateGraphNew() {
  LR1StateGraph *graph = malloc(sizeof(LR1StateGraph));
  graph->states = HashTableNew(
      LR1StateHash, LR1StateEqual, LR1StateDelete, NULL);
  graph->init = NULL;
  return graph;
}

LR1State *LR1StateGraphAddState(LR1StateGraph *graph, LR1State *state) {
  HashTable *states = graph->states;
  HashTableEntry *entry = HashTableEntryRetrieve(states, state);
  if (entry) {
    LR1StateDelete(state);
    return entry->key;
  }
  HashTableEntryAdd(states, state, NULL);
  if (!graph->init)
    graph->init = state;
  return state;
}

int LR1IDCmp(const void *a, const void *b) {
  void *id1 = *((void **)a);
  void *id2 = *((void **)b);
  if (id1 < id2)
    return -1;
  if (id1 > id2)
    return 1;
  return 0;
}

uint64_t LR1SymbolStringHash(void *a) {
  LR1SymbolString *string = a;
  return (uint64_t)string->rule ^
         ((uint64_t)string->dot << 32);
}

bool LR1SymbolStringEqual(void *a, void *b) {
  LR1SymbolString *string1 = a;
  LR1SymbolString *string2 = b;
  return string1->rule == string2->rule &&
         string1->dot == string2->dot;
}

int LR1SymbolStringCmp(const void *a, const void *b) {
  LR1SymbolString *string1 = *((LR1SymbolString **)a);
  LR1SymbolString *string2 = *((LR1SymbolString **)b);
  CFGRule *rule1 = string1->rule;
  CFGRule *rule2 = string2->rule;
  if (string1->dot == rule1->numRHS)
    return 1;
  if (string2->dot == rule2->numRHS)
    return -1;
  int id1 = rule1->rhs[string1->dot];
  int id2 = rule2->rhs[string2->dot];
  return id1 - id2;
}

uint64_t LR1ItemHash(LR1SymbolString *string, Vector *lookahead) {
  uint64_t hash = (uint64_t)string->rule ^ (uint64_t)string->dot;
  hash ^= (uint64_t)lookahead->size << 32;
  int numLookahead = lookahead->size;
  for (int i = 0; i < numLookahead; ++i) {
    hash = (hash << 7) | (hash >> 57);
    hash ^= (uint64_t)lookahead->arr[i];
  }
  return hash;
}

bool LR1ItemEqual(
    LR1SymbolString *string1, Vector *lookahead1,
    LR1SymbolString *string2, Vector *lookahead2) {
  if (string1->rule    != string2->rule ||
      string1->dot     != string2->dot  ||
      lookahead1->size != lookahead2->size)
    return false;
  int numLookahead = lookahead1->size;
  for (int i = 0; i < numLookahead; ++i) {
    if (lookahead1->arr[i] != lookahead2->arr[i])
      return false;
  }
  return true;
}

uint64_t LR1StateHash(void *a) {
  LR1State *state = a;
  uint64_t hash = 0;
  for (HashTableEntry *entry = state->items->head; entry;
       entry = entry->nextInTable) {
    LR1SymbolString *string = entry->key;
    Vector *lookahead = entry->value;
    hash ^= LR1ItemHash(string, lookahead);
  }
  return hash;
}

bool LR1StateEqual(void *a, void *b) {
  LR1State *state1 = a;
  LR1State *state2 = b;
  HashTable *items1 = state1->items;
  HashTable *items2 = state2->items;
  if (items1->size != items2->size)
    return false;
  for (HashTableEntry *entry1 = items1->head; entry1;
       entry1 = entry1->nextInTable) {
    HashTableEntry *entry2 = HashTableEntryRetrieve(items2, entry1->key);
    if (!entry2)
      return false;
    LR1SymbolString *string1 = entry1->key;
    Vector *lookahead1 = entry1->value;
    LR1SymbolString *string2 = entry2->key;
    Vector *lookahead2 = entry2->value;
    // TODO: maybe this function just needs to compare the lookaheads because
    //       the strings are already compared
    if (!LR1ItemEqual(string1, lookahead1, string2, lookahead2))
      return false;
  }
  return true;
}

LR1StateGraph *LR1StateGraphFromCFG(CFG *cfg) {
  int startVariable = CFGAddVariable(cfg);
  int eofTokenID = cfg->eofTokenID;
  CFGRule *initRule = CFGAddRule(
      cfg, startVariable, 1, cfg->startVariable);
  cfg->startVariable = startVariable;
  CFGFinalize(cfg);

  bool *nullable = LR1Nullable(cfg);

  // DEBUG
  printf("\n\nNullable varaibles:");
  for (int i = 0; i < cfg->numVariables; ++i) {
    if (nullable[i])
      printf(" %d", i);
  }
  printf("\n");

  LR1SymbolString *initString = LR1SymbolStringNew(initRule, 0);
  Vector *initLookahead = VectorNewWithCapacity(1);
  VectorAdd(initLookahead, (void*)(long long)eofTokenID);
  LR1State *initState = LR1ItemClosure(
      cfg, nullable, initString, initLookahead, NULL, true);
  LR1StateGraph *graph = LR1StateGraphNew();
  initState = LR1StateGraphAddState(graph, initState);

  HashTable *states = graph->states;
  Vector *sortedStrings = VectorNew();
  for (HashTableEntry *stateEntry = states->head; stateEntry;
       stateEntry = stateEntry->nextInTable) {
    // DEBUG
    printf("%d states\n", states->size);
    if (states->size > 3000) {
      printf("Too many states\n");
      break;
    }

    LR1State *state = stateEntry->key;
    HashTable *items = state->items;
    sortedStrings->size = 0;
    for (HashTableEntry *itemEntry = items->head; itemEntry;
         itemEntry = itemEntry->nextInTable) {
      LR1SymbolString *string = itemEntry->key;
      VectorAdd(sortedStrings, string);
    }
    int numStrings = sortedStrings->size;
    qsort(sortedStrings->arr, numStrings, sizeof(sortedStrings->arr[0]),
          LR1SymbolStringCmp);

    LR1State *newState = NULL;
    for (int i = 0; i < numStrings; ++i) {
      LR1SymbolString *string = sortedStrings->arr[i];
      HashTableEntry *itemEntry = HashTableEntryRetrieve(items, string);
      assert(itemEntry);
      Vector *lookahead = itemEntry->value;

      CFGRule *rule = string->rule;
      int dot = string->dot;
      if (dot == rule->numRHS)
        break;
      LR1SymbolString *newString = LR1SymbolStringNew(rule, dot + 1);
      Vector *newLookahead = LR1SetCopy(lookahead);
      LR1SymbolString *nextString =
          (i == numStrings - 1) ? NULL : sortedStrings->arr[i + 1];

      // The closure is complete and can be added to the set of states if:
      // (1) There are no more items to add to the closure
      // (2) The next item's RHS has nothing on the right of its dot
      // (3) The next item's ID of the RHS after its dot is different to that of
      //     the current item's
      bool complete = !nextString ||
                      nextString->dot == nextString->rule->numRHS ||
                      nextString->rule->rhs[nextString->dot] != rule->rhs[dot];
      newState = LR1ItemClosure(
          cfg, nullable, newString, newLookahead, newState, complete);
      if (complete) {
        newState = LR1StateGraphAddState(graph, newState);
        LR1Transition *transition = LR1TransitionNew(rule->rhs[dot], newState);
        LR1StateAddTransition(state, transition);
        newState = NULL;
      }
    }
  }
  VectorDelete(sortedStrings);

  free(nullable);

  return graph;
}

void LR1StateGraphDelete(LR1StateGraph *graph) {
  HashTableDelete(graph->states);
  free(graph);
}

void LR1StateGraphPrint(
    CFG *cfg, LR1StateGraph *graph, Vector *tokens, Vector *variables,
    FILE *file) {
  int idx = 1;
  HashTable *states = graph->states;
  for (HashTableEntry *entry = states->head; entry; entry = entry->nextInTable)
    entry->value = (void*)(long long)idx++;

  fprintf(file, "digraph LR1 {\n");
  fprintf(file, "  label=\"%d states\";\n", idx - 1);
  fprintf(file, "  rankdir=LR;\n");
  fprintf(file, "  node[shape=box];\n");

  // Need to add extra token and variable strings, since a new EOF token and
  // start variable were added to the CFG when constructing the LR(1) graph
  VectorAdd(tokens, "$");
  VectorAdd(variables, "S'");

  for (HashTableEntry *entry = states->head; entry;
       entry = entry->nextInTable) {
    LR1State *state = entry->key;
    idx = (int)(long long)entry->value;
    fprintf(file, "  %d [label=\"", idx);

    HashTable *items = state->items;
    for (HashTableEntry *entryItem = items->head; entryItem;
         entryItem = entryItem->nextInTable) {
      // Print rule with dot at the RHS
      LR1SymbolString *string = entryItem->key;
      CFGRule *rule = string->rule;
      int dot = string->dot;
      int id = rule->lhs;
      fprintf(file, "%s -> ",
              (char*)variables->arr[CFG_VARIABLE_ID_TO_IDX(cfg, id)]);
      int *rhs = rule->rhs;
      int numRHS = rule->numRHS;
      for (int i = 0; i < numRHS; ++i) {
        if (i == dot)
          fprintf(file, "• ");
        id = rhs[i];
        char *idStr;
        if (CFG_IS_TOKEN_ID(cfg, id))
          idStr = tokens->arr[id];
        else
          idStr = variables->arr[CFG_VARIABLE_ID_TO_IDX(cfg, id)];
        fprintf(file, "%s", idStr);
        if (i != numRHS - 1)
          fprintf(file, " ");
      }
      if (dot == numRHS)
        fprintf(file, " •");

      // Print lookahead tokens
      fprintf(file, ", ");
      Vector *lookahead = entryItem->value;
      int numLookahead = lookahead->size;
      for (int i = 0; i < numLookahead; ++i) {
        int tokenID = (int)(long long)lookahead->arr[i];
        if (i != 0)
          fprintf(file, "/");

        // DEBUG
        if (tokenID < 0 || tokenID >= tokens->size)
          printf("Strange token %d\n", tokenID);

        fprintf(file, "%s", (char*)tokens->arr[tokenID]);
      }
      fprintf(file, "\\l");
    }

    fprintf(file, "\"];\n");
  }

  // Add arrow to point to initial state
  fprintf(file, "  0 [label= \"\", shape=none,height=.0,width=.0];\n");
  fprintf(file, "  0 -> 1;\n");

  idx = 1;
  for (HashTableEntry *entry = states->head; entry;
       entry = entry->nextInTable) {
    LR1State *stateFrom = entry->key;
    int from = (int)(long long)entry->value;
    for (LR1Transition *transition = stateFrom->transition; transition;
         transition = transition->next) {
      LR1State *stateTo = transition->state;
      HashTableEntry *entryTo = HashTableEntryRetrieve(states, stateTo);
      assert(entryTo);
      int to = (int)(long long)entryTo->value;
      const char *id;
      if (CFG_IS_TOKEN_ID(cfg, transition->id))
        id = tokens->arr[transition->id];
      else
        id = variables->arr[CFG_VARIABLE_ID_TO_IDX(cfg, transition->id)];
      fprintf(file, "  %d -> %d [label=\"%s\"];\n", from, to, id);
    }
  }

  // Remove the extra added EOF token and start variable
  --tokens->size;
  --variables->size;

  fprintf(file, "}\n");
}
