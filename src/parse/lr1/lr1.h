#ifndef LR1_H
#define LR1_H

#include "parse/cfg/cfg.h"
#include "util/hashtable/hashtable.h"
#include "util/vector/vector.h"
#include <stdio.h>

// Forward declarations
typedef struct LR1SymbolString LR1SymbolString;
typedef struct LR1State LR1State;
typedef struct LR1Transition LR1Transition;
typedef struct LR1StateGraph LR1StateGraph;
typedef struct LR1Table LR1Table;

// An LR(1) symbol string, which is a string consisting of token and variable
// IDs, and an index to store where the dot is
struct LR1SymbolString {
  CFGRule *rule;
  int dot;
};

// An LR(1) state, which consists of a set of LR(1) items and a list of
// transitions. An LR(1) item is an entry in the hash table "items", where the
// key is an LR1SymbolString, and the value is its lookahead tokens stored in a
// Vector
struct LR1State {
  HashTable *items;
  LR1Transition *transition;
};

// An LR(1) transition, which consists of an ID that the transitions goes on
// and the LR(1) state it goes to; it also contains a next point to link
// multiple LR(1) transitions together
struct LR1Transition {
  int id;
  LR1State *state;
  LR1Transition *next;
};

// An LR(1) state transition graph, which contains a set of states, as well as
// an initial state, which points to the first state in the set of states
struct LR1StateGraph {
  HashTable *states;
  LR1State *init;
};

// Constructs a LR(1) state graph from "cfg"; note that because this function
// calls CFGFinalize, CFGFinalize should not be called on "cfg" before or after
// this function is called; note that this function adds a new varaiable
// $accept, a new rule $accept -> (start variable of "cfg") $end, and makes
// $accept the new start variable
LR1StateGraph *LR1StateGraphFromCFG(CFG *cfg);
// Deletes an LR(1) state "graph" created with LR1StateGraphFromCFG
void LR1StateGraphDelete(LR1StateGraph *graph);
// Prints a LR(1) state "graph" of "cfg" to "file" in the dot language,
// replacing each token (with ascending order in token ID) with a string in
// "tokens", and each variable (with ascending order in variable ID) with a
// string in "variables"; note that the start variable is printed as $accept,
// and the end of file token is printed as $end
void LR1StateGraphPrint(
    CFG *cfg, LR1StateGraph *graph, Vector *tokens, Vector *variables,
    FILE *file);
// Same as LR1StateGraphPrint but in XML format
void LR1StateGraphPrintXML(
    CFG *cfg, LR1StateGraph *graph, Vector *tokens, Vector *variables,
    FILE *file);

#endif // LR1_H
