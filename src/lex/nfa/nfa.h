#ifndef NFA_H
#define NFA_H

#include "lex/regex/regex.h"
#include <stdbool.h>
#include <stdio.h>

// Forward declarations
typedef struct NFA NFA;
typedef struct NFAState NFAState;
typedef struct NFATransition NFATransition;

// An NFA structure which cantains the NFA's initial state as well as the last
// state added to the NFA
struct NFA {
  NFAState *init, *last;
};

// A state in an NFA, which contains all transitions to other states, whether
// it is an accepting state, and a pointer to the next state to connect all
// states in the NFA together as a list
struct NFAState {
  bool accepting;
  NFATransition *transition;
  NFAState *next;
};

#define NFA_EPSILON 129
// A transition in the NFA, which links to another NFA state when a particular
// character is encountered, and another pointer to connect all transitions
// from the same state together as a list
struct NFATransition {
  unsigned char a;
  NFAState *state;
  NFATransition *next;
};

// Initializes all constant NFAs
void NFAInit();
// Construct an NFA from "regex"
NFA *NFAFromRegex(Regex *regex);
// Deletes an "nfa" created with NFAFromRegex
void NFADelete(NFA *nfa);
// Print the content of "nfa" as a graph written in the DOT language into "file"
void NFAPrint(NFA *nfa, FILE *file);

#endif // NFA_H
