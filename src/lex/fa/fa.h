#ifndef FA_H
#define FA_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

// Forward declarations
typedef struct FA FA;
typedef struct FAState FAState;
typedef struct FATransition FATransition;

// An finite automaton structure which cantains the FA's initial state as well
// as the last state added to the FA; the lexer can optionally store the name
// of the regex that is used to generate this automaton in "name", so that its
// name (instead of ID) can be printed when a conflict occurs
struct FA {
  FAState *init, *last;
  char *name;
};

// A state in an FA, which contains all transitions to other states, what kind
// of accepting state it is (if it is one), and a pointer to the next state to
// connect all states in the FA together as a list
#define FA_ACCEPT_NONE -1
struct FAState {
  int accepting;
  FATransition *transition;
  FAState *next;
};

#define FA_EPSILON 129
// A transition in the FA, which links to another FA state when a particular
// character is encountered, and another pointer to connect all transitions
// from the same state together as a list
struct FATransition {
  unsigned char a;
  FAState *state;
  FATransition *next;
};

// Construct an empty FA
FA *FANew(void);
// Deletes an "fa" created with FANew, as well as the transitions it contains
void FADelete(FA *fa);
// Creates a new FA state with no transitions and property "accepting"
FAState *FAStateNew(int accepting);
// Adds "state" to "fa"; note that if "fa" currently has no states, then
// "state" will become its initial state
void FAAddState(FA *fa, FAState *state);
// Adds all states of "fa2" to "fa1"; note that if "fa1" currently has no
// states, then the initial state of "fa2" will become the initial state of
// "fa1"
void FAAddStates(FA *fa1, FA *fa2);
// Adds a transition from "state1" via character "a" to "state2"
void FAStateAddTransition(FAState *state1, unsigned char a, FAState *state2);
// Find the destination state when taking the transition from "state1" via
// character "a"
FAState *FAStateGetTransition(FAState *state1, unsigned char a);
// Print the content of "fa" as a graph written in the DOT language into "file"
void FAPrint(FA *fa, FILE *file);
// A hash function for state pointer "a"; used in hash tables
uint64_t FAStatePtrHash(void *a);
// A equal function for state pointers "a" and "b"; used in hash tables
bool FAStatePtrEqual(void *a, void *b);
// A compare function for two FA transitions "a" and "b", which first compares
// by the state that the transitions go to, then compares by character; used for
// sorting
int FATransitionCmp(const void *a, const void *b);

#endif // FA_H
