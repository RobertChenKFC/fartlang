#include "lex/nfa/nfa.h"
#include "util/hashtable/hashtable.h"
#include "util/vector/vector.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// Constants
// Check if a character is an ASCII control character
#define IS_CONTROL_CHARACTER(a) (((a) >= 0 && (a) <= 31) || (a) == 127)

// Helper functions
// Creates a new NFA with no states
NFA *NFANew();
// Creates a new NFA state with no transitions with property "accepting"
NFAState *NFAStateNew(bool accepting);
// Adds "state" to "nfa"; note that if "nfa" currently has no states, then
// "state" will become its initial state
void NFAAddState(NFA *nfa, NFAState *state);
// Adds all states of "nfa2" to "nfa1"; note that if "nfa1" currently has no
// states, then the initial state of "nfa2" will become the initial state of
// "nfa1"
void NFAAddStates(NFA *nfa1, NFA *nfa2);
// Adds a transition from "state1" via character "a" to "state2"
void NFAStateAddTransition(NFAState *state1, unsigned char a, NFAState *state2);
// Check if all subregexes of "list" are letter regexes
bool NFAAllLetterRegexes(RegexList *list);
// Create NFA from "regexUnion", which is the union of all letter regexes
NFA *NFAFromUnionLetterRegexes(RegexUnion *regexUnion);
// Creates NFA from "regexConcat", which is the concat of all letter regexes
NFA *NFAFromConcatLetterRegexes(RegexConcat *regexConcat);
// A hash function for state pointer "a"; used in hash tables
uint64_t NFAStatePtrHash(void *a);
// A equal function for state pointers "a" and "b"; used in hash tables
bool NFAStatePtrEqual(void *a, void *b);
// A compare function for two NFA transitions "a" and "b"; used for sorting
int NFATransitionCmp(const void *a, const void *b);
// Prints a transition from state with index "fromIdx", via characters from
// "charFrom" to "charTo", to state "to", where state "to" is translated to
// its index using "table", to "file"
void NFATransitionPrint(
    int fromIdx,
    unsigned char charFrom, unsigned char charTo,
    NFAState *to,
    HashTable *table,
    FILE *file);
// Prints a character "a" to "file", with special care for control characters
void NFAPrintChar(unsigned char a, FILE *file);

void NFAInit() {
  // Initialze regex
  RegexInit();
}

NFA *NFANew() {
  NFA *nfa = calloc(1, sizeof(NFA));
  return nfa;
}

NFAState *NFAStateNew(bool accepting) {
  NFAState *state = calloc(1, sizeof(NFAState));
  state->accepting = accepting;
  return state;
}

void NFAAddState(NFA *nfa, NFAState *state) {
  if (nfa->init == NULL) {
    nfa->init = nfa->last = state;
  } else {
    nfa->last->next = state;
    nfa->last = state;
  }
}

void NFAAddStates(NFA *nfa1, NFA *nfa2) {
  if (nfa1->init == NULL) {
    nfa1->init = nfa2->init;
    nfa1->last = nfa2->last;
  } else {
    nfa1->last->next = nfa2->init;
    nfa1->last = nfa2->last;
  }
}

void NFAStateAddTransition(
    NFAState *state1, unsigned char a, NFAState *state2) {
  NFATransition *transition = malloc(sizeof(NFATransition));
  transition->a = a;
  transition->state = state2;
  transition->next = state1->transition;
  state1->transition = transition;
}

bool NFAAllLetterRegexes(RegexList *list) {
  for (RegexListNode *node = list->head; node; node = node->next) {
    Regex *regex = node->cur;
    if (regex->type != REGEX_LETTER)
      return false;
  }
  return true;
}

NFA *NFAFromUnionLetterRegexes(RegexUnion *regexUnion) {
  NFA *nfa = NFANew();
  NFAState *init = NFAStateNew(false);
  NFAState *accept = NFAStateNew(true);
  NFAAddState(nfa, init);
  NFAAddState(nfa, accept);
  for (RegexListNode *node = regexUnion->list.head; node;
       node = node->next) {
    Regex *subRegex = node->cur;
    unsigned char a = subRegex->regexLetter.a;
    NFAStateAddTransition(init, a, accept);
  }
  return nfa;
}

NFA *NFAFromConcatLetterRegexes(RegexConcat *regexConcat) {
  NFA *nfa = NFANew();
  NFAState *prev = NFAStateNew(false);
  NFAAddState(nfa, prev);
  for (RegexListNode *node = regexConcat->list.head; node;
       node = node->next) {
    Regex *subRegex = node->cur;
    unsigned char a = subRegex->regexLetter.a;
    NFAState *state = NFAStateNew(false);
    NFAAddState(nfa, state);
    NFAStateAddTransition(prev, a, state);
    prev = state;
  }
  prev->accepting = true;
  return nfa;
}

NFA *NFAFromRegex(Regex *regex) {
  switch (regex->type) {
    case REGEX_NULL: {
      NFA *nfa = NFANew();
      NFAState *init = NFAStateNew(false);
      NFAAddState(nfa, init);
      return nfa;
    } case REGEX_LETTER: {
      unsigned char a = regex->regexLetter.a;
      NFA *nfa = NFANew();
      NFAState *init = NFAStateNew(false);
      NFAState *accept = NFAStateNew(true);
      // An NFA accepting a single letter "a" can be constructed by having
      // two states: the initiali state "init" and the accepting state "accept",
      // connecting them with a transition "a"
      NFAStateAddTransition(init, a, accept);
      NFAAddState(nfa, init);
      NFAAddState(nfa, accept);
      return nfa;
    } case REGEX_UNION: {
      // Specifically optimize union of all letter regexes to avoid adding too
      // many states
      if (NFAAllLetterRegexes(&regex->regexUnion.list))
        return NFAFromUnionLetterRegexes(&regex->regexUnion);
      NFA *nfa = NFANew();
      NFAState *init = NFAStateNew(false);
      NFAAddState(nfa, init);
      // An NFA accepting a union of sub-regexes can be constructed by having
      // an initial state "init", and connecting "init" to all the initial
      // states of the sub-NFAs, which are constructed from the sub-regexes, via
      // epsilon transition
      for (RegexListNode *node = regex->regexUnion.list.head; node;
           node = node->next) {
        Regex *subRegex = node->cur;
        NFA *subNFA = NFAFromRegex(subRegex);
        NFAAddStates(nfa, subNFA);
        NFAStateAddTransition(init, NFA_EPSILON, subNFA->init);
        free(subNFA);
      }
      return nfa;
    } case REGEX_CONCAT: {
      // Specifically optimize concatenation of all letter regexes to avoid
      // adding too many states
      if (NFAAllLetterRegexes(&regex->regexUnion.list))
        return NFAFromConcatLetterRegexes(&regex->regexConcat);
      NFA *nfa = NFANew();
      NFAState *prev = NULL;
      // An NFA accepting a concatenation of sub-regexes can be constructed by
      // connecting all accepting states of the i-th sub-NFA, which is
      // constructed from the i-th sub-regex, to the initial state of the
      // (i+1)-th sub-NFA, then transforming all accepting states of the i-th
      // sub-NFA into non-accepting states
      for (RegexListNode *node = regex->regexUnion.list.head; node;
           node = node->next) {
        Regex *subRegex = node->cur;
        NFA *subNFA = NFAFromRegex(subRegex);
        if (prev) {
          for (NFAState *state = prev; state; state = state->next) {
            if (state->accepting) {
              NFAStateAddTransition(state, NFA_EPSILON, subNFA->init);
              state->accepting = false;
            }
          }
        }
        NFAAddStates(nfa, subNFA);
        prev = subNFA->init;
        free(subNFA);
      }
      return nfa;
    } case REGEX_STAR: {
      // An NFA accepting zero or more occrurrences of a regex can be
      // constructed by first constructing the NFA for the regex, then modifying
      // the NFA to connect all accepting states to the initial state via
      // epsilon transitions, and change the initial state to accepting
      NFA *nfa = NFAFromRegex(regex->regexStar.regex);
      NFAState *init = nfa->init;
      for (NFAState *state = init; state; state = state->next) {
        if (state->accepting)
          NFAStateAddTransition(state, NFA_EPSILON, init);
      }
      init->accepting = true;
      return nfa;
    }
  }
}

void NFADelete(NFA *nfa) {
  NFAState *nextState;
  for (NFAState *state = nfa->init; state; state = nextState) {
    NFATransition *nextTransition;
    for (NFATransition *transition = state->transition; transition;
         transition = nextTransition) {
      nextTransition = transition->next;
      free(transition);
    }
    nextState = state->next;
    free(state);
  }
  free(nfa);
}

uint64_t NFAStatePtrHash(void *state) {
  return (uint64_t)state;
}

bool NFAStatePtrEqual(void *state1, void *state2) {
  return state1 == state2;
}

void NFAPrint(NFA *nfa, FILE *file) {
  HashTable *table = HashTableNew(NFAStatePtrHash, NFAStatePtrEqual);
  int idx = 0;
  for (NFAState *state = nfa->init; state; state = state->next)
    HashTableEntryAdd(table, state, (void*)(uint64_t)idx++, false, false);

  fprintf(file, "digraph NFA {\n");
  fprintf(file, "  rankdir=LR;\n");

  // Print accepting states
  fprintf(file, "  node [shape=doublecircle];");
  idx = 0;
  bool empty = true;
  for (NFAState *state = nfa->init; state; state = state->next) {
    if (state->accepting) {
      fprintf(file, " %d", idx);
      empty = false;
    }
    ++idx;
  }
  if (!empty)
    fprintf(file, ";");
  fprintf(file, "\n  node [shape=circle];");

  // Print non-accepting states
  idx = 0;
  empty = true;
  for (NFAState *state = nfa->init; state; state = state->next) {
    if (!state->accepting) {
      fprintf(file, " %d", idx);
      empty = false;
    }
    ++idx;
  }
  if (!empty)
    fprintf(file, ";");
  fprintf(file, "\n");

  // Print transitions of all states
  idx = 0;
  Vector *vec = VectorNew();
  for (NFAState *state = nfa->init; state; state = state->next) {
    // Sort all transitions so that all transtitions are first grouped by
    // target state, then by the characters
    vec->size = 0;
    for (NFATransition *transition = state->transition; transition;
         transition = transition->next)
      VectorAdd(vec, transition);
    qsort(vec->arr, vec->size, sizeof(NFATransition*), NFATransitionCmp);

    // group set of transitions that goes to the same state and whose characters
    // form a continuous set together and print as one transition
    NFATransition *prev = NULL;
    unsigned char charFrom, charTo;
    for (int i = 0; i < vec->size; ++i) {
      NFATransition *transition = vec->arr[i];
      if (!prev ||
          transition->state != prev->state ||
          transition->a - prev->a != 1) {
        // Transition doesn't go to the same state as previous, or doesn't form
        // a range of characters, thus print the transition
        if (prev)
          NFATransitionPrint(idx, charFrom, charTo, prev->state, table, file);
        charFrom = charTo = transition->a;
      } else {
        charTo = transition->a;
      }
      prev = transition;
    }
    // The last bunch of transitions is printed here
    if (prev)
      NFATransitionPrint(idx, charFrom, charTo, prev->state, table, file);

    ++idx;
  }
  VectorDelete(vec);

  fprintf(file, "}\n");

  HashTableDelete(table, false, false);
}

int NFATransitionCmp(const void *a, const void *b) {
  NFATransition *pa = *((NFATransition**)a), *pb = *((NFATransition**)b);
  if (pa->state < pb->state)
    return -1;
  if (pa->state > pb->state)
    return 1;
  if (pa->a < pb->a)
    return -1;
  if (pa->a > pb->a)
    return 1;
  return 0;
}

void NFATransitionPrint(
    int fromIdx,
    unsigned char charFrom, unsigned char charTo,
    NFAState *to,
    HashTable *table,
    FILE *file) {
  HashTableEntry *entry = HashTableEntryRetrieve(table, to);
  assert(entry);
  int toIdx = (int)entry->value;
  fprintf(file, "  %d -> %d [label=\"", fromIdx, toIdx);
  NFAPrintChar(charFrom, file);
  if (charTo != charFrom) {
    fprintf(file, " ~ ");
    NFAPrintChar(charTo, file);
  }
  fprintf(file, "\"];\n");
}

void NFAPrintChar(unsigned char a, FILE *file) {
  if (IS_CONTROL_CHARACTER(a)) {
    fprintf(file, "'\\x%02x'", (unsigned)a);
  } else {
    switch (a) {
      case '\r': fprintf(file, "'\\r'"); break;
      case '\t': fprintf(file, "'\\t'"); break;
      case '\n': fprintf(file, "'\\n'"); break;
      case '"': fprintf(file, "'\\\"'"); break;
      case NFA_EPSILON: fprintf(file, "&epsilon;"); break;
      default: fprintf(file, "'%c'", a); break;
    }
  }
}
