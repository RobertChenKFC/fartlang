#include "lex/fa/fa.h"
#include "util/hashtable/hashtable.h"
#include "util/vector/vector.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// Constants
// Check if a character is an ASCII control character
#define IS_CONTROL_CHARACTER(a) (((a) >= 0 && (a) <= 31) || (a) == 127)

// Prints a transition from state with index "fromIdx", via characters from
// "charFrom" to "charTo", to state "to", where state "to" is translated to
// its index using "table", to "file"
void FATransitionPrint(
    int fromIdx,
    unsigned char charFrom, unsigned char charTo,
    FAState *to,
    HashTable *table,
    FILE *file);
// Prints a character "a" to "file", with special care for control characters
void FAPrintChar(unsigned char a, FILE *file);

FA *FANew() {
  FA *fa = calloc(1, sizeof(FA));
  return fa;
}

void FADelete(FA *fa) {
  FAState *nextState;
  for (FAState *state = fa->init; state; state = nextState) {
    FATransition *nextTransition;
    for (FATransition *transition = state->transition; transition;
         transition = nextTransition) {
      nextTransition = transition->next;
      free(transition);
    }
    nextState = state->next;
    free(state);
  }
  free(fa);
}

FAState *FAStateNew(bool accepting) {
  FAState *state = calloc(1, sizeof(FAState));
  state->accepting = accepting;
  return state;
}

void FAAddState(FA *fa, FAState *state) {
  if (fa->init == NULL) {
    fa->init = fa->last = state;
  } else {
    fa->last->next = state;
    fa->last = state;
  }
}

void FAAddStates(FA *fa1, FA *fa2) {
  if (fa1->init == NULL) {
    fa1->init = fa2->init;
    fa1->last = fa2->last;
  } else {
    fa1->last->next = fa2->init;
    fa1->last = fa2->last;
  }
}

void FAStateAddTransition(
    FAState *state1, unsigned char a, FAState *state2) {
  FATransition *transition = malloc(sizeof(FATransition));
  transition->a = a;
  transition->state = state2;
  transition->next = state1->transition;
  state1->transition = transition;
}

void FATransitionPrint(
    int fromIdx,
    unsigned char charFrom, unsigned char charTo,
    FAState *to,
    HashTable *table,
    FILE *file) {
  HashTableEntry *entry = HashTableEntryRetrieve(table, to);
  assert(entry);
  int toIdx = (int)(int64_t)entry->value;
  fprintf(file, "  %d -> %d [label=\"", fromIdx, toIdx);
  FAPrintChar(charFrom, file);
  if (charTo != charFrom) {
    fprintf(file, " ~ ");
    FAPrintChar(charTo, file);
  }
  fprintf(file, "\"];\n");
}

void FAPrintChar(unsigned char a, FILE *file) {
  if (IS_CONTROL_CHARACTER(a)) {
    fprintf(file, "'\\x%02x'", (unsigned)a);
  } else {
    switch (a) {
      case '\r': fprintf(file, "'\\r'"); break;
      case '\t': fprintf(file, "'\\t'"); break;
      case '\n': fprintf(file, "'\\n'"); break;
      case '"': fprintf(file, "'\\\"'"); break;
      case FA_EPSILON: fprintf(file, "&epsilon;"); break;
      default: fprintf(file, "'%c'", a); break;
    }
  }
}

void FAPrint(FA *fa, FILE *file) {
  HashTable *table = HashTableNew(FAStatePtrHash, FAStatePtrEqual,
                                  NULL, NULL);
  int idx = 0;
  for (FAState *state = fa->init; state; state = state->next)
    HashTableEntryAdd(table, state, (void*)(uint64_t)idx++);

  fprintf(file, "digraph FA {\n");
  fprintf(file, "  rankdir=LR;\n");

  // Print accepting states
  fprintf(file, "  node [shape=doublecircle];");
  idx = 0;
  bool empty = true;
  for (FAState *state = fa->init; state; state = state->next) {
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
  for (FAState *state = fa->init; state; state = state->next) {
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
  for (FAState *state = fa->init; state; state = state->next) {
    // Sort all transitions so that all transtitions are first grouped by
    // target state, then by the characters
    vec->size = 0;
    for (FATransition *transition = state->transition; transition;
         transition = transition->next)
      VectorAdd(vec, transition);
    qsort(vec->arr, vec->size, sizeof(FATransition*), FATransitionCmp);

    // group set of transitions that goes to the same state and whose characters
    // form a continuous set together and print as one transition
    FATransition *prev = NULL;
    unsigned char charFrom, charTo;
    for (int i = 0; i < vec->size; ++i) {
      FATransition *transition = vec->arr[i];
      if (!prev ||
          transition->state != prev->state ||
          transition->a - prev->a != 1) {
        // Transition doesn't go to the same state as previous, or doesn't form
        // a range of characters, thus print the transition
        if (prev)
          FATransitionPrint(idx, charFrom, charTo, prev->state, table, file);
        charFrom = charTo = transition->a;
      } else {
        charTo = transition->a;
      }
      prev = transition;
    }
    // The last bunch of transitions is printed here
    if (prev)
      FATransitionPrint(idx, charFrom, charTo, prev->state, table, file);

    ++idx;
  }
  VectorDelete(vec);

  fprintf(file, "}\n");

  HashTableDelete(table);
}

uint64_t FAStatePtrHash(void *state) {
  return (uint64_t)state;
}

bool FAStatePtrEqual(void *state1, void *state2) {
  return state1 == state2;
}

int FATransitionCmp(const void *a, const void *b) {
  FATransition *pa = *((FATransition**)a), *pb = *((FATransition**)b);
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
