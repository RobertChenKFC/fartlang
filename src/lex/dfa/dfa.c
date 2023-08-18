#include "lex/dfa/dfa.h"
#include "util/bitset/bitset.h"
#include "util/hashtable/hashtable.h"
#include "util/source/source.h"
#include "util/vector/vector.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
typedef struct DFAState DFAState;
typedef struct DFAStateSet DFAStateSet;
typedef struct DFAStatePartition DFAStatePartition;
typedef struct DFATransition DFATransition;

// Constants
DFAState *DFA_STATE_NULL;
DFAStateSet *DFA_STATE_SET_NULL;

// A DFA state is similar to a normal FA state, except that it records whether
// it is an initial state, and uses a DFATransition, which additionally records
// the state the transition came from, as well as the set it belongs to; also,
// it has an "index" that can be used to map between DFA states in an array and
// FA states in an array, and a "traversed" flag used in DFS for removing
// unreachable states
struct DFAState {
  bool init;
  int accepting;
  DFATransition *transition;
  DFAStateSet *set;
  DFAState *prev, *next;
  int index;
  bool traversed;
};

// A DFA state set contains a list of DFA states, an indicator as to whether it
// contains the original initial state, as well as the pointers to connect the
// sets together as a doubly linked list for easy insertion/deletion
struct DFAStateSet {
  bool containsInit;
  DFAState *head, *last;
  DFAStateSet *prev, *next;
};

// A DFA state partition is simply a list of sets, each set containing a bunch
// of DFA states
struct DFAStatePartition {
  DFAStateSet *head, *last;
};

// A DFA transition is just an FA transition that additionally records which
// state this transition came from
struct DFATransition {
  unsigned char a;
  DFAState *stateFrom, *stateTo;
  DFATransition *next;
};

// Helper functions
// Gathers all NFA states reachable from "state" using only epsilon transitions,
// obtain their indices using "table", and return them as a collected bitset;
// "numNFAStates" should contain the number of states of the NFA that "state"
// is from
Bitset *DFAEpsilonClosureFromNFAState(
    FAState *state, HashTable *table, int numNFAStates);
// Implementation of DFAEpsilonClosureFromNFAState, which takes an extra
// argument "bitset" for recursion purposes
void DFAEpsilonClosureFromNFAStateImpl(
    FAState *state, HashTable *table, Bitset *bitset);
// Given a "bitset" containing indices of NFA states, convert them to NFA states
// using "nfaStates", get their epsilon closures (which are sets of NFA states),
// take the union of all these NFA states, convert them back to indices using
// "table", and return them as a collected bitset
Bitset *DFAEpsilonClosureFromBitset(
    Bitset *bitset, FAState **nfaStates, HashTable *table);
// Add DFA state, which is a set of states from "nfaStates" represented by
// "bitset", into "table" if it doesn't exist yet. Either returns a newly
// created state or an existing state corresponding to "bitset".
// Note that if there is already a corresponding state, then the two states
// are combined, which means that if they are both accepting states, then only
// one can be chosen. In case this happens, the NFA that was listed first
// will be the one chosen, and the names of the regex stored in "regexNames",
// which is used to generate these NFAs, are printed
FAState *DFAAddBitsetStateToTable(
    Bitset *bitset, FAState **nfaStates, HashTable *table,
    Vector *regexNames);
// See DFAFromNFA for general information on this function. An extra vector
// of strings is stored in "regexNames" that contains the names of the regexes
// used to generate the NFAs that are combined to create "nfa"
FA *DFAFromNFAImpl(FA *nfa, Vector *regexNames);
// Create a new DFA state from "state" that doesn't belong to any set and with
// property "init"
DFAState *DFAStateNew(FAState *state, bool init);
// Delete "state" created with DFAStateNew, as well as the transitions it
// contains
void DFAStateDelete(DFAState *state);
// Add transition from "state1" via "a" to "state2"
void DFAStateAddTransition(DFAState *state1, unsigned char a, DFAState *state2);
// Compare function for two DFA state pointers "a" and "b"; used for sorting
int DFAStatePtrCmp(const void *a, const void *b);
// Create an empty set of DFA states
DFAStateSet *DFAStateSetNew();
// Delete "set" created with DFAStateSetNew; note that this funciton does NOT
// delete the states contained in the set
void DFAStateSetDelete(DFAStateSet *set);
// Add "state" to "set"; note that it doesn't change the set that "state"
// belongs to
void DFAStateSetAddState(DFAStateSet *set, DFAState *state);
// Remove "state" from the set it belongs to; note that it doesn't change the
// set that "state" belongs to
void DFAStateSetRemoveState(DFAState *state);
// Move all states in "set2" to "set1"
void DFAStateSetMove(DFAStateSet *set1, DFAStateSet *set2);
// Creates an empty partition of DFA state sets
DFAStatePartition *DFAStatePartitionNew();
// Delete "partition" created with DFAStatePartitionNew, as well as all the
// sets it contains, and also the states that are contained in the sets
void DFAStatePartitionDelete(DFAStatePartition *partition);
// Add "set" to "partition"
void DFAStatePartitionAddSet(DFAStatePartition *partition, DFAStateSet *set);
// Remove "set" from "partition"
void DFAStatePartitionRemoveSet(DFAStatePartition *partition, DFAStateSet *set);
// Create a new DFA transition goes from "state" to DFA_STATE_NULL (a state
// that belongs to the special set DFA_STATE_SET_NULL) via character "a"
DFATransition *DFATransitionNull(DFAState *state, unsigned char a);
// Delete "transition" created by DFATransitionNull
void DFATransitionDelete(DFATransition *transition);
// Compare function for two DFA transitions "a" and "b", which first compares
// the character used in the transitions, then compares the state of which the
// transition came from; used for sorting
int DFATransitionCmpByStateFrom(const void *a, const void *b);
// Compare function for two DFA transitions "a" and "b", which first compares
// the character used in the transitions, then compares the set that contains
// the states the transitions go to; used for sorting
int DFATransitionCmpByStateToSet(const void *a, const void *b);

void DFAInit() {
  DFA_STATE_NULL = calloc(1, sizeof(DFAState));
  DFA_STATE_SET_NULL = calloc(1, sizeof(DFAStateSet));
  DFA_STATE_NULL->set = DFA_STATE_SET_NULL;
}

Bitset *DFAEpsilonClosureFromNFAState(
    FAState *state, HashTable *table, int numNFAStates) {
  Bitset *bitset = BitsetNew(numNFAStates);
  DFAEpsilonClosureFromNFAStateImpl(state, table, bitset);
  return bitset;
}

void DFAEpsilonClosureFromNFAStateImpl(
    FAState *state, HashTable *table, Bitset *bitset) {
  HashTableEntry *entry = HashTableEntryRetrieve(table, state);
  assert(entry);
  int stateIdx = (int)(int64_t)entry->value;
  if (BitsetIsSet(bitset, stateIdx))
    return;

  BitsetSet(bitset, stateIdx);
  for (FATransition *transition = state->transition; transition;
       transition = transition->next) {
    if (transition->a != FA_EPSILON)
      continue;
    FAState *stateTo = transition->state;
    DFAEpsilonClosureFromNFAStateImpl(stateTo, table, bitset);
  }
}

Bitset *DFAEpsilonClosureFromBitset(
    Bitset *bitset, FAState **nfaStates, HashTable *table) {
  int numNFAStates = bitset->n;
  Bitset *resultBitset = BitsetNew(numNFAStates);
  Bitset *curBitset = BitsetNew(numNFAStates);
  for (int i = 0; i < numNFAStates; ++i) {
    BitsetClearAll(curBitset);
    if (BitsetIsSet(bitset, i)) {
      FAState *state = nfaStates[i];
      DFAEpsilonClosureFromNFAStateImpl(state, table, curBitset);
      BitsetOr(resultBitset, resultBitset, curBitset);
    }
  }
  BitsetDelete(curBitset);
  return resultBitset;
}

FAState *DFAAddBitsetStateToTable(
    Bitset *bitset, FAState **nfaStates, HashTable *table,
    Vector *regexNames) {
  HashTableEntry *entry = HashTableEntryRetrieve(table, bitset);
  FAState *dfaState;
  if (entry) {
    dfaState = entry->value;
    BitsetDelete(bitset);
  } else {
    int accepting = FA_ACCEPT_NONE;
    for (int i = 0; i < bitset->n; ++i) {
      FAState *state = nfaStates[i];
      if (BitsetIsSet(bitset, i) && state->accepting != FA_ACCEPT_NONE) {
        if (accepting == FA_ACCEPT_NONE) {
          accepting = state->accepting;
        } else {
          int chosenAccepting = accepting < state->accepting ?
                                accepting : state->accepting;
          if (accepting != state->accepting) {
            if (regexNames) {
              fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET
                      " Regex %s and %s are not disjoint, choosing regex %s.\n",
                      (char*)regexNames->arr[state->accepting],
                      (char*)regexNames->arr[accepting],
                      (char*)regexNames->arr[chosenAccepting]);
            } else {
              fprintf(stderr, SOURCE_COLOR_YELLOW"[Warning]"SOURCE_COLOR_RESET
                      " Token ID %d and %d are not disjoint,"
                      " choosing token %d.\n",
                      state->accepting, accepting, chosenAccepting);
            }
          }
          accepting = chosenAccepting;
        }
      }
    }
    dfaState = FAStateNew(accepting);
    HashTableEntryAdd(table, bitset, dfaState);
  }
  return dfaState;
}

FA *DFAFromNFAImpl(FA *nfa, Vector *regexNames) {
  // Construct a hashtable to convert from NFA states to indices, and an array
  // to convert from indices back to NFA states
  HashTable *nfaStateToIdx = HashTableNew(
      FAStatePtrHash, FAStatePtrEqual, NULL, NULL);
  for (FAState *state = nfa->init; state; state = state->next)
    HashTableEntryAdd(nfaStateToIdx, state, (void*)nfaStateToIdx->size);
  FAState **idxToNFAState = malloc(sizeof(FAState*) * nfaStateToIdx->size);
  int numNFAStates = 0;
  for (FAState *state = nfa->init; state; state = state->next)
    idxToNFAState[numNFAStates++] = state;

  // A set containing the epsilon closure of an NFA state is a DFA state
  HashTable *bitsetToDFAState = HashTableNew(
      BitsetHash, BitsetEqual, BitsetDelete, NULL);
  for (FAState *state = nfa->init; state; state = state->next) {
    Bitset *bitset = DFAEpsilonClosureFromNFAState(
        state, nfaStateToIdx, numNFAStates);
    DFAAddBitsetStateToTable(
        bitset, idxToNFAState, bitsetToDFAState, regexNames);
  }

  // Iterate through all DFA states "dfaStateFrom", each of which is a set of
  // NFA states, gather the epsilon closures of all NFA states reachable from
  // NFA states in "dfaStateFrom" via one transition using the character "a",
  // then this gathered set of NFA states forms another DFA state "dfaStateTo",
  // and finally "dfaStateFrom" --"a"--> "dfaStateTo" is a transition in the DFA
  FA *dfa = FANew();
  for (HashTableEntry *dfaStateEntry = bitsetToDFAState->head; dfaStateEntry;
       dfaStateEntry = dfaStateEntry->nextInTable) {
    Bitset *dfaTransitions[128] = {false};
    Bitset *dfaBitsetFrom = dfaStateEntry->key;
    for (int i = 0; i < dfaBitsetFrom->n; ++i) {
      if (!BitsetIsSet(dfaBitsetFrom, i))
        continue;
      FAState *nfaStateFrom = idxToNFAState[i];
      for (FATransition *transition = nfaStateFrom->transition; transition;
           transition = transition->next) {
        if (transition->a == FA_EPSILON)
          continue;
        FAState *nfaStateTo = transition->state;
        HashTableEntry *entry = HashTableEntryRetrieve(
            nfaStateToIdx, nfaStateTo);
        assert(entry);
        int nfaStateIdxTo = (int)(int64_t)entry->value;
        Bitset *dfaBitsetTo = dfaTransitions[transition->a];
        if (!dfaBitsetTo)
          dfaTransitions[transition->a] = dfaBitsetTo = BitsetNew(numNFAStates);
        BitsetSet(dfaBitsetTo, nfaStateIdxTo);
      }
    }

    FAState *dfaStateFrom = dfaStateEntry->value;
    FAAddState(dfa, dfaStateFrom);
    for (unsigned char a = 0; a < 128; ++a) {
      Bitset *dfaBitsetTo = dfaTransitions[a];
      if (!dfaBitsetTo)
        continue;
      Bitset* dfaBitsetEpsilonClosureTo = DFAEpsilonClosureFromBitset(
          dfaBitsetTo, idxToNFAState, nfaStateToIdx);
      BitsetDelete(dfaBitsetTo);
      FAState *dfaStateTo = DFAAddBitsetStateToTable(
          dfaBitsetEpsilonClosureTo, idxToNFAState, bitsetToDFAState,
          regexNames);
      FAStateAddTransition(dfaStateFrom, a, dfaStateTo);
    }
  }

  HashTableDelete(nfaStateToIdx);
  free(idxToNFAState);
  HashTableDelete(bitsetToDFAState);

  return dfa;
}

FA *DFAFromNFA(FA *nfa) {
  return DFAFromNFAImpl(nfa, NULL);
}

FA *DFAFromNFAs(Vector *nfas) {
  FAState *init = FAStateNew(FA_ACCEPT_NONE);
  FA *combinedNFA = FANew();
  FAAddState(combinedNFA, init);
  int numNFAs = nfas->size;
  bool containNames = true;
  for (int i = 0; i < numNFAs; ++i) {
    FA *nfa = nfas->arr[i];
    FAAddStates(combinedNFA, nfa);
    FAStateAddTransition(init, FA_EPSILON, nfa->init);
    for (FAState *state = nfa->init; state; state = state->next) {
      if (state->accepting != FA_ACCEPT_NONE)
        state->accepting = i;
    }
    if (!nfa->name)
      containNames = false;
  }

  // The lexer will store the names of the regexes used to generated the NFAs
  // in each NFA. We use the names only if all of the NFAs are assigned a name.
  Vector *regexNames = NULL;
  if (containNames) {
    regexNames = VectorNew();
    VectorReserve(regexNames, numNFAs);
    for (int i = 0; i < numNFAs; ++i) {
      FA *nfa = nfas->arr[i];
      VectorAdd(regexNames, nfa->name);
    }
  }

  FA *dfa = DFAFromNFAImpl(combinedNFA, regexNames);

  if (regexNames)
    VectorDelete(regexNames);

  // To make sure that the NFAs that are passed in this function are not
  // deleted, we only delete the initial state as well as the transitions we
  // added, instead of directly using FADelete on "combinedNFA"; also, we need
  // to split the list of states of the original NFA's apart because they
  // are joined together in "combinedNFA"
  FATransition *nextTransition;
  for (FATransition *transition = init->transition; transition;
       transition = nextTransition) {
    nextTransition = transition->next;
    free(transition);
  }
  for (int i = 0; i < numNFAs; ++i) {
    FA *nfa = nfas->arr[i];
    if (nfa->last)
      nfa->last->next = NULL;
  }
  free(init);
  free(combinedNFA);

  return dfa;
}

DFAState *DFAStateNew(FAState *state, bool init) {
  DFAState *dfaState = calloc(1, sizeof(DFAState));
  dfaState->accepting = state->accepting;
  dfaState->init = init;
  return dfaState;
}

void DFAStateDelete(DFAState *state) {
  DFATransition *nextTransition;
  for (DFATransition *transition = state->transition; transition;
       transition = nextTransition) {
    nextTransition = transition->next;
    free(transition);
  }
  free(state);
}

void DFAStateAddTransition(
    DFAState *state1, unsigned char a, DFAState *state2) {
  DFATransition *transition = malloc(sizeof(DFATransition));
  transition->a = a;
  transition->stateFrom = state1;
  transition->stateTo = state2;
  transition->next = state1->transition;
  state1->transition = transition;
}

int DFAStatePtrCmp(const void *a, const void *b) {
  const DFAState *state1 = *((const DFAState **)a);
  const DFAState *state2 = *((const DFAState **)b);
  if (state1 < state2)
    return -1;
  if (state1 > state2)
    return 1;
  return 0;
}

DFAStateSet *DFAStateSetNew() {
  DFAStateSet *set = calloc(1, sizeof(DFAStateSet));
  return set;
}

void DFAStateSetDelete(DFAStateSet *set) {
  free(set);
}

void DFAStateSetAddState(DFAStateSet *set, DFAState *state) {
  state->prev = set->last;
  if (set->last)
    set->last->next = state;
  else
    set->head = state;
  set->last = state;
  set->containsInit = set->containsInit || state->init;
}

void DFAStateSetRemoveState(DFAState *state) {
  DFAStateSet *set = state->set;
  if (state->prev)
    state->prev->next = state->next;
  else
    set->head = state->next;
  if (state->next)
    state->next->prev = state->prev;
  else
    set->last = state->prev;
  state->prev = state->next = NULL;
  set->containsInit = set->containsInit && !state->init;
}

void DFAStateSetMove(DFAStateSet *set1, DFAStateSet *set2) {
  for (DFAState *state = set2->head; state; state = state->next)
    state->set = set1;
  if (set1->last)
    set1->last->next = set2->head;
  else
    set1->last = set2->head;
  if (set2->head)
    set2->head->prev = set1->last;
  set2->head = set2->last = NULL;
  set1->containsInit = set1->containsInit || set2->containsInit;
  set2->containsInit = false;
}

DFAStatePartition *DFAStatePartitionNew() {
  DFAStatePartition *partition = calloc(1, sizeof(DFAStatePartition));
  return partition;
}

void DFAStatePartitionDelete(DFAStatePartition *partition) {
  DFAStateSet *nextSet;
  for (DFAStateSet *set = partition->head; set; set = nextSet) {
    DFAState *nextState;
    for (DFAState *state = set->head; state; state = nextState) {
      nextState = state->next;
      DFAStateDelete(state);
    }
    nextSet = set->next;
    DFAStateSetDelete(set);
  }
  free(partition);
}

void DFAStatePartitionAddSet(DFAStatePartition *partition, DFAStateSet *set) {
  set->prev = partition->last;
  if (partition->last)
    partition->last->next = set;
  else
    partition->head = set;
  partition->last = set;
}

void DFAStatePartitionRemoveSet(
    DFAStatePartition *partition, DFAStateSet *set) {
  if (set->prev)
    set->prev->next = set->next;
  else
    partition->head = set->next;
  if (set->next)
    set->next->prev = set->prev;
  else
    partition->last = set->prev;
}

DFATransition *DFATransitionNull(DFAState *state, unsigned char a) {
  DFATransition *transition = malloc(sizeof(DFATransition));
  transition->a = a;
  transition->stateFrom = state;
  transition->stateTo = DFA_STATE_NULL;
  return transition;
}

void DFATransitionDelete(DFATransition *transition) {
  free(transition);
}

int DFATransitionCmpByStateFrom(const void *a, const void *b) {
  const DFATransition *transition1 = *((const DFATransition **)a);
  const DFATransition *transition2 = *((const DFATransition **)b);
  if (transition1->a < transition2->a)
    return -1;
  if (transition1->a > transition2->a)
    return 1;
  return DFAStatePtrCmp(&transition1->stateFrom, &transition2->stateFrom);
}

int DFATransitionCmpByStateToSet(const void *a, const void *b) {
  const DFATransition *transition1 = *((const DFATransition **)a);
  const DFATransition *transition2 = *((const DFATransition **)b);
  if (transition1->a < transition2->a)
    return -1;
  if (transition1->a > transition2->a)
    return 1;
  if (transition1->stateTo->set < transition2->stateTo->set)
    return -1;
  if (transition1->stateTo->set > transition2->stateTo->set)
    return 1;
  return 0;
}

FA *DFAMinimize(FA *dfa) {
  // Initially partition the states according to their "accepting" properties,
  // and also construct the table of converting original FA states to new DFA
  // states so that the new DFA transitions can be constructed properly
  DFAStatePartition *partition = DFAStatePartitionNew();
  Vector *acceptingSets = VectorNew();
  DFAStateSet *rejectingSet = DFAStateSetNew();
  DFAStatePartitionAddSet(partition, rejectingSet);
  HashTable *toDFAState = HashTableNew(
      FAStatePtrHash, FAStatePtrEqual, NULL, NULL);
  for (FAState *state = dfa->init; state; state = state->next) {
    DFAState *dfaState = DFAStateNew(state, state == dfa->init);
    HashTableEntryAdd(toDFAState, state, dfaState);
    int accepting = state->accepting;
    if (accepting == FA_ACCEPT_NONE) {
      DFAStateSetAddState(rejectingSet, dfaState);
      dfaState->set = rejectingSet;
    } else {
      DFAStateSet *acceptingSet;
      while (acceptingSets->size <= accepting) {
        acceptingSet = DFAStateSetNew();
        VectorAdd(acceptingSets, acceptingSet);
        DFAStatePartitionAddSet(partition, acceptingSet);
      }
      acceptingSet = acceptingSets->arr[accepting];
      DFAStateSetAddState(acceptingSet, dfaState);
      dfaState->set = acceptingSet;
    }
  }
  VectorDelete(acceptingSets);

  // Construct the DFA transitions for the newly created DFA states
  for (FAState *stateFrom = dfa->init; stateFrom; stateFrom = stateFrom->next) {
    HashTableEntry *entry = HashTableEntryRetrieve(toDFAState, stateFrom);
    assert(entry);
    DFAState *dfaStateFrom = entry->value;
    for (FATransition *transition = stateFrom->transition; transition;
         transition = transition->next) {
      entry = HashTableEntryRetrieve(toDFAState, transition->state);
      assert(entry);
      DFAState *dfaStateTo = entry->value;
      DFAStateAddTransition(dfaStateFrom, transition->a, dfaStateTo);
    }
  }
  HashTableDelete(toDFAState);

  bool changed;
  Vector *states = VectorNew();
  Vector *transitions = VectorNew();
  Vector *sets = VectorNew();
  do {
    changed = false;
    DFAStateSet *nextSet;
    for (DFAStateSet *set = partition->head; set; set = nextSet) {
      // Collect all states and transitions
      states->size = 0;
      transitions->size = 0;
      for (DFAState *state = set->head; state; state = state->next) {
        VectorAdd(states, state);
        for (DFATransition *transition = state->transition; transition;
             transition = transition->next)
          VectorAdd(transitions, transition);
      }

      // Sort the states in the set, and also sort the transitions, first by
      // character then by the state it comes from, so that we can check given
      // a character, which states are missing transitions (a state might not
      // accept a particular character, hence the missing transition), and add
      // them back as transitions that go to DFA_STATE_NULL (a state that
      // doesn't belong to any set)
      qsort(states->arr, states->size, sizeof(states->arr[0]), DFAStatePtrCmp);
      qsort(transitions->arr, transitions->size, sizeof(transitions->arr[0]),
            DFATransitionCmpByStateFrom);
      int numStates = states->size;
      int numTransitions = transitions->size;
      for (int transitionIdx = 0; transitionIdx < numTransitions; ) {
        DFATransition *transition = transitions->arr[transitionIdx];
        unsigned char a = transition->a;
        for (int stateIdx = 0; stateIdx < numStates; ++stateIdx) {
          DFAState *state = states->arr[stateIdx];
          if (transitionIdx == numTransitions ||
              (transition =
               transitions->arr[transitionIdx])->stateFrom != state ||
              transition->a != a) {
            DFATransition *nullTransition = DFATransitionNull(state, a);
            VectorAdd(transitions, nullTransition);
          } else {
            ++transitionIdx;
          }
        }
      }

      // Sort the transitions again, first by character then by the set
      // that the state it transitions to belongs, and create new sets to gather
      // them together
      numTransitions = transitions->size;
      qsort(transitions->arr, numTransitions, sizeof(transitions->arr[0]),
            DFATransitionCmpByStateToSet);
      DFAStateSet *newSet = DFAStateSetNew();
      sets->size = 0;
      VectorAdd(sets, newSet);
      for (int i = 0; i < numTransitions; ++i) {
        DFATransition *transition = transitions->arr[i];
        DFATransition *nextTransition;
        DFAStateSetRemoveState(transition->stateFrom);
        DFAStateSetAddState(newSet, transition->stateFrom);
        if (i == numTransitions - 1 ||
            transition->a != (nextTransition = transitions->arr[i + 1])->a) {
          if (sets->size == 1) {
            // All transitions of the same character goes to states belonging to
            // the set, so don't modify the current partition
            set->head = newSet->head;
            set->last = newSet->last;
            set->containsInit = newSet->containsInit;
            DFAStateSetDelete(newSet);
            nextSet = set->next;
          } else if (sets->size > 1) {
            // All transitions of the same character goes to states belonging to
            // more than 1 set, so delete the current set and add the new sets
            // to the current partition
            for (int j = 0; j < sets->size; ++j) {
              newSet = sets->arr[j];
              DFAStatePartitionAddSet(partition, newSet);
              for (DFAState *state = newSet->head; state; state = state->next)
                state->set = newSet;
            }
            nextSet = set->next;
            DFAStatePartitionRemoveSet(partition, set);
            DFAStateSetDelete(set);
            changed = true;
          }
          sets->size = 0;
          newSet = DFAStateSetNew();
          VectorAdd(sets, newSet);
          if (changed)
            break;
        } else if (transition->stateTo->set != nextTransition->stateTo->set) {
          newSet = DFAStateSetNew();
          VectorAdd(sets, newSet);
        }
      }
      // Must delete the null transitions because they won't be deleted when
      // deleting the entire partition
      for (int i = 0; i < numTransitions; ++i) {
        DFATransition *transition = transitions->arr[i];
        if (transition->stateTo == DFA_STATE_NULL)
          DFATransitionDelete(transition);
      }

      // The last created newSet is always extra
      DFAStateSetDelete(newSet);

      // nextSet will not be updated when the for loop isn't executed
      if (transitions->size == 0)
        nextSet = set->next;
    }
  } while (changed);
  VectorDelete(transitions);
  VectorDelete(sets);

  // Collect one state from each set in the partition, record which state added
  // belongs to a set that contains the original initial state, and swap that
  // state to the front of the array so that it gets added first as the new
  // initial state
  states->size = 0;
  int currentIdx = 0, initIdx;
  for (DFAStateSet *set = partition->head; set; set = set->next, ++currentIdx) {
    VectorAdd(states, set->head);
    if (set->containsInit)
      initIdx = currentIdx;
  }
  DFAState *tmpState = states->arr[0];
  states->arr[0] = states->arr[initIdx];
  states->arr[initIdx] = tmpState;

  // Go through all DFA states, and mark the states that are traversable from
  // the initial state
  int numStates = states->size;
  for (int i = 0; i < numStates; ++i) {
    DFAState *state = states->arr[i];
    state->traversed = false;
  }
  Vector *minDFAStates = VectorNew();
  VectorAdd(minDFAStates, states->arr[0]);
  while (minDFAStates->size != 0) {
    DFAState *state = minDFAStates->arr[--minDFAStates->size];
    if (!state->traversed) {
      state->traversed = true;
      for (DFATransition *transition = state->transition; transition;
           transition = transition->next) {
        VectorAdd(minDFAStates, transition->stateTo->set->head);
      }
    }
  }

  // Add all traversable states to the new DFA
  FA *minDFA = FANew();
  minDFAStates->size = 0;
  for (int i = 0; i < numStates; ++i) {
    DFAState *dfaState = states->arr[i];
    if (dfaState->traversed) {
      dfaState->index = minDFAStates->size;
      FAState *minDFAState = FAStateNew(dfaState->accepting);
      FAAddState(minDFA, minDFAState);
      VectorAdd(minDFAStates, minDFAState);
    }
  }

  // Add all transitions to the new DFA, converting the transitions from the
  // using the states in original DFA to using the chosen states instead
  for (int i = 0; i < numStates; ++i) {
    DFAState *dfaState = states->arr[i];
    if (dfaState->traversed) {
      FAState *minDFAStateFrom = minDFAStates->arr[dfaState->index];
      for (DFATransition *dfaTransition = dfaState->transition; dfaTransition;
           dfaTransition = dfaTransition->next) {
        DFAState *dfaStateTo = dfaTransition->stateTo->set->head;
        FAState *minDFAStateTo = minDFAStates->arr[dfaStateTo->index];
        FAStateAddTransition(minDFAStateFrom, dfaTransition->a, minDFAStateTo);
      }
    }
  }

  VectorDelete(states);
  VectorDelete(minDFAStates);
  DFAStatePartitionDelete(partition);

  return minDFA;
}
