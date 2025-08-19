#include "lex/fa/fa.h"
#include "util/hashtable/hashtable.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main() {
  FA *fa = FANew();

  // 1. Test adding states (individually)
  FAState *state = FAStateNew(FA_ACCEPT_NONE);
  FAState *state2 = FAStateNew(0);
  FAAddState(fa, state);
  FAAddState(fa, state2);
  FILE *file = fopen("1.out", "w");
  FAPrint(fa, file);
  fclose(file);

  // 2. Test adding states (from another FA)
  FA *fa2 = FANew();
  FAState *state3 = FAStateNew(FA_ACCEPT_NONE);
  FAAddState(fa2, state3);
  FAAddStates(fa, fa2);
  file = fopen("2.out", "w");
  FAPrint(fa, file);
  free(fa2);

  // 3. Test adding transitions
  FAStateAddTransition(state, 'a', state2);
  FAStateAddTransition(state, 'b', state2);
  FAStateAddTransition(state2, 'c', state3);
  FAStateAddTransition(state3, FA_EPSILON, state);
  file = fopen("3.out", "w");
  FAPrint(fa, file);

  // 4. Test FA state pointers in hash table
  HashTable *table = HashTableNew(
      HashTablePtrHash, HashTablePtrEqual, NULL, NULL);
  HashTableEntryAdd(table, state, NULL);
  assert(HashTableEntryRetrieve(table, state));
  assert(HashTableEntryRetrieve(table, state2) == NULL);
  HashTableEntryAdd(table, state2, NULL);
  assert(HashTableEntryRetrieve(table, state2));
  assert(HashTableEntryRetrieve(table, state3) == NULL);
  HashTableEntryAdd(table, state3, NULL);
  assert(HashTableEntryRetrieve(table, state3));
  HashTableDelete(table);

  FADelete(fa);
}
