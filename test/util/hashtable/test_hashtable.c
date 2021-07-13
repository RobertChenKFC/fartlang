#include "util/hashtable/hashtable.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define M 78885403583278429
#define N 1000
#define S(s) (strdup(s))

char *RandStr() {
  char *str = malloc(sizeof(char) * N);
  for (int i = 0; i < N - 1; ++i)
    str[i] = (rand() % 128) + 1;
  str[N - 1] = 0;
  return str;
}

uint64_t Hash(void *p) {
  char *str = p;
  uint64_t hash = 0;
  for (int i = 0; str[i] != 0; ++i)
    hash = ((hash * 129) % M + ((str[i] + 1) % M)) % M;
  return hash;
}

bool Equal(void *p1, void *p2) {
  return strcmp(p1, p2) == 0;
}

int main() {
  HashTable *table = HashTableNew(Hash, Equal);

  // 1. Add key value pair to hash table and retrieve
  HashTableEntryAdd(table, S("hello"), (void*)1, true, false);
  HashTableEntry *entry = HashTableEntryRetrieve(table, "hello");
  assert(entry && entry->value == (void*)1);

  // 2. Replace key value pair in hash table and retrieve
  HashTableEntryAdd(table, S("hello"), (void*)2, true, false);
  entry = HashTableEntryRetrieve(table, "hello");
  assert(entry && entry->value == (void*)2);

  // 3. Adding and replacing multiple entries
  HashTableEntryAdd(table, S("hello"), (void*)1, true, false);
  HashTableEntryAdd(table, S("world"), (void*)2, true, false);
  HashTableEntryAdd(table, S("my"), (void*)3, true, false);
  HashTableEntryAdd(table, S("name"), (void*)4, true, false);
  HashTableEntryAdd(table, S("is"), (void*)5, true, false);
  HashTableEntryAdd(table, S("Robert"), (void*)6, true, false);
  entry = HashTableEntryRetrieve(table, "hello");
  assert(entry && entry->value == (void*)1);
  entry = HashTableEntryRetrieve(table, "world");
  assert(entry && entry->value == (void*)2);
  entry = HashTableEntryRetrieve(table, "my");
  assert(entry && entry->value == (void*)3);
  entry = HashTableEntryRetrieve(table, "name");
  assert(entry && entry->value == (void*)4);
  entry = HashTableEntryRetrieve(table, "is");
  assert(entry && entry->value == (void*)5);
  entry = HashTableEntryRetrieve(table, "Robert");
  assert(entry && entry->value == (void*)6);

  // 4. Retrieving non-existent entries
  entry = HashTableEntryRetrieve(table, "this string is not in the table");
  assert(!entry);
  HashTableEntryAdd(table, S("this string is not in the table"), (void*)69,
                    true, false);
  entry = HashTableEntryRetrieve(table, "this string is not in the table");
  assert(entry && entry->value == (void*)69);

  // 5. Deleting entries
  HashTableEntryDelete(table, entry, true, false);
  entry = HashTableEntryRetrieve(table, "this string is not in the table");
  assert(!entry);

  // 6. Adding entries until resize
  uint64_t curCapacity = table->capacity;
  while (table->capacity == curCapacity) {
    char *str = RandStr();
    HashTableEntryAdd(table, str, (void*)(uint64_t)rand(), true, false);
  }
  entry = HashTableEntryRetrieve(table, "hello");
  assert(entry && entry->value == (void*)1);
  entry = HashTableEntryRetrieve(table, "Robert");
  assert(entry && entry->value == (void*)6);
  HashTableEntryDelete(table, entry, true, false);
  entry = HashTableEntryRetrieve(table, "Robert");
  assert(!entry);

  HashTableDelete(table, true, false);
}
