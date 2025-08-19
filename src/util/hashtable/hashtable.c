#include "util/hashtable/hashtable.h"
#include <stdlib.h>

// Hash table capacities are always prime numbers, and every time a growth is
// needed, the capacity grows to the next prime number greater than the current
// capacity times 2
const uint64_t HASH_TABLE_CAPACITIES[] = {
  67ULL, 137ULL, 277ULL, 557ULL, 1117ULL, 2237ULL, 4481ULL, 8963ULL, 17929ULL,
  35863ULL, 71741ULL, 143483ULL, 286973ULL, 573953ULL, 1147921ULL, 2295859ULL,
  4591721ULL, 9183457ULL, 18366923ULL, 36733847ULL, 73467739ULL, 146935499ULL,
  293871013ULL, 587742049ULL, 1175484103ULL, 2350968209ULL, 4701936433ULL,
  9403872883ULL, 18807745819ULL, 37615491649ULL, 75230983309ULL,
  150461966647ULL, 300923933311ULL, 601847866637ULL, 1203695733337ULL,
  2407391466731ULL, 4814782933489ULL, 9629565867073ULL, 19259131734151ULL,
  38518263468319ULL, 77036526936769ULL, 154073053873553ULL, 308146107747157ULL,
  616292215494337ULL, 1232584430988709ULL, 2465168861977429ULL,
  4930337723954869ULL, 9860675447909777ULL, 19721350895819579ULL,
  39442701791639197ULL, 78885403583278429ULL, 157770807166556917ULL,
  315541614333113837ULL, 631083228666227807ULL, 1262166457332455659ULL,
  2524332914664911359ULL, 5048665829329822783ULL, 10097331658659645683ULL
};

// Helper functions
// Creates a new hash table entry with "key" and "value"
HashTableEntry *HashTableEntryNew(void *key, void *value);
// Add "entry" to "table"; using this function avoids touching the hash table
// bucket itself (doesn't alter the bucket list)
void HashTableAddToList(HashTable *table, HashTableEntry *entry);
// Add "entry" to "bucket"; using this function avoids touching the hash table
// itself (doesn't alter the hash table entries list)
void HashTableBucketAdd(HashTableBucket *bucket, HashTableEntry *entry);

HashTable *HashTableNew(
    HashTableHashFunction hash, HashTableEqualFunction equal,
    HashTableDestructor keyDelete, HashTableDestructor valDelete) {
  HashTable *table = malloc(sizeof(HashTable));
  table->capacityIdx = 0;
  table->capacity = HASH_TABLE_CAPACITIES[0];
  table->size = 0;
  table->buckets = calloc(table->capacity, sizeof(HashTableBucket));
  table->hash = hash;
  table->equal = equal;
  table->keyDelete = keyDelete;
  table->valDelete = valDelete;
  table->head = NULL;
  table->last = NULL;
  return table;
}

void HashTableDelete(HashTable *table) {
  free(table->buckets);
  HashTableEntry *next;
  for (HashTableEntry *entry = table->head; entry; entry = next) {
    next = entry->nextInTable;
    if (table->keyDelete)
      table->keyDelete(entry->key);
    if (table->valDelete)
      table->valDelete(entry->value);
    free(entry);
  }
  free(table);
}

HashTableEntry *HashTableEntryNew(void *key, void *value) {
  HashTableEntry *entry = calloc(1, sizeof(HashTableEntry));
  entry->key = key;
  entry->value = value;
  return entry;
}

void HashTableAddToList(HashTable *table, HashTableEntry *entry) {
  if (table->last) {
    table->last->nextInTable = entry;
    entry->prevInTable = table->last;
    table->last = entry;
  } else {
    table->head = table->last = entry;
  }
}

void HashTableBucketAdd(HashTableBucket *bucket, HashTableEntry *entry) {
  if (bucket->last) {
    bucket->last->next = entry;
    entry->prev = bucket->last;
    bucket->last = entry;
  } else {
    bucket->head = bucket->last = entry;
  }
}

void HashTableEntryAdd(HashTable *table, void *key, void *value) {
  uint64_t hash = table->hash(key);
  HashTableBucket *bucket = &table->buckets[hash % table->capacity];
  for (HashTableEntry *entry = bucket->head; entry; entry = entry->next) {
    if (table->equal(key, entry->key)) {
      if (table->keyDelete)
        table->keyDelete(entry->key);
      if (table->valDelete)
        table->valDelete(entry->value);
      entry->key = key;
      entry->value = value;
      return;
    }
  }

  if (++table->size >= table->capacity * HASHTABLE_LOAD_FACTOR) {
    free(table->buckets);
    table->capacity = HASH_TABLE_CAPACITIES[++table->capacityIdx];
    table->buckets = calloc(table->capacity, sizeof(HashTableBucket));
    for (HashTableEntry *entry = table->head; entry;
         entry = entry->nextInTable) {
      uint64_t curHash = table->hash(entry->key);
      entry->prev = entry->next = NULL;
      HashTableBucketAdd(&table->buckets[curHash % table->capacity], entry);
    }
    bucket = &table->buckets[hash % table->capacity];
  }

  HashTableEntry *entry = HashTableEntryNew(key, value);
  HashTableAddToList(table, entry);
  HashTableBucketAdd(bucket, entry);
}

HashTableEntry *HashTableEntryRetrieve(HashTable *table, void *key) {
  uint64_t hash = table->hash(key);
  HashTableBucket *bucket = &table->buckets[hash % table->capacity];
  for (HashTableEntry *entry = bucket->head; entry; entry = entry->next) {
    if (table->equal(key, entry->key))
      return entry;
  }
  return NULL;
}

void HashTableEntryDelete(HashTable *table, HashTableEntry *entry) {
  if (entry->prevInTable)
    entry->prevInTable->nextInTable = entry->nextInTable;
  if (entry->nextInTable)
    entry->nextInTable->prevInTable = entry->prevInTable;
  if (table->head == entry)
    table->head = entry->nextInTable;
  if (table->last == entry)
    table->last = entry->prevInTable;

  uint64_t hash = table->hash(entry->key);
  HashTableBucket *bucket = &table->buckets[hash % table->capacity];
  if (entry->prev)
    entry->prev->next = entry->next;
  if (entry->next)
    entry->next->prev = entry->prev;
  if (bucket->head == entry)
    bucket->head = entry->next;
  if (bucket->last == entry)
    bucket->last = entry->prev;

  if (table->keyDelete)
    table->keyDelete(entry->key);
  if (table->valDelete)
    table->valDelete(entry->value);
  free(entry);

  --table->size;
}

void HashTablePtrHash(void *p) {
  return (uint64_t)p;
}

bool HashTablePtrEqual(void *p, void *q) {
  return p == q;
}
