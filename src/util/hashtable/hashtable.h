#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdint.h>
#include <stdbool.h>

// Constants
// How full a hash table should be before resizing
#define HASHTABLE_LOAD_FACTOR 0.75

// A hash table entry, which contains the keys and values, the next in the
// bucket, and the next in the entire table
typedef struct HashTableEntry {
  void *key, *value;
  struct HashTableEntry *prev, *prevInTable;
  struct HashTableEntry *next, *nextInTable;
} HashTableEntry;

// A hash table bucket, which mainly contains a list of hash table entries
typedef struct {
  HashTableEntry *head, *last;
} HashTableBucket;

// A hash function type that converts a "key" to a 64-bit unsigned integer
typedef uint64_t (*HashTableHashFunction)(void *key);

// A function type that checks if two keys are equal
typedef bool (*HashTableEqualFunction)(void *key1, void *key2);

// A hash table structure containing an array of buckets, the corresponding
// hash function and equal function to use, as a list containing all hash table
// entries
typedef struct {
  HashTableBucket *buckets;
  int capacityIdx;
  uint64_t size, capacity;
  HashTableHashFunction hash;
  HashTableEqualFunction equal;
  HashTableEntry *head, *last;
} HashTable;

// Creates a new hash table that uses the hash function "f" and equal
// function "g"
HashTable *HashTableNew(
    HashTableHashFunction hash, HashTableEqualFunction equal);
// Delete "table" created with HashTableNew
void HashTableDelete(HashTable *table, bool deleteKey, bool deleteValue);
// Add or replace an existing hash table entry with "key" and "value"
void HashTableEntryAdd(HashTable *table, void *key, void *value,
  bool deleteKey, bool deleteValue);
// Retrieve a hash table entry from "table" that has the corresponding "key"
HashTableEntry *HashTableEntryRetrieve(HashTable *table, void *key);
// Delete the hash table entry "entry" from "table"
void HashTableEntryDelete(
    HashTable *table, HashTableEntry *entry, bool deleteKey, bool deleteValue);

#endif // HASHTABLE_H
