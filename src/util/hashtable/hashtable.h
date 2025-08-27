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

// A function type that checks if "key1" and "key2" are equal
typedef bool (*HashTableEqualFunction)(void *key1, void *key2);

// A destructor function type that can be used to delete "obj", which can be
// either a hash table key or value
typedef void (*HashTableDestructor)(void *obj);

// A hash table structure containing an array of buckets, the corresponding
// hash function and equal function to use, as a list containing all hash table
// entries
typedef struct {
  HashTableBucket *buckets;
  int capacityIdx;
  uint64_t size, capacity;
  HashTableHashFunction hash;
  HashTableEqualFunction equal;
  HashTableDestructor keyDelete;
  HashTableDestructor valDelete;
  HashTableEntry *head, *last;
} HashTable;

// Creates a new hash table that uses the hash function "hash", equal
// function "equal", destructor "keyDelete" for key deletion and destructor
// "valDelete" for value deletion; note that "keyDelete" and "valDelete"
// can be NULL, which means that the key or value won't be deleted when
// they are replaced or removed
HashTable *HashTableNew(
    HashTableHashFunction hash, HashTableEqualFunction equal,
    HashTableDestructor keyDelete, HashTableDestructor valDelete);
// Delete "table" created with HashTableNew
void HashTableDelete(HashTable *table);
// Add or replace an existing hash table entry with "key" and "value"
void HashTableEntryAdd(HashTable *table, void *key, void *value);
// Retrieve a hash table entry from "table" that has the corresponding "key"
HashTableEntry *HashTableEntryRetrieve(HashTable *table, void *key);
// Delete the hash table entry "entry" from "table"
void HashTableEntryDelete(HashTable *table, HashTableEntry *entry);
// Delete all entries in the hash "table"
void HashTableClear(HashTable *table);

// Hash and equal functions
// Uses the pointer "p" itself as the hash value
uint64_t HashTablePtrHash(void *p);
// Compares the pointers addresses "p" and "q" directly for equality
bool HashTablePtrEqual(void *p, void *q);
// Compute a rolling hash of the null-terminated ASCII string "key"
uint64_t HashTableStringHash(void *key);
// Compare two null-terminated ASCII strings for equality
bool HashTableStringEqual(void *key1, void *key2);

#endif // HASHTABLE_H
