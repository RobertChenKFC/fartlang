#include "util/bitset/bitset.h"
#include "util/hashtable/hashtable.h"
#include <assert.h>
#include <stdlib.h>

int main() {
  // 1. Test default created bitset
  int numBits = 1025;
  Bitset *bitset = BitsetNew(numBits);
  assert(bitset->n == numBits);
  int numWords = numBits / BITSET_WORD_NBITS +
                 (numBits % BITSET_WORD_NBITS == 0 ? 0 : 1);
  for (int i = 0; i < numWords; ++i)
    assert(bitset->words[i] == 0);

  // 2. Test bitset set and read
  for (int i = 0; i < numBits; i += 2)
    BitsetSet(bitset, i);
  for (int i = 0; i < numBits; ++i)
    assert(BitsetIsSet(bitset, i) == (i % 2 == 0));

  // 3. Test bitset clear and read
  for (int i = 0; i < numBits; i += 3)
    BitsetClear(bitset, i);
  for (int i = 0; i < numBits; ++i)
    assert(BitsetIsSet(bitset, i) == (i % 2 == 0 && i % 3 != 0));

  // 4. Test bitset set value
  for (int i = 0; i < numBits; ++i)
    BitsetSetVal(bitset, i, (i % 3 == 0 || i % 5 == 0) ? 1 : 0);
  for (int i = 0; i < numBits; ++i)
    assert(BitsetIsSet(bitset, i) == (i % 3 == 0 || i % 5 == 0));

  // 5. Test bitset in hash table
  HashTable *table = HashTableNew(BitsetHash, BitsetEqual, BitsetDelete, NULL);
  Bitset *bitset2 = BitsetNew(numBits);
  Bitset *bitset3 = BitsetNew(numBits);
  Bitset *bitset4 = BitsetNew(numBits + 1);
  for (int i = 0; i < numBits; ++i) {
    uint64_t val = BitsetIsSet(bitset, i) ? 1 : 0;
    BitsetSetVal(bitset2, i, val);
    BitsetSetVal(bitset3, i, 1 - val);
    BitsetSetVal(bitset4, i, val);
  }
  HashTableEntryAdd(table, bitset, NULL);
  assert(HashTableEntryRetrieve(table, bitset2));
  HashTableEntryAdd(table, bitset2, NULL);
  assert(HashTableEntryRetrieve(table, bitset3) == NULL);
  HashTableEntryAdd(table, bitset3, NULL);
  assert(HashTableEntryRetrieve(table, bitset3));
  assert(HashTableEntryRetrieve(table, bitset4) == NULL);
  HashTableEntryAdd(table, bitset4, NULL);
  assert(HashTableEntryRetrieve(table, bitset4));
  HashTableDelete(table);
}
